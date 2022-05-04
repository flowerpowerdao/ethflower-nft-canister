import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Cycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Float "mo:base/Float";
import HashMap "mo:base/HashMap";
import Int "mo:base/Int";
import Int64 "mo:base/Int64";
import Int8 "mo:base/Int8";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Nat8 "mo:base/Nat8";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Random "mo:base/Random";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Time "mo:base/Time";

import Canistergeek "mo:canistergeek/canistergeek";
import Cap "mo:cap/Cap";
import Encoding "mo:encoding/Binary";
import Root "mo:cap/Root";
import Router "mo:cap/Router";
import Types "mo:cap/Types";

import AID "./toniq-labs/util/AccountIdentifier";
import ExtAllowance "./toniq-labs/ext/Allowance";
import ExtCommon "./toniq-labs/ext/Common";
import ExtCore "./toniq-labs/ext/Core";
import ExtNonFungible "./toniq-labs/ext/NonFungible";

shared ({ caller = init_minter}) actor class Canister() = this {
  
  /*********
  * TYPES *
  *********/
  type Time = Time.Time;
  type AccountIdentifier = ExtCore.AccountIdentifier;
  type SubAccount = ExtCore.SubAccount;
  type User = ExtCore.User;
  type Balance = ExtCore.Balance;
  type TokenIdentifier = ExtCore.TokenIdentifier;
  type TokenIndex  = ExtCore.TokenIndex ;
  type Extension = ExtCore.Extension;
  type CommonError = ExtCore.CommonError;
  type BalanceRequest = ExtCore.BalanceRequest;
  type BalanceResponse = ExtCore.BalanceResponse;
  type TransferRequest = ExtCore.TransferRequest;
  type TransferResponse = ExtCore.TransferResponse;
  type AllowanceRequest = ExtAllowance.AllowanceRequest;
  type ApproveRequest = ExtAllowance.ApproveRequest;
  type Metadata = ExtCommon.Metadata;
  type NotifyService = ExtCore.NotifyService;
  type MintingRequest = {
    to : AccountIdentifier;
    asset : Nat32;
  };
  
  // start custom
  // cap 
  type DetailValue = Root.DetailValue;
  type Event = Root.Event;
  type IndefiniteEvent = Root.IndefiniteEvent;
  // end custom

  //Marketplace
  type Transaction = {
    token : TokenIdentifier;
    seller : Principal;
    price : Nat64;
    buyer : AccountIdentifier;
    time : Time;
  };
  type Settlement = {
    seller : Principal;
    price : Nat64;
    subaccount : SubAccount;
    buyer : AccountIdentifier;
  };
  type Listing = {
    seller : Principal;
    price : Nat64;
    locked : ?Time;
  };
  type ListRequest = {
    token : TokenIdentifier;
    from_subaccount : ?SubAccount;
    price : ?Nat64;
  };
  type AccountBalanceArgs = { account : AccountIdentifier };
  type ICPTs = { e8s : Nat64 };
  type SendArgs = {
    memo: Nat64;
    amount: ICPTs;
    fee: ICPTs;
    from_subaccount: ?SubAccount;
    to: AccountIdentifier;
    created_at_time: ?Time;
  };
  type File = {
    ctype : Text;//"image/jpeg"
    data : [Blob];
  };
  type Asset = {
    name : Text;
    thumbnail : ?File;
    metadata: ?File;
    payload : File;
  };
  
  let LEDGER_CANISTER = actor "ryjl3-tyaaa-aaaaa-aaaba-cai" : actor { 
    account_balance_dfx : shared query AccountBalanceArgs -> async ICPTs;
    send_dfx : shared SendArgs -> async Nat64; 
  };
  
  // cap
  // start custom
  private stable var rootBucketId : ?Text = null;
  let cap = Cap.Cap(null, rootBucketId);
  let creationCycles : Nat = 1_000_000_000_000;
  // end custom
  
  private let EXTENSIONS : [Extension] = ["@ext/common", "@ext/nonfungible"];
  
  //State work
  private stable var _registryState : [(TokenIndex, AccountIdentifier)] = [];
	private stable var _tokenMetadataState : [(TokenIndex, Metadata)] = [];
  private stable var _ownersState : [(AccountIdentifier, [TokenIndex])] = [];
  
  //For marketplace
	private stable var _tokenListingState : [(TokenIndex, Listing)] = [];
	private stable var _tokenSettlementState : [(TokenIndex, Settlement)] = [];
	private stable var _paymentsState : [(Principal, [SubAccount])] = [];
	private stable var _refundsState : [(Principal, [SubAccount])] = [];
  
  private var _registry : HashMap.HashMap<TokenIndex, AccountIdentifier> = HashMap.fromIter(_registryState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
  private var _tokenMetadata : HashMap.HashMap<TokenIndex, Metadata> = HashMap.fromIter(_tokenMetadataState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
	private var _owners : HashMap.HashMap<AccountIdentifier, [TokenIndex]> = HashMap.fromIter(_ownersState.vals(), 0, AID.equal, AID.hash);
  
  //For marketplace
  private var _tokenListing : HashMap.HashMap<TokenIndex, Listing> = HashMap.fromIter(_tokenListingState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
  private var _tokenSettlement : HashMap.HashMap<TokenIndex, Settlement> = HashMap.fromIter(_tokenSettlementState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
  private var _payments : HashMap.HashMap<Principal, [SubAccount]> = HashMap.fromIter(_paymentsState.vals(), 0, Principal.equal, Principal.hash);
  private var _refunds : HashMap.HashMap<Principal, [SubAccount]> = HashMap.fromIter(_refundsState.vals(), 0, Principal.equal, Principal.hash);
  private var ESCROWDELAY : Time = 2 * 60 * 1_000_000_000;
	private stable var _usedPaymentAddressess : [(AccountIdentifier, Principal, SubAccount)] = [];
	private stable var _transactions : [Transaction] = [];
  private stable var _supply : Balance  = 0;
  private stable var _minter : Principal  = init_minter;
  private stable var _nextTokenId : TokenIndex  = 0;
	private stable var _assets : [Asset] = [];
  //_assets := [];

  //EXTv2 SALE
  private stable var _disbursementsState : [(TokenIndex, AccountIdentifier, SubAccount, Nat64)] = [];
  private stable var _nextSubAccount : Nat = 0;
  private var _disbursements : List.List<(TokenIndex, AccountIdentifier, SubAccount, Nat64)> = List.fromArray(_disbursementsState);
  private var salesFees : [(AccountIdentifier, Nat64)] = [
    ("64478e81211f72ab1ba2d6a0f87d5bb85f9e936c4d1d431fdf19e33c055d8ef8", 7500), //Royalty Fee 
    ("c7e461041c0c5800a56b64bb7cefc247abc0bbbb99bd46ff71c64e92d9f5c2f9", 1000), //Entrepot Fee 
  ];

  /****************
  * CANISTERGEEK *
  ****************/
  private let canistergeekMonitor = Canistergeek.Monitor();
  stable var _canistergeekMonitorUD: ? Canistergeek.UpgradeData = null;

  /**
  * Returns collected data based on passed parameters.
  * Called from browser.
  */
  public query ({caller}) func getCanisterMetrics(parameters: Canistergeek.GetMetricsParameters): async ?Canistergeek.CanisterMetrics {
      validateCaller(caller);
      canistergeekMonitor.getMetrics(parameters);
  };

  /**
  * Force collecting the data at current time.
  * Called from browser or any canister "update" method.
  */
  public shared ({caller}) func collectCanisterMetrics(): async () {
      validateCaller(caller);
      canistergeekMonitor.collectMetrics();
  };
  
  private func validateCaller(principal: Principal) : () {
    assert( principal == Principal.fromText("ikywv-z7xvl-xavcg-ve6kg-dbbtx-wy3gy-qbtwp-7ylai-yl4lc-lwetg-kqe")) // canistergeek principal
  };

  // start custom
  private stable var isShuffled : Bool = false;
  private stable var auctionEnded : Bool = false;
  // end custom

  //State functions
  system func preupgrade() {
    _registryState := Iter.toArray(_registry.entries());
    _tokenMetadataState := Iter.toArray(_tokenMetadata.entries());
    _ownersState := Iter.toArray(_owners.entries());
    _tokenListingState := Iter.toArray(_tokenListing.entries());
    _tokenSettlementState := Iter.toArray(_tokenSettlement.entries());
    _paymentsState := Iter.toArray(_payments.entries());
    _refundsState := Iter.toArray(_refunds.entries());
    _disbursementsState := List.toArray(_disbursements);
    _canistergeekMonitorUD := ? canistergeekMonitor.preupgrade();
  };
  system func postupgrade() {
    _registryState := [];
    _tokenMetadataState := [];
    _ownersState := [];
    _tokenListingState := [];
    _tokenSettlementState := [];
    _paymentsState := [];
    _refundsState := [];
    _disbursementsState := [];
    canistergeekMonitor.postupgrade(_canistergeekMonitorUD);
    _canistergeekMonitorUD := null;
  };
  
  // cap
  // start custom
  public shared(msg) func initCap() : async Result.Result<(), Text> {
    assert(msg.caller == _minter);
    canistergeekMonitor.collectMetrics();
    let pid = Principal.fromActor(this);
    let tokenContractId = Principal.toText(pid);

    try {
        rootBucketId := await cap.handshake(
            tokenContractId,
            creationCycles
        );

        return #ok();
    } catch e {
        throw e;
    };
  };
  // end custom

  //Listings
  //EXTv2 SALE
  func _natToSubAccount(n : Nat) : SubAccount {
    let n_byte = func(i : Nat) : Nat8 {
      assert(i < 32);
      let shift : Nat = 8 * (32 - 1 - i);
      Nat8.fromIntWrap(n / 2**shift)
    };
    Array.tabulate<Nat8>(32, n_byte)
  };

  func _getNextSubAccount() : SubAccount {
    var _saOffset = 4294967296;
    _nextSubAccount += 1;
    return _natToSubAccount(_saOffset+_nextSubAccount);
  };

  func _addDisbursement(d : (TokenIndex, AccountIdentifier, SubAccount, Nat64)) : () {
    _disbursements := List.push(d, _disbursements);
  };

  public shared(msg) func lock(tokenid : TokenIdentifier, price : Nat64, address : AccountIdentifier, _subaccountNOTUSED : SubAccount) : async Result.Result<AccountIdentifier, CommonError> {
    canistergeekMonitor.collectMetrics();
		if (ExtCore.TokenIdentifier.isPrincipal(tokenid, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(tokenid));
		};
		let token = ExtCore.TokenIdentifier.getIndex(tokenid);
    if (_isLocked(token)) {					
      return #err(#Other("Listing is locked"));				
    };
    let subaccount = _getNextSubAccount();
		switch(_tokenListing.get(token)) {
			case (?listing) {
        if (listing.price != price) {
          return #err(#Other("Price has changed!"));
        } else {
          let paymentAddress : AccountIdentifier = AID.fromPrincipal(Principal.fromActor(this), ?subaccount);
          _tokenListing.put(token, {
            seller = listing.seller;
            price = listing.price;
            locked = ?(Time.now() + ESCROWDELAY);
          });
          switch(_tokenSettlement.get(token)) {
            case(?settlement){
              let resp : Result.Result<(), CommonError> = await settle(tokenid);
              switch(resp) {
                case(#ok) {
                  return #err(#Other("Listing has sold"));
                };
                case(#err _) {
                  //Atomic protection
                  if (Option.isNull(_tokenListing.get(token))) return #err(#Other("Listing has sold"));
                };
              };
            };
            case(_){};
          };
          _tokenSettlement.put(token, {
            seller = listing.seller;
            price = listing.price;
            subaccount = subaccount;
            buyer = address;
          });
          return #ok(paymentAddress);
        };
			};
			case (_) {
				return #err(#Other("No listing!"));				
			};
		};
  };
  public shared(msg) func settle(tokenid : TokenIdentifier) : async Result.Result<(), CommonError> {
    canistergeekMonitor.collectMetrics();
		if (ExtCore.TokenIdentifier.isPrincipal(tokenid, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(tokenid));
		};
		let token = ExtCore.TokenIdentifier.getIndex(tokenid);
    switch(_tokenSettlement.get(token)) {
      case(?settlement){
        let response : ICPTs = await LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(Principal.fromActor(this), ?settlement.subaccount)});
        switch(_tokenSettlement.get(token)) {
          case(?settlement){
            if (response.e8s >= settlement.price){
              switch (_registry.get(token)) {
                case (?token_owner) {
                  var bal : Nat64 = settlement.price - (10000 * Nat64.fromNat(salesFees.size() + 1));
                  var rem = bal;
                  for(f in salesFees.vals()){
                    var _fee : Nat64 = bal * f.1 / 100000;
                    _addDisbursement((token, f.0, settlement.subaccount, _fee));
                    rem := rem -  _fee : Nat64;
                  };
                  _addDisbursement((token, token_owner, settlement.subaccount, rem));
                  let event : IndefiniteEvent = {
                    operation = "sale";
                    details = [
                      ("to", #Text(settlement.buyer)),
                      ("from", #Principal(settlement.seller)),
                      ("price_decimals", #U64(8)),
                      ("price_currency", #Text("ICP")),
                      ("price", #U64(settlement.price)),
                      ("token_id", #Text(tokenid))
                    ];
                    caller = msg.caller;
                  };
                  ignore cap.insert(event);
                  _transferTokenToUser(token, settlement.buyer);
                  _transactions := Array.append(_transactions, [{
                    token = tokenid;
                    seller = settlement.seller;
                    price = settlement.price;
                    buyer = settlement.buyer;
                    time = Time.now();
                  }]);
                  _tokenListing.delete(token);
                  _tokenSettlement.delete(token);
                  return #ok();
                };
                case (_) {
                  return #err(#InvalidToken(tokenid));
                };
              };
            } else {
              if (_isLocked(token)) {					
                return #err(#Other("Insufficient funds sent"));
              } else {
                _tokenSettlement.delete(token);
                return #err(#Other("Nothing to settle"));				
              };
            };
          };
          case(_) return #err(#Other("Nothing to settle"));
        };
      };
      case(_) return #err(#Other("Nothing to settle"));
    };
  };
  public shared(msg) func list(request: ListRequest) : async Result.Result<(), CommonError> {
    canistergeekMonitor.collectMetrics();
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
		};
    if (not auctionEnded and msg.caller != Principal.fromText("f7kev-f7vep-kclyf-u5gfo-i3oxk-w26ds-xkewz-gye6h-yuvve-iszjj-sqe")){
      return #err(#Other("Auction is not ended"));
    };
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
    if (_isLocked(token)) {					
      return #err(#Other("Listing is locked"));				
    };
    switch(_tokenSettlement.get(token)) {
      case(?settlement){
        let resp : Result.Result<(), CommonError> = await settle(request.token);
        switch(resp) {
          case(#ok) return #err(#Other("Listing as sold"));
          case(#err _) {};
        };
      };
      case(_){};
    };
    let owner = AID.fromPrincipal(msg.caller, request.from_subaccount);
    switch (_registry.get(token)) {
      case (?token_owner) {
				if(AID.equal(owner, token_owner) == false) {
					return #err(#Other("Not authorized"));
				};
        switch(request.price) {
          case(?price) {
            _tokenListing.put(token, {
              seller = msg.caller;
              price = price;
              locked = null;
            });
          };
          case(_) {
            _tokenListing.delete(token);
          };
        };
        if (Option.isSome(_tokenSettlement.get(token))) {
          _tokenSettlement.delete(token);
        };
        return #ok;
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };

  public shared(msg) func disburse() : async () {
    canistergeekMonitor.collectMetrics();
    var _cont : Bool = true;
    while(_cont){
      var last = List.pop(_disbursements);
      switch(last.0){
        case(?d) {
          _disbursements := last.1;
          try {
            var bh = await LEDGER_CANISTER.send_dfx({
              memo = Encoding.BigEndian.toNat64(Blob.toArray(Principal.toBlob(Principal.fromText(ExtCore.TokenIdentifier.fromPrincipal(Principal.fromActor(this), d.0)))));
              amount = { e8s = d.3 };
              fee = { e8s = 10000 };
              from_subaccount = ?d.2;
              to = d.1;
              created_at_time = null;
            });
          } catch (e) {
            _disbursements := List.push(d, _disbursements);
          };
        };
        case(_) {
          _cont := false;
        };
      };
    };
  };

	public shared(msg) func setMinter(minter : Principal) : async () {
		assert(msg.caller == _minter);
    canistergeekMonitor.collectMetrics();
		_minter := minter;
	};

  // start custom
  private func _prng(current: Nat8) : Nat8 {
    // a pseudo random number generator that returns Nat8
    // between 0-99
    let next : Int =  _fromNat8ToInt(current) * 1103515245 + 12345;
    return _fromIntToNat8(next) % 100;
  };

  private func _fromNat8ToInt(n : Nat8) : Int {
    Int8.toInt(Int8.fromNat8(n))
  };

  private func _fromIntToNat8(n: Int) : Nat8 {
    Int8.toNat8(Int8.fromIntWrap(n))
  };

  public shared(msg) func endAuction() {
    assert(msg.caller == _minter and auctionEnded == false);
    canistergeekMonitor.collectMetrics();
    auctionEnded := true;
  };

  public shared(msg) func shuffleAssets() :async () {
    assert(msg.caller == _minter and isShuffled == false);
    canistergeekMonitor.collectMetrics();
    // get a random seed from the IC
    let seed: Blob = await Random.blob();
    // use that seed to generate a truly random number
    var randomNumber : Nat8 = Random.byteFrom(seed);
    // get the number of available assets
    var currentIndex : Nat = _assets.size();
    // create mutable copy of _assets
    var assets = Array.thaw<Asset>(_assets);


    // shuffle the assets array using the random beacon
    while (currentIndex != 1){
      // create a pseudo random number between 0-99
      randomNumber := _prng(randomNumber);
      // use that number to calculate a random index between 0 and currentIndex
      var randomIndex : Nat = Int.abs(Float.toInt(Float.floor(Float.fromInt(_fromNat8ToInt(randomNumber)* currentIndex/100))));
      assert(randomIndex < currentIndex);
      currentIndex -= 1;
      // we never want to touch the 0 index
      // as it contains the seed video
      if (randomIndex == 0) {
        randomIndex += 1;
      };
      assert((randomIndex != 0) and (currentIndex != 0));
      let temporaryValue = assets[currentIndex];
      assets[currentIndex] := assets[randomIndex];
      assets[randomIndex] := temporaryValue;
    };

    _assets := Array.freeze(assets);
    isShuffled := true;
  };
  // end custom

	public shared(msg) func streamAsset(id : Nat, isThumb : Bool, payload : Blob) : async () {
    assert(msg.caller == _minter);
    canistergeekMonitor.collectMetrics();
    var tassets : [var Asset]  = Array.thaw<Asset>(_assets);
    var asset : Asset = tassets[id];
    if (isThumb) {
      switch(asset.thumbnail) {
        case(?t) {
          asset := {
            name = asset.name;
            thumbnail = ?{
              ctype = t.ctype;
              data = Array.append(t.data, [payload]);
            };
            payload = asset.payload;
            metadata = asset.metadata;
          };
        };
        case(_){};
      };
    } else {
      asset := {
        name = asset.name;
        thumbnail = asset.thumbnail;
        payload = {
          ctype = asset.payload.ctype;
          data = Array.append(asset.payload.data, [payload]);
        };
        metadata = asset.metadata;
      };
    };
    tassets[id] := asset;
    _assets := Array.freeze(tassets);
  };
  public shared(msg) func updateThumb(name : Text, file : File) : async ?Nat {
    assert(msg.caller == _minter);
    canistergeekMonitor.collectMetrics();
    var i : Nat = 0;
    for(a in _assets.vals()){
      if (a.name == name) {
        var tassets : [var Asset]  = Array.thaw<Asset>(_assets);
        var asset : Asset = tassets[i];
        asset := {
          name = asset.name;
          thumbnail = ?file;
          payload = asset.payload;
          metadata = asset.metadata;
        };
        tassets[i] := asset;
        _assets := Array.freeze(tassets);
        return ?i;
      };
      i += 1;
    };
    return null;
  };
  public shared(msg) func addAsset(asset : Asset) : async Nat {
    assert(msg.caller == _minter);
    canistergeekMonitor.collectMetrics();
    _assets := Array.append(_assets, [asset]);
    _assets.size() - 1;
  };
  // start custom
  func _indexToIdentifier(index: Nat32) : Text {
    let identifier = ExtCore.TokenIdentifier.fromPrincipal(Principal.fromActor(this),index);
    assert(index == ExtCore.TokenIdentifier.getIndex(identifier));
    return identifier;
  };
  // end custom
  public shared(msg) func initMint(addresses : [Text]) : async () {
		assert(msg.caller == _minter and _nextTokenId == 0);
    canistergeekMonitor.collectMetrics();
    //Mint
    for (address in Iter.fromArray(addresses)) {
      for (i in Iter.range(0, 100)) {
        _tokenMetadata.put(_nextTokenId, #nonfungible({
          // we start with asset 1, as index 0
          // contains the seed animation and is not being shuffled
          metadata = ?_nat32ToBlob(_nextTokenId+1);
        }));
        _transferTokenToUser(_nextTokenId, address);
        _supply := _supply + 1;
        _nextTokenId := _nextTokenId + 1;
      }
    }
	};
  func _nat32ToBlob(n : Nat32) : Blob {
    if (n < 256) {
      return Blob.fromArray([0,0,0, Nat8.fromNat(Nat32.toNat(n))]);
    } else if (n < 65536) {
      return Blob.fromArray([
        0,0,
        Nat8.fromNat(Nat32.toNat((n >> 8) & 0xFF)), 
        Nat8.fromNat(Nat32.toNat((n) & 0xFF))
      ]);
    } else if (n < 16777216) {
      return Blob.fromArray([
        0,
        Nat8.fromNat(Nat32.toNat((n >> 16) & 0xFF)), 
        Nat8.fromNat(Nat32.toNat((n >> 8) & 0xFF)), 
        Nat8.fromNat(Nat32.toNat((n) & 0xFF))
      ]);
    } else {
      return Blob.fromArray([
        Nat8.fromNat(Nat32.toNat((n >> 24) & 0xFF)), 
        Nat8.fromNat(Nat32.toNat((n >> 16) & 0xFF)), 
        Nat8.fromNat(Nat32.toNat((n >> 8) & 0xFF)), 
        Nat8.fromNat(Nat32.toNat((n) & 0xFF))
      ]);
    };
  };

  func _blobToNat32(b : Blob) : Nat32 {
    var index : Nat32 = 0;
    Array.foldRight<Nat8, Nat32>(Blob.toArray(b), 0, func (u8, accum) {
      index += 1;
      accum + Nat32.fromNat(Nat8.toNat(u8)) << ((index-1) * 8);
    });
  };

  public shared(msg) func transfer(request: TransferRequest) : async TransferResponse {
    canistergeekMonitor.collectMetrics();
    if (request.amount != 1) {
			return #err(#Other("Must use amount of 1"));
		};
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
		};
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
    if (Option.isSome(_tokenListing.get(token))) {
			return #err(#Other("This token is currently listed for sale!"));
    };
    let owner = ExtCore.User.toAID(request.from);
    let spender = AID.fromPrincipal(msg.caller, request.subaccount);
    let receiver = ExtCore.User.toAID(request.to);
		if (AID.equal(owner, spender) == false) {
      return #err(#Unauthorized(spender));
    };
    switch (_registry.get(token)) {
      case (?token_owner) {
				if(AID.equal(owner, token_owner) == false) {
					return #err(#Unauthorized(owner));
				};
        if (request.notify) {
          switch(ExtCore.User.toPrincipal(request.to)) {
            case (?canisterId) {
              //Do this to avoid atomicity issue
              _removeTokenFromUser(token);
              let notifier : NotifyService = actor(Principal.toText(canisterId));
              switch(await notifier.tokenTransferNotification(request.token, request.from, request.amount, request.memo)) {
                case (?balance) {
                  if (balance == 1) {
                    // start custom
                    let event : IndefiniteEvent = {
                            operation = "transfer";
                            details = [
                              ("to", #Text receiver ),
                              ("from", #Text owner),
                              ("token_id", #Text(request.token))
                            ];
                            caller = msg.caller;
                    };
                    ignore cap.insert(event);
                    // end custom
                    _transferTokenToUser(token, receiver);
                    return #ok(request.amount);
                  } else {
                    //Refund
                    _transferTokenToUser(token, owner);
                    return #err(#Rejected);
                  };
                };
                case (_) {
                  //Refund
                  _transferTokenToUser(token, owner);
                  return #err(#Rejected);
                };
              };
            };
            case (_) {
              return #err(#CannotNotify(receiver));
            }
          };
        } else {
          // start custom
          let event : IndefiniteEvent = {
                  operation = "transfer";
                  details = [
                    ("to", #Text receiver ),
                    ("from", #Text owner),
                    ("token_id", #Text(request.token))
                  ];
                  caller = msg.caller;
          };
          ignore cap.insert(event);
          // end custom
          _transferTokenToUser(token, receiver);
          return #ok(request.amount);
        };
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };
  
  public query func getMinter() : async Principal {
    _minter;
  };
  public query func extensions() : async [Extension] {
    EXTENSIONS;
  };
  public query func balance(request : BalanceRequest) : async BalanceResponse {
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
		};
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let aid = ExtCore.User.toAID(request.user);
    switch (_registry.get(token)) {
      case (?token_owner) {
				if (AID.equal(aid, token_owner) == true) {
					return #ok(1);
				} else {					
					return #ok(0);
				};
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };
	public query func bearer(token : TokenIdentifier) : async Result.Result<AccountIdentifier, CommonError> {
		if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(token));
		};
		let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_getBearer(tokenind)) {
      case (?token_owner) {
				return #ok(token_owner);
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
	};
  // start custom
  public query func supply() : async Result.Result<Balance, CommonError> {
  // end custom
    #ok(_supply);
  };
  public query func getRegistry() : async [(TokenIndex, AccountIdentifier)] {
    Iter.toArray(_registry.entries());
  };
  // start custom
  public query func getTokens() : async [(TokenIndex, Text)] {
    var resp : [(TokenIndex, Text)] = [];
    for(e in _tokenMetadata.entries()){
      let assetid = _assets[Nat32.toNat(e.0)+1].name;
      resp := Array.append(resp, [(e.0, assetid)]);
    };
    resp;
  };
  // end custom
  public query func tokens(aid : AccountIdentifier) : async Result.Result<[TokenIndex], CommonError> {
    switch(_owners.get(aid)) {
      case(?tokens) return #ok(tokens);
      case(_) return #err(#Other("No tokens"));
    };
  };
  
  public query func tokens_ext(aid : AccountIdentifier) : async Result.Result<[(TokenIndex, ?Listing, ?Blob)], CommonError> {
		switch(_owners.get(aid)) {
      case(?tokens) {
        var resp : [(TokenIndex, ?Listing, ?Blob)] = [];
        for (a in tokens.vals()){
          resp := Array.append(resp, [(a, _tokenListing.get(a), null)]);
        };
        return #ok(resp);
      };
      case(_) return #err(#Other("No tokens"));
    };
	};
  public query func metadata(token : TokenIdentifier) : async Result.Result<Metadata, CommonError> {
    if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(token));
		};
		let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_tokenMetadata.get(tokenind)) {
      case (?token_metadata) {
				return #ok(token_metadata);
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
  };
  public query func details(token : TokenIdentifier) : async Result.Result<(AccountIdentifier, ?Listing), CommonError> {
		if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(token));
		};
		let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_getBearer(tokenind)) {
      case (?token_owner) {
				return #ok((token_owner, _tokenListing.get(tokenind)));
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
	};
  
  //Listings
  public query func transactions() : async [Transaction] {
    _transactions;
  };
  public query func settlements() : async [(TokenIndex, AccountIdentifier, Nat64)] {
    //Lock to admin?
    var result : [(TokenIndex, AccountIdentifier, Nat64)] = [];
    for((token, listing) in _tokenListing.entries()) {
      if(_isLocked(token)){
        switch(_tokenSettlement.get(token)) {
          case(?settlement) {
            result := Array.append(result, [(token, AID.fromPrincipal(settlement.seller, ?settlement.subaccount), settlement.price)]);
          };
          case(_) {};
        };
      };
    };
    result;
  };
  public query(msg) func payments() : async ?[SubAccount] {
    _payments.get(msg.caller);
  };
  public query func listings() : async [(TokenIndex, Listing, Metadata)] {
    var results : [(TokenIndex, Listing, Metadata)] = [];
    for(a in _tokenListing.entries()) {
      results := Array.append(results, [(a.0, a.1, #nonfungible({ metadata = null }))]);
    };
    results;
  };
  public query(msg) func allSettlements() : async [(TokenIndex, Settlement)] {
    Iter.toArray(_tokenSettlement.entries())
  };
  public query(msg) func allPayments() : async [(Principal, [SubAccount])] {
    Iter.toArray(_payments.entries())
  };
  public shared(msg) func clearPayments(seller : Principal, payments : [SubAccount]) : async () {
    canistergeekMonitor.collectMetrics();
    var removedPayments : [SubAccount] = [];
    for (p in payments.vals()){
      let response : ICPTs = await LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(seller, ?p)});
      if (response.e8s < 10_000){
        removedPayments := Array.append(removedPayments, [p]);
      };
    };
    switch(_payments.get(seller)) {
      case(?sellerPayments) {
        var newPayments : [SubAccount] = [];
        for (p in sellerPayments.vals()){
          if (Option.isNull(Array.find(removedPayments, func(a : SubAccount) : Bool {
            Array.equal(a, p, Nat8.equal);
          }))) {
            newPayments := Array.append(newPayments, [p]);
          };
        };
        _payments.put(seller, newPayments)
      };
      case(_){};
    };
  };

  //HTTP
  type HeaderField = (Text, Text);
  type HttpResponse = {
    status_code: Nat16;
    headers: [HeaderField];
    body: Blob;
    streaming_strategy: ?HttpStreamingStrategy;
  };
  type HttpRequest = {
    method : Text;
    url : Text;
    headers : [HeaderField];
    body : Blob;
  };
  type HttpStreamingCallbackToken =  {
    content_encoding: Text;
    index: Nat;
    key: Text;
    sha256: ?Blob;
  };

  type HttpStreamingStrategy = {
    #Callback: {
        // start custom
        callback: shared () -> async ();
        // end custom
        token: HttpStreamingCallbackToken;
    };
  };

  type HttpStreamingCallbackResponse = {
    body: Blob;
    token: ?HttpStreamingCallbackToken;
  };
  let NOT_FOUND : HttpResponse = {status_code = 404; headers = []; body = Blob.fromArray([]); streaming_strategy = null};
  let BAD_REQUEST : HttpResponse = {status_code = 400; headers = []; body = Blob.fromArray([]); streaming_strategy = null};
  
  public query func http_request(request : HttpRequest) : async HttpResponse {
    let path = Iter.toArray(Text.tokens(request.url, #text("/")));
    switch(_getParam(request.url, "tokenid")) {
      case (?tokenid) {
        // start custom
        // we assume the seed animation video is stored in index 0
        // and thus uploaded first
        if (not isShuffled){
          return _processFile(Nat.toText(0), _assets[0].payload);
        };
        // end custom
        switch(_getTokenData(tokenid)) {
          case(?metadata)  {
            let assetid : Nat = Nat32.toNat(_blobToNat32(metadata));
            let asset : Asset = _assets[assetid];
            switch(_getParam(request.url, "type")) {
              case(?t) {
                // start custom
                switch(t) {
                  case("thumbnail") {
                    switch(asset.thumbnail) {
                      case(?thumb) {
                        return {
                          status_code = 200;
                          headers = [("content-type", thumb.ctype)];
                          body = thumb.data[0];
                          streaming_strategy = null;
                        };
                      };
                      case (_){};
                    };
                  };
                  case("metadata") {
                    switch(asset.metadata) {
                      case(?metadata) {
                        return {
                          status_code = 200;
                          headers = [("content-type", metadata.ctype)];
                          body = metadata.data[0];
                          streaming_strategy = null;
                        };
                      };
                      case (_){};
                    };
                  };
                  case(_){};
                };
                // end custom
              };
              case(_) {
              };
            };
            return _processFile(Nat.toText(assetid), asset.payload);
          };
          case (_){};
        };
      };
      case (_){};
    };
    switch(_getParam(request.url, "asset")) {
      case (?atext) {
        switch(_natFromText(atext)){
          case(?assetid){
            let asset : Asset = _assets[assetid];
            switch(_getParam(request.url, "type")) {
              case(?t) {
                // start custom
                switch(t) {
                  case("thumbnail") {
                    switch(asset.thumbnail) {
                      case(?thumb) {
                        return {
                          status_code = 200;
                          headers = [("content-type", thumb.ctype)];
                          body = thumb.data[0];
                        streaming_strategy = null;
                        };
                      };
                      case (_){};
                    };
                  };
                  case("metadata") {
                    switch(asset.metadata) {
                      case(?metadata) {
                        return {
                          status_code = 200;
                          headers = [("content-type", metadata.ctype)];
                          body = metadata.data[0];
                          streaming_strategy = null;
                        };
                      };
                      case (_){};
                    };
                  };
                  case(_){};
                };
                // end custom
              };
              case(_) {
              };
            };
            return _processFile(Nat.toText(assetid), asset.payload);
          };
          case (_){};
        };
      };
      case (_){};
    };

    /**********************
    * TOKEN INDEX LOOKUP *
    **********************/
    // check if theres a path
    switch (path.size()) {
      // check if there's only on "argument" to it
      case 1 {
        // try and convert it to a Nat from Text
        switch(_natFromText(path[0])) {
          // if that works, use that
          case (?tokenIndex) {
            switch (_getTokenDataFromIndex(Nat32.fromNat(tokenIndex))) {
              case (?assetIdBlob) {
                let assetid : Nat = Nat32.toNat(_blobToNat32(assetIdBlob));
                let asset : Asset = _assets[assetid];
                return _processFile(Nat.toText(assetid), asset.payload);
              };
              case (_) {};
            };
          };
          case (_) {};
        };
      };
      case (_) {};
    };
    
    //Just show index
    var soldValue : Nat = Nat64.toNat(Array.foldLeft<Transaction, Nat64>(_transactions, 0, func (b : Nat64, a : Transaction) : Nat64 { b + a.price }));
    var avg : Nat = if (_transactions.size() > 0) {
      soldValue/_transactions.size();
    } else {
      0;
    };
    return {
      status_code = 200;
      headers = [("content-type", "text/plain")];
      body = Text.encodeUtf8 (
        "BTC Flower \n" #
        "---\n" #
        "Cycle Balance:                            ~" # debug_show (Cycles.balance()/1000000000000) # "T\n" #
        "Minted NFTs:                              " # debug_show (_nextTokenId) # "\n" #
        "Marketplace Listings:                     " # debug_show (_tokenListing.size()) # "\n" #
        "Sold via Marketplace:                     " # debug_show (_transactions.size()) # "\n" #
        "Sold via Marketplace in ICP:              " # _displayICP(soldValue) # "\n" #
        "Average Price ICP Via Marketplace:        " # _displayICP(avg) # "\n" #
        "Admin:                                    " # debug_show (_minter) # "\n"
      );
      streaming_strategy = null;
    };
  };
  public query func http_request_streaming_callback(token : HttpStreamingCallbackToken) : async HttpStreamingCallbackResponse {
    switch(_natFromText(token.key)) {
      case null return {body = Blob.fromArray([]); token = null};
      case (?assetid) {
        let asset : Asset = _assets[assetid];
        let res = _streamContent(token.key, token.index, asset.payload.data);
        return {
          body = res.0;
          token = res.1;
        };
      };
    };
  };
  // Attempt to parse char to digit.
  private func digitFromChar(c: Char): ?Nat {
      switch(c) {
          case '0' ?0;
          case '1' ?1;
          case '2' ?2;
          case '3' ?3;
          case '4' ?4;
          case '5' ?5;
          case '6' ?6;
          case '7' ?7;
          case '8' ?8;
          case '9' ?9;
          case _ null;
      }
  };
  // Attempts to parse a nat from a path string.
  private func _natFromText(
      text : Text
  ) : ?Nat {
      var exponent : Nat = text.size();
      var number : Nat = 0;
      for (char in text.chars()){
          switch (digitFromChar(char)) {
              case (?digit) {
                  exponent -= 1;
                  number += digit * (10**exponent);
              };
              case (_) {
                  return null
              }
          }
      };
      ?number
  };
  private func _processFile(tokenid : TokenIdentifier, file : File) : HttpResponse {
    // start custom
    let self: Principal = Principal.fromActor(this);
    let canisterId: Text = Principal.toText(self);
    let canister = actor (canisterId) : actor { http_request_streaming_callback : shared () -> async () };
    // end custom

    if (file.data.size() > 1 ) {
      let (payload, token) = _streamContent(tokenid, 0, file.data);
      return {
        // start custom
        status_code = 200;
        headers = [
          ("Content-Type", file.ctype), 
          ("Cache-Control", "public, max-age=15552000"),
          ("Access-Control-Expose-Headers","Content-Length, Content-Range"),
          ("Access-Control-Allow-Methods", "GET, POST, HEAD, OPTIONS"),
          ("Access-Control-Allow-Origin", "*"),
          ("Content-Length","505258"),
          ("Accept-Ranges","bytes"),
        ];
        // end custom
        body = payload;
        streaming_strategy = ?#Callback({
          token = Option.unwrap(token);
          callback = canister.http_request_streaming_callback;
        });
      };
    } else {
      return {
        status_code = 200;
        headers = [("content-type", file.ctype), ("cache-control", "public, max-age=15552000")];
        body = file.data[0];
        streaming_strategy = null;
      };
    };
  };

  private func _getTokenDataFromIndex(tokenind: Nat32) : ?Blob {
    switch (_tokenMetadata.get(tokenind)) {
      case (?token_metadata) {
        switch(token_metadata) {
          case (#fungible data) return null;
          case (#nonfungible data) return data.metadata;
        };
      };
      case (_) {
        return null;
      };
    };
    return null;
  };
  
  private func _getTokenData(token : Text) : ?Blob {
    if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
      return null;
    };
    let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_tokenMetadata.get(tokenind)) {
      case (?token_metadata) {
        switch(token_metadata) {
          case (#fungible data) return null;
          case (#nonfungible data) return data.metadata;
        };
      };
      case (_) {
        return null;
      };
    };
    return null;
  };
  private func _getParam(url : Text, param : Text) : ?Text {
    var _s : Text = url;
    Iter.iterate<Text>(Text.split(_s, #text("/")), func(x, _i) {
      _s := x;
    });
    Iter.iterate<Text>(Text.split(_s, #text("?")), func(x, _i) {
      if (_i == 1) _s := x;
    });
    var t : ?Text = null;
    var found : Bool = false;
    Iter.iterate<Text>(Text.split(_s, #text("&")), func(x, _i) {
      if (found == false) {
        Iter.iterate<Text>(Text.split(x, #text("=")), func(y, _ii) {
          if (_ii == 0) {
            if (Text.equal(y, param)) found := true;
          } else if (found == true) t := ?y;
        });
      };
    });
    return t;
  };
  private func _streamContent(id : Text, idx : Nat, data : [Blob]) : (Blob, ?HttpStreamingCallbackToken) {
    let payload = data[idx];
    let size = data.size();

    if (idx + 1 == size) {
        return (payload, null);
    };

    return (payload, ?{
        content_encoding = "gzip";
        index = idx + 1;
        sha256 = null;
        key = id;
    });
  };
    
  //Internal cycle management - good general case
  public func acceptCycles() : async () {
    canistergeekMonitor.collectMetrics();
    let available = Cycles.available();
    let accepted = Cycles.accept(available);
    assert (accepted == available);
  };
  public query func availableCycles() : async Nat {
    return Cycles.balance();
  };
  
  //Private
  func _removeTokenFromUser(tindex : TokenIndex) : () {
    let owner : ?AccountIdentifier = _getBearer(tindex);
    _registry.delete(tindex);
    switch(owner){
      case (?o) _removeFromUserTokens(tindex, o);
      case (_) {};
    };
  };
  func _transferTokenToUser(tindex : TokenIndex, receiver : AccountIdentifier) : () {
    let owner : ?AccountIdentifier = _getBearer(tindex); // who owns the token (no one if mint)
    _registry.put(tindex, receiver); // transfer the token to the new owner
    switch(owner){
      case (?o) _removeFromUserTokens(tindex, o);
      case (_) {};
    };
    _addToUserTokens(tindex, receiver);
  };
  func _removeFromUserTokens(tindex : TokenIndex, owner : AccountIdentifier) : () {
    switch(_owners.get(owner)) {
      case(?ownersTokens) _owners.put(owner, Array.filter(ownersTokens, func (a : TokenIndex) : Bool { (a != tindex) }));
      case(_) ();
    };
  };
  func _addToUserTokens(tindex : TokenIndex, receiver : AccountIdentifier) : () {
    let ownersTokensNew : [TokenIndex] = switch(_owners.get(receiver)) {
      case(?ownersTokens) Array.append(ownersTokens, [tindex]);
      case(_) [tindex];
    };
    _owners.put(receiver, ownersTokensNew);
  };
  func _getBearer(tindex : TokenIndex) : ?AccountIdentifier {
    _registry.get(tindex);
  };
  func _isLocked(token : TokenIndex) : Bool {
    switch(_tokenListing.get(token)) {
      case(?listing){
        switch(listing.locked) {
          case(?time) {
            if (time > Time.now()) {
              return true;
            } else {					
              return false;
            }
          };
          case(_) {
            return false;
          };
        };
      };
      case(_) return false;
		};
	};
  func _displayICP(amt : Nat) : Text {
    debug_show(amt/100000000) # "." # debug_show ((amt%100000000)/1000000) # " ICP";
  };
  public query func stats() : async (Nat64, Nat64, Nat64, Nat64, Nat, Nat, Nat) {
    var res : (Nat64, Nat64, Nat64) = Array.foldLeft<Transaction, (Nat64, Nat64, Nat64)>(_transactions, (0,0,0), func (b : (Nat64, Nat64, Nat64), a : Transaction) : (Nat64, Nat64, Nat64) {
      var total : Nat64 = b.0 + a.price;
      var high : Nat64 = b.1;
      var low : Nat64 = b.2;
      if (high == 0 or a.price > high) high := a.price; 
      if (low == 0 or a.price < low) low := a.price; 
      (total, high, low);
    });
    var floor : Nat64 = 0;
    for (a in _tokenListing.entries()){
      if (floor == 0 or a.1.price < floor) floor := a.1.price;
    };
    (res.0, res.1, res.2, floor, _tokenListing.size(), _registry.size(), _transactions.size());
  };
  
}