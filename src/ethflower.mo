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
  public shared(msg) func initMint() : async () {
		assert(msg.caller == _minter and _nextTokenId == 0);
    canistergeekMonitor.collectMetrics();
    //Mint
    while(_nextTokenId < 2015) {
      _tokenMetadata.put(_nextTokenId, #nonfungible({
        // we start with asset 1, as index 0
        // contains the seed animation and is not being shuffled
        metadata = ?_nat32ToBlob(_nextTokenId+1);
      }));
      _transferTokenToUser(_nextTokenId, "0000");
      _supply := _supply + 1;
      _nextTokenId := _nextTokenId + 1;
    };
    
    //Airdrop
    let airdrop : [(AccountIdentifier, TokenIndex)] = [("9dd5c70ada66e593cc5739c3177dc7a40530974f270607d142fc72fce91b1d25", 2009), ("9dd5c70ada66e593cc5739c3177dc7a40530974f270607d142fc72fce91b1d25", 2010),("9dd5c70ada66e593cc5739c3177dc7a40530974f270607d142fc72fce91b1d25", 2011),("9dd5c70ada66e593cc5739c3177dc7a40530974f270607d142fc72fce91b1d25", 2012),("9dd5c70ada66e593cc5739c3177dc7a40530974f270607d142fc72fce91b1d25", 2013),("9dd5c70ada66e593cc5739c3177dc7a40530974f270607d142fc72fce91b1d25", 2014), ("d989e0edbaaace9e9f5d7b6f1eeaeb3243a0944cd2446bbf2c64434eb7a215b7", 307),("a555c4f1e300d5b8b8275c29ff9a8e06f746ed736edef52ecf2debf65f065e36", 1183),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 288),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 31),("904826bd329d037d3fcf38e7eb54295cca9f5c68b174c3c750ea5205254ff6bf", 715),("81537cf105cf802521bca09a93eb555a685f8f1802f1d794a427983e3e4203a3", 67),("238b3f9aa95a8564b8de709a2b3f7f90d07f90e1c0d6ff9342c56e75a5f88c9c", 1992),("aeffae3744a37e03236d26b2eff3f1a0aeeff778937fbc1745062d05b09e863a", 771),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 1103),("94e02368944dedb539dbde90baaacbb50c0dc19e95ed00e6705f8e9781086c85", 1879),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1211),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 581),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1173),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1548),("480ae8b83fde310e8468e536b6c07a60ea8009ef541a03ac3df79c6c1ee99394", 1636),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 1055),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 1593),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 1803),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 1697),("beb1ca33f1a660c15af29ff1414f60d30eff54cb6c2b9d78635853b872f7d1a0", 1178),("81d70d3299ec8b559871a443dad19646c7b41a7cfe88ac0f336e710fb708dd01", 567),("767748e59b5dd9bafe543932ad3a7b49731992f93e97fdd376c6820836ee6bcb", 514),("0a74c5bd278d5912c15d8e090206c6d2c263f3f5bf9a19912150b0f96ba22805", 209),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 297),("ee5154288dcb91d9c879d267d034c68139bc8e10a3201a1e46060df015da6ec1", 912),("e4b405c802fd40be17f7ae41d625bb4d1fe5e030ee45430c79c08e3a345ea325", 182),("4db213a4c511a635235da06a60bd13d20ca1206dfe5ba7ee1c147cff0acae43a", 1080),("f771bff3e411c13f5fad663330f0f6ca63e72f945fd879d74fb860c84835a00c", 544),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 525),("ab8f4f9e1c720e220693d6a60274fbc50858b53a33b85014821a7c810e24fad4", 1946),("ee5154288dcb91d9c879d267d034c68139bc8e10a3201a1e46060df015da6ec1", 1012),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 651),("e378e7b78a62d4f27a8ae1da4d90ff7b2b68b296da4cdd0e84747e98d56b36be", 1298),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1306),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1237),("62c4051e0f73d790a599882da383765968a986aefb9c88a1a754f6f252e0ad2e", 95),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 223),("23c2954703a2e6843ef085f8d529db94e0c9d6ed0361e01308b83e9828346baa", 1846),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 676),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 1502),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1504),("08a8f7e278b0e210a70c35be3254c3ca4d1cf2e983974ba33d7f61fb84277c50", 165),("e3c9ffd0d542f2cd6cbc91271884f41c9e4633fc50edd5714c153a0e0053c8da", 1136),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 1914),("f07142c3caa2c9529ef7fc2c58bdedc800e894579dbfc4b38af2878940463375", 1379),("93f39d1a5df51e3696b742f6e18c3db037b946b06142db9a4194d5d07ed3bee3", 291),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 722),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1871),("7b2d4389bc503021aed81af28668781b13f24872a80dfb8c2b9ca3d74f31e3b8", 331),("ba269dc70371d4cc156a316204bbebd7cd0b6238bd08ce1ab4fad39852b33429", 341),("b97a3ffff2662f52aafff4908805941b265afd8b9cfb1fdecee8101483269e32", 1577),("ee5154288dcb91d9c879d267d034c68139bc8e10a3201a1e46060df015da6ec1", 1509),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1919),("f2fa999f5d93ec078b793667ddf5f28144120004aae485bf57df9164d725b7dc", 758),("505595ec0ea410c11e19f826a8d2e09f7972c28eb1cc308e88bff8afe827d780", 1670),("a27ab2369b88ac1fe0fdaca0ed8b32e74659ffa3677ddaecdcbac893922631b0", 1545),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 328),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 290),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 1256),("3ab32762925164debd5dc2ef9d751c6932339870d9db0c1b0b55b9709ce9c624", 718),("df531e38324b8aed256e529eff465fdba43058c29500bb93cced66daa7db3e7c", 241),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 349),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 395),("8cec62bef352d3d6d2ab86c0c1fdbd52ad86525f3386d5252f70cc6c7751a80c", 438),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 1505),("c7866df1f42427ac865f01a7f05647f93174e78ef303f07d2812e35858a6846b", 1054),("96722c5326c0d8d06779f6100bdf5e7ee8c568eb801a7454a984d36b43e35248", 317),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 1120),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 1474),("4e8795165822cfe96b288bd2695fee6982f8bddc66adf6ade76d7f826d00a8da", 305),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1748),("3a1ee5d046ef68b89e4e39a6f11c67faa4f62a60e0fa01a05348f86c36683635", 1604),("d05f71ef66c5eb904945f5a1de31ac791e2a2f85cf31121401ce509740bcbd32", 1637),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 127),("901ce01bcc4652d177d718b57b128f8b0afd2962e8b74914e1e1ffbac998dc1c", 910),("15f742473c793cd219bfe3291dcc3cc3d6050084ce7f101b523c191cebf22bbf", 671),("78cc47c96b4c2d318002f342fb0658ba4e4183294c0d7f7c3e6489c87d43f064", 1410),("8cb5df48a9acf983571b87832d5e4533fe769bc7ec0b4f7c0dc5c90d333b0142", 1367),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 1816),("982f9770d339e21131238391a1489d7ab5ac582cbc40255296b9c851a64ca993", 502),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 237),("8d5a29bb5fef3db96a896dbe0e77790d627c2df094facb67460ca0d8d67ae6e8", 1264),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 444),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 902),("0aa3f7c95eafd19ee4dd8b2dc2f9438f0f9316beb899063928e5d41aaaac0199", 1350),("4f9498d627abaa48fc5e313463655fdd77aa8acd2eceeb43b040bcdaf22f484d", 1563),("c590639f29b29cc306c113ab3dadd41227a426f8091e151af52f9b2684881439", 860),("30d08d681dec813f78f736f3e9d1b5894d8fb1009d88ab6ac4af0cafa2df95f9", 978),("b5c49136ac33abc56c431d4a9b003d77def2838f81a45b158b43b23c09bfa703", 2007),("205f80780efc816a6aa8b7952f8a6f8f58a8115c8f814f7a4a7b39236e0c1fd0", 1829),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 407),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 404),("15995f7756130dea106057830620882969ef89efc6b8c556a05f320ada03b8be", 819),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 246),("5aeb337e48754fc8bc26b9ea29e6092c36398a44047133e3871812f5886e9ff6", 666),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 263),("1e41f5d0f2c036ec9deb5f9698e673a61771f6ba0be7dadf1d878bd49381d565", 1616),("9ed0bb6a80207a4a8d9dcc723ac311de5e4ff0dda8f0487afa5b9a0affb0ebb4", 1675),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1679),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 1643),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 1707),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 304),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1217),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 1408),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1061),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 1708),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 1682),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 1888),("03849f6af2b01270c771c070484324631690150f9afb22485bfe8489051ff3b9", 1085),("610b7e988b7b991a0a4fe35ea62e02c5c0bbe40d7f32f48b430a64ec7d2e634f", 1045),("47c8d3d732147faeb8261e1aa2c7b303c2003b1d497aac25d3d0be7724087d9f", 1257),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 737),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1762),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 416),("a5a9dce51088dc4584342667844fb29cd120020274a95f3329026693375a78b0", 210),("6f7b1ed97f29c19850f2798dbabd1a2bd3eefce14fc95e4c9c125f018e72191d", 1349),("c7975e503f9e748ee54565dcc24d4f1170f7b7ef573cd34b22c4f9f7e33e47f1", 1291),("d13451b20169fc02bd202693337148e185470c794b5763e749a233b6f4f89732", 877),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 356),("9b4b32fe9d4fb7724e1780e2050a5fef0454a9eb56d67a7079be931f3b2560da", 1742),("b54f34bf38605f66cf625c5544df702d8d52038072edd344c9c13284727d29ef", 360),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 777),("ab66b0e22fb2e07e5c5101c32b773f7700eb3cef6f9ede03323d22537c9c6614", 190),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 1246),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 390),("d24e01491766b6fe9ff78860af3642df3e80748a0db6fcd843d1022dce274fc2", 1629),("9fa7757ba5f4bac3a4ce641952fe0a766bae340701c2ebe96f2ab579835e3511", 1316),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 1869),("4a9f5a4edc2cc10d2680e70e9d9c574bbc35a2c70fb7c1a2f7f0b2aab2dadf47", 1521),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 1805),("2ce68c9a533593c49c64c89507390e7e2e6f60dfcb4f2a199b41b346456d0402", 82),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 818),("7d5f714db0e46d94e55a56867a036e6d1ca937cba8b3dcfd5066993bf3a5d2b2", 741),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 828),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 979),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1596),("ef73650d04258cd780c3bcfa892ac72b2184fd2badf7c0c6a0046e9ecaae5ae7", 527),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 881),("a5a9dce51088dc4584342667844fb29cd120020274a95f3329026693375a78b0", 831),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 678),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 366),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1281),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 1097),("b24e6fe6c7e677ac697a28cb782cf4c018de46528b4bdbfc93b718c38b4fa255", 540),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 1163),("27287174ddc4c05c547a2b59ec0fc8033c45908d768c76800f0cfdb911d9cfa5", 914),("6e4c2073ffa7d3555eb794376f03056f5a14ce347c211d5592b2285c5066b334", 1704),("82a4edbd08a57f7b5e6b1c63f0430dc559bb7808b28087751fecb006adf8e7f1", 853),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 462),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 226),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 1395),("e5c8dc40979a1c9f8ccfee1cafeca464e12bae9302d39cc62fafdb0def17ce58", 1678),("79026e209b24167118832ae13db00a5ca82634ab80dcffc0c3ec53eb957a01de", 1929),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 878),("1939ae0d38e14a78290c822c2d98553fae16fff58fbc8dc48c399d7ae169a817", 1927),("3a1ee5d046ef68b89e4e39a6f11c67faa4f62a60e0fa01a05348f86c36683635", 1351),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 1010),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 662),("f859a28b9a145aa220a663a3651781b11e6cb0eb91dea4102898a691797d6e02", 880),("15995f7756130dea106057830620882969ef89efc6b8c556a05f320ada03b8be", 72),("c87ea6f9aaa2dce9e2ac569c5a584e7cd2fdcf84dd6f2781fda99b38d5309d5a", 939),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1933),("93cf58f89a5660c7e3be7ed3b550c6ee3d7830d274f44d2ba88d90db8d006698", 1585),("1a1ec2c7fa352c4498abc335b8fd92fdccb1b5950cde740eacfc257fb07713f1", 140),("57b4c37aad1fbb7bd225eaf1eea909ae51dec260030086512576de7f9d21eac4", 1984),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 605),("981dd287f8db86e7d066155007ba391781af9592bbf03e506e5255fc8bacb263", 1082),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 953),("e1d3e659d9c9174757db14802bb27bee70b44a3964ae4d490e167fa78b4aaef9", 756),("961ed8301afaf93ee4d8a95808eff66e1f28a4821317e3287bbcfbc963d95f23", 1954),("2819b5f2d30bf0750b9a1a0030ad5fa94900b9e638a0f6ac2bce83ceac3da362", 785),("9f1f541c56bad7f5f2cdadd61c14a54efcd65f6254c17c30736c0ffd39bae2f7", 1500),("3accb10c9d6208f59987e99b96f5b8e531a8c168bf70baf8ba967e6a3291fbcc", 720),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 781),("a50b963449a6e8ed4473912013c0dadf08eec9cd80381286fb5323d349292f3e", 1296),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 312),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 168),("55e4bc6f6c03d3b2518f992e11f1e879ee4d858507d407cd2ca3fb269cdd652d", 301),("72d6be118bb00b82a29a4ab59bf3157191ac3e0226f29bf95fc9bed92789f747", 281),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 850),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 84),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 922),("0ea1ff7b51a93974a55af5981e4e76b9885885fdad73b689a23ac19e048d8166", 455),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 1028),("c0b23cf318f56fedcec61acf0d5a620259131742ac81030581947b42e0b4ed66", 1900),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 57),("eae28ad374cddea17bfb7c819fe67051b4ccf6b7face8ca2243f683a1489f1a4", 1562),("56ec9055de22539161ec32c512b573011ca5fb0d7e998fd1e6a133e976d47412", 897),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1651),("dbb29a9d73a8426b38b954d317a9f5b72bc41406b2efe609e458a0e97fadea20", 1698),("f2e23e8c97ed09b78b250e0be88e6664fdf98036a04eb52108eea11611ee36a8", 78),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 1519),("4ae7f96e7df3009d9356531ddca3a5c9bd4d2d55fce67c9c8c3215b8b1921424", 885),("6d139f4193f3c17d654afde57869ba8e53ee4e70141e3d0cd3f1bfe80c279ce3", 1926),("7c45eb9fffaf44589ac7d2edebac87f340723d035d20ecfdf4fc931a20676103", 1445),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 941),("edefd3cdd60e5319dbbb36bb0a699db07dbda655afa2c51faeed3c2dd9eab251", 1872),("3de6767744ee1fbc5ec0a6fecd8fef2becbf536d92861422cf78976d82b3dea8", 596),("d57e0e4225ce7c3c79288f85ac8f7c741ddeb98421bfccc0e18c1a15b1e0c7e1", 641),("5844cee93e3ba6776edefe080483a62de41f9805dd8af441f9b42ead28169810", 1419),("238b3f9aa95a8564b8de709a2b3f7f90d07f90e1c0d6ff9342c56e75a5f88c9c", 1091),("d13451b20169fc02bd202693337148e185470c794b5763e749a233b6f4f89732", 1052),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1223),("4f0a40f1e24fdac90cfa87d7a0a3427aea78dc0cd6b046ba3497689343a2852b", 49),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 629),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 1407),("7417e67fadf786b86dc99a02f798ea46ba90d6bcef0076a8f85d2734795526a9", 441),("dd2a477e400ef58677b39eddd1bfa7ff784af6b6f419b4b9bc72e948b1e89726", 1873),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 1589),("7a10a4d3da8081f7234845ed153cdb477e10bd6294cd74bc75b04399c123d99e", 1958),("0b927d67d8316ace58aed9e8d7567d4acd4b810cb54aa02ec3a309f9b0417b24", 115),("16e876522f46df4e847e4298d008a531739860edb29523068a78f0bac32297db", 628),("9dac022d56d6759155f8a613488d400fbbb93bb9a4920cdd943b1214ec5dd116", 958),("ed184ecf381cf3c5ab1c0a4b3ac258e01b3fd342e768904cb1c6e2783dc63f92", 746),("9ed0bb6a80207a4a8d9dcc723ac311de5e4ff0dda8f0487afa5b9a0affb0ebb4", 147),("fb91a563b63749cff361283880365e3af257af899f6ee1d308713c64f8b1c547", 625),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1032),("9deb2dd30da78dbb0f695a5d6bd4e2d65be806292839df8ac0b171d4e0e79000", 208),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 1499),("f771bff3e411c13f5fad663330f0f6ca63e72f945fd879d74fb860c84835a00c", 1844),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 546),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 485),("c43278d9070d0b78377a05ec89f240931fca99df4f3a81441b3f1638e9d858a3", 947),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1118),("36870df541416c5b94a0eb75f6bd989fd5b7031b5bc4bf2843202238d04b53bb", 1834),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 184),("b97a3ffff2662f52aafff4908805941b265afd8b9cfb1fdecee8101483269e32", 591),("cb49863bcfd936a5236075670b2bc25c9afd20be93d1019d5fca4eb5c23a8532", 1381),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 896),("d13451b20169fc02bd202693337148e185470c794b5763e749a233b6f4f89732", 815),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1213),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 1447),("527f4857f88275bfd38cf471c51f3506e68e9fc173e9223407ec250a99c80cde", 1961),("ce6f84f5d7523a339a1e25e2b571584690c94780f41c9e6e0040915ab0c55509", 887),("1e0bdfe2670a655efee77c4952ca04232a751c51f2aba811402ee6aff69a70ea", 1322),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 1995),("758aa79dcc0b9fa3d893624bfe7e184b6964fe3a4c6b09d18c09126f5d552e49", 1910),("d83bbb01c80d1a387ea099fcb0468b3ab46c0777723f3bcfa27350f301994274", 1245),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 844),("553846454c5ac96ba3966d446d1c2eaf440a59e19dca4d08218e42cb0087a1f3", 688),("43f720ca6960ed7af55c1a598bb032e227aedc9a83cc46d47bcdb0f0da0a0583", 1674),("b2417afd245f4fe1c38601b288df0167918170e6156b0416422a57122b60532c", 909),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 743),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 1808),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 874),("46a903671c0959f6d72e3f049100f81058cc044dc5e041a1523eb57ba9f01553", 906),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 1225),("64a8a7e504faa935dfba2c9f07fca7512ac8965f8416b96a90cc6a20d7bfef93", 1860),("eb5c6187a75d3728d3dbdf630a4874b35cc34cf2f27aee0604ef990863b38066", 1769),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 658),("12e08144520424e03bb6866c6221643818c79a4417f76aa7142898938ae4f3cf", 1229),("a63692635af741ae8ddda013ac8ad1fc3835a45ddf2b2a60fa6350492f573886", 901),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 155),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 482),("6f7b1ed97f29c19850f2798dbabd1a2bd3eefce14fc95e4c9c125f018e72191d", 316),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 394),("383fa3ce77ba5db2169327f1c561fc85fbee49758c626cf0ce283516c8f3a1ea", 1564),("6e4c2073ffa7d3555eb794376f03056f5a14ce347c211d5592b2285c5066b334", 1825),("ccd3c64d6b1341488e71de5a03c83b31e56800c675fb05d6a4d1fcac73d0eef2", 1940),("b89f65dbd48b614d9073eff5243d135336dcd86d8172e50ed266fb0a7fab0121", 774),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1459),("08cbdeccfc2e6207d732b7fbff3d7915981d445ebfb3346833f0a1e6a672738b", 266),("1bdee506864b3b33f40af8e2082bd0be51015aafa0d539a58ca201d1da7d2dbf", 202),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1935),("70954a4b30dddf56ba4abbfa56dee64f4ea86d25155c4aa0689a544668b83c2a", 1174),("de4b46fbe07eaae16381c22146324a863e5cbcd0f6f718290bf6fc321edaf002", 178),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 968),("01f06be72c7f213776fc138630f918a4d1a2bd8bc0dae34b0a8e3163109bbfac", 1542),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 422),("9ed0bb6a80207a4a8d9dcc723ac311de5e4ff0dda8f0487afa5b9a0affb0ebb4", 553),("c0b23cf318f56fedcec61acf0d5a620259131742ac81030581947b42e0b4ed66", 1824),("206ebc76eb9299a704eede055b15d32b74ee2ed41341d75961c4367888e7973a", 1852),("3d3f12959e3a0af0a4bc12c23691742ed65006e9cb132979d2a1f8f1523ae08d", 1268),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 406),("7131ae3ba595c77b2ffe1c19ee03df39d0dec387bafc22415c53cad2394080ba", 1638),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 143),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 917),("15cdc43e42edc01302c58ab33d20be8b29d68607b3fa7399bc02e2cf1aa894cf", 1462),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 1632),("a95c91172820e274c9914ec69a0980487ed608c365f5cbb812449039d20826e2", 991),("553846454c5ac96ba3966d446d1c2eaf440a59e19dca4d08218e42cb0087a1f3", 85),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1267),("206ebc76eb9299a704eede055b15d32b74ee2ed41341d75961c4367888e7973a", 383),("968baa7563566af7b9ddea3ace38303e8ef7d12b3f3058940d9704700d6dc47e", 1667),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 398),("0b927d67d8316ace58aed9e8d7567d4acd4b810cb54aa02ec3a309f9b0417b24", 187),("6d81bb02f36adc7d71aab1d5773a2f74cdfb2ad2b685490a94ac620f807f4557", 755),("a28059d3ec57a549ab6e21117ca4e9ce8d80f5e5332c76940367aca3f7e95061", 283),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 2002),("5413567d5af78eb1dc0da87d0be68d156e6652de700d2661a8d41268010ff6d2", 868),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 452),("3accb10c9d6208f59987e99b96f5b8e531a8c168bf70baf8ba967e6a3291fbcc", 579),("46495845866754672a28bf3f1c272e145b04b5920fce13bd18868876313e1605", 271),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1868),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 694),("d04e5ad88518457ba027d9a46ff4bdf0785fc0fc2a747fb523cd3c1ac9847576", 401),("58af3c77d9c5fa1f8d62db69a3a50392e5647925a7e04fdeaab4b5240bdfc66f", 124),("d57e0e4225ce7c3c79288f85ac8f7c741ddeb98421bfccc0e18c1a15b1e0c7e1", 1394),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1130),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 639),("dbb29a9d73a8426b38b954d317a9f5b72bc41406b2efe609e458a0e97fadea20", 1644),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1952),("0ea1ff7b51a93974a55af5981e4e76b9885885fdad73b689a23ac19e048d8166", 1327),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 228),("5e4b483e6293f98fc08bacde48e3b7e1bfa55b95861de2498d202f944bdf9b32", 1897),("dbb29a9d73a8426b38b954d317a9f5b72bc41406b2efe609e458a0e97fadea20", 1885),("5ed361d45b385cfc0051d0b23f90f9dbbaf31bbf477fd680c50291cf660fdf54", 112),("d7ebf7ae714405b4990b90b8d6a07cf97b966f331a5cdbc436c3c2ec35d21d26", 1106),("60a58226cc9e1425e41d6f68ed9f4695c0dcb20a4087240220e41bac111ad696", 298),("9cb0db639d8bccb930814df5e31f0a7bbd10b95d57d7311589bf9ba3cadabeb8", 1770),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 1775),("d7cd72b73dfa0824fee80c23dd8b62a4991fb1c92e1e2bd50ab05443743613a6", 1725),("b54f34bf38605f66cf625c5544df702d8d52038072edd344c9c13284727d29ef", 1909),("89608f311b29030f940ca52e77c170b1c68b7abf2d62a319e2beb241307470e4", 131),("c7dbe768206eda376beaf98af5b32f6ad88e1395ddbb568c0be2f28cc9a8af27", 907),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 324),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 704),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1479),("81537cf105cf802521bca09a93eb555a685f8f1802f1d794a427983e3e4203a3", 1414),("5cbc75b2be1d3ba33df4380f615b7c818222c086ff2cb8358142e4e6afa62972", 1672),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 1398),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 238),("ebdb2741d815cc87b0f5fd71198b44705a0f740fa9d868b89c8c28b002d7e9c5", 1864),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1240),("89608f311b29030f940ca52e77c170b1c68b7abf2d62a319e2beb241307470e4", 1454),("c7975e503f9e748ee54565dcc24d4f1170f7b7ef573cd34b22c4f9f7e33e47f1", 1584),("348973c34e4f3e4b0a525c91dec8f78a35040c6bfa0ee4d47097069741cb4d1e", 273),("34906d7dcad4c799ef97f8e39a4e9f314b724fa16aeed36972198721824d22ee", 1787),("4743240db87fee6fe5fe0a4e26ae7b434d3e469304b4e3dfea1abc9a6a6c25de", 613),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 1450),("85de60661b4ebffad9ab6db5c0da516291d0d80a402bbbcd1042a041bd96465d", 657),("b6a241f56ad261a4a3a4af2cd28e9d9942fd0dde50ed09ec4abae76fe098addb", 1740),("65c5f34a3a20d4934272ac64fc48f7791224224c7c0b9166f41b943be8697ad9", 1113),("ed817a1e117bb940e559511add3614958888fe5bf96a7cd5dd9ed2bcc53ea242", 1353),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 116),("0450d96e77ef4cc20d7885977127ae26bc66404077780f1e03f0b6dbc57199c0", 723),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 26),("0d68a0da0a6474a0e5f1b3cf2c8e2e357dbbfa1ceadfd65a03c0b7cb54ca4028", 1695),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1164),("904826bd329d037d3fcf38e7eb54295cca9f5c68b174c3c750ea5205254ff6bf", 1435),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 524),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 765),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 265),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 1737),("66b5ebd2ac3e237480eef309399c3ba37cbc8c13f207a87a337d9b6d6165a956", 1304),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 870),("38d0c913eed3c4b2ae366b6f4a2e6e9117b2fd7fc4ce017acd636469b13f5a4e", 1013),("acd50fe26a641d46757ae1a11fbeeb9a7c52bd40f388b060ed46d199d9d8db24", 517),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 1132),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1772),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1059),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 570),("a189720c645710834526ff32e988cf5339e8a936ab29fac7a093faa80e9610d4", 330),("857a225fbdc9bd9a193be0026abd820d2403353d7f59c7a1d4fa193baa419dd4", 333),("f771bff3e411c13f5fad663330f0f6ca63e72f945fd879d74fb860c84835a00c", 1083),("bbd103c777b5ad50de82d049c1041e5516a79b9aec94a6365e4bded62ea209a4", 807),("b7075d2b3df812eab0833188ce688f295218853f96d26b9488bc1ce9fe3f408a", 1288),("3496ccc519f4f67acbed07b7ab74757da29f720341f14506db945b64c34c5c43", 1324),("38317edea5f41a8413cfa0d2457e788e71d1979335c8519b63da27fdfa315aec", 1854),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 1571),("4ae7f96e7df3009d9356531ddca3a5c9bd4d2d55fce67c9c8c3215b8b1921424", 268),("4898eee51fd5e61d859581899296a5515b3da88544bbf063eead7f0cb1801db1", 166),("4663c4e81c59780ea6c074d9708c1b559e32865ced3cb9e608625780cde636aa", 1814),("41bd33b76e8287b5f474d76e3c3e28a39834d61db3fd49046fd49a653edc4794", 1718),("22d01e02f3ce11de0040d8b7a2b5219ab8eda957fe68e14b4d16731bbe60674f", 1463),("13db9c4dbb4b05f8182940aeb6fe35f7475c03fad2c03ae7c9d7d4586190e05f", 793),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 660),("a5a9dce51088dc4584342667844fb29cd120020274a95f3329026693375a78b0", 382),("05d03259d7a1a4be9f927d7360cbea38ce44b81b236fb97427d24ff4f1da62c6", 63),("a6e48a8939bcf9196c44f75cbc6d291b237fb1f333ff7a5d02ab8b28141085bb", 1110),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1818),("100b83ac9cfdedcfbbe4f61c67cf23ce30ad617c084481f5c4555d2ecb56c2ec", 621),("502062492ecee5b58908839ba094bbd67fa46d3447d4c82b376f09c296ff7e84", 549),("07deaeb2a584aa38851e7ddecff3fbf75e7172f1e7c6a15ad01c317ee226f32e", 898),("c1b03c6bb52207020ee33f9efb1609074d79af5bc913aab4fc5df3a081ecd2ad", 592),("c7dbe768206eda376beaf98af5b32f6ad88e1395ddbb568c0be2f28cc9a8af27", 2008),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 1443),("2cb6c992eb0884fbdbb3ff7a6b14d496db0f746049f40ee4387ae038e6608287", 1796),("5066bca3ecf91a11396efbb7bf4bfe81fcec7b973f91701b2b1a34c6952a4812", 1302),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 236),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 21),("1eae2dc33b5a5b304b3beb1c4422461a31b6624ff41511ccd7e92e854f6c3862", 1999),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 983),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 602),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 207),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 410),("d12682f8d06180dd9dbb7184931348862cd685dfc1b9c28eab2741ce6739fa91", 1057),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1811),("1a96f7857beef47553c674737b90a73acf087147ff68e480e5170344df8f9b0f", 378),("c15bfcc5100060143de313def76e748cf02e0c7f42cce614bae672cc11122bb0", 672),("e75001b78170959550e7943ff987163f9dc164bd88626855abba578befe08fd2", 1923),("67e70ee5f77b5f4fcaef4c1b5d9539a8dae1a18b608314ea501ba1126ed1c49c", 855),("8d5f8f2d76dd4117e1ca152d4480e2a618cac22c9660ed8c931f510e814ccff7", 436),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 322),("519ec61853dca52961f7160d70d7b7dd50cd81185f3a642c2fda25fef847fbbd", 1529),("350653e8cba6a4a6a408e973fda85b7fac55d1a372d875a9b8fc361cd4202bd4", 62),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 532),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1594),("b31a40899bfbc84c7ba780b6780bff5f33d44ce8798084db23db1a9038f9375d", 172),("78835be9889e048ae6166e7f3f422cfb388af5142e24cd03cf360089422df6f5", 713),("cc818989e3891971c7ab80a8106265c379b2b228d32295526e928eb4b4c9c86c", 974),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 1797),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 215),("b47601fffb6952129d143a1ade778dc477004e76617f160a9493b8acb480f981", 1652),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 1177),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 854),("d73fafbc6e70c458227deb3c00dbd36a10af597f92d3292e53c8f865d337e297", 1123),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1232),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 505),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1201),("5f83c8afe411267a228b035e3af2e2dbf3e2692eb9ac9773fb268af890ab60f2", 1072),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 814),("0cbbb65cefe16126fcebea58670a9646e6e29c782d9999e0ef995cf23611928a", 1321),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 173),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 77),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 665),("6572206c2a58ea17564af0381237f6482c0bb50a0c1bd2897bc895623f1c757c", 472),("4b0019a1518f4f518370cf214b8f89cae0ebcd8cb3bf2d323676db46c0f36b5a", 1273),("9bf49f38ced0034b3d8529581a19cc415e7ed9a6e362fadc325203f5a5b2cc4c", 1838),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 358),("e4b97cd3d5071d9e7cfdbf42bd496d1e5850afe2ee92f1a16db61c99083a639d", 1071),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 96),("dbb2cb3000fcbfd1ff337ddc5b5851f41734a5d17c63b67746c9462747f6f4e6", 1525),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 576),("d930337ccee12384f60aae8aed7c1044b856b09cded5b0e05a27303eac61ed2a", 14),("7085f7970aa793cf5eb3a7d41698dd213749130341c3faf96439aa9b8d3e15f9", 1471),("f2730be5d2bfc4e039c74b8e9c9ff38a37e3fd4639c695f12282d98e48e1ba98", 533),("eff83341e00950b5fc55ba22d6b48584c0d65e196fe7ca6863ae68776010d79a", 595),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 788),("53e6a3d5a8a2cc17c1bef9a2023a67c1bcdcfc170b116596c9f9cdaa3aeba6af", 1784),("ca0ba661d08c848c9265e7018eb8b84f9f1f32f8a5ae4a15d13942e1655fc562", 1700),("c8e7f3d0f4b244d4a4e46b91af87fe29e05859f458d41684f8cc32a93fd2e037", 1668),("d2e9bd9e9cebb3c85c8c0dd9d5370cf86cff70404f37464ff6dd89e6e1a3682d", 1035),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1442),("b74b2380ff90de9d91e64dea8c7db4339e732044d56c2cade9fe55fb2a79acf4", 489),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1752),("f12235c192288fcf147f73cefa5cdb5c79badffc8a159df04e4c823d157c5d90", 1206),("e36e65ea37089c230a6119ffa08b82721d67ae03f167d8728fe985251735784c", 129),("aa073e60b2ab9209202562757d7093f93427874dcda854a352d9f359fa6fb7ac", 159),("cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e", 1333),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 521),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 396),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 566),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 1827),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 817),("88adfc453e54f3d6c35d6978d763403d3606aa65761f869aaffa33763973b242", 1732),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 992),("01f112373fe074b867e58b9ba3d4cc69088fd3b561f929074e6af77a0bb21453", 1051),("222311a7fd492aec155c86d8a966259e2547c2d234f1d928290ce43a941d9dd7", 1601),("6a82f7e8c2285591ccfd9c20ffdbd710f0f5186d1e0334d3b03f639e53f087ac", 942),("03849f6af2b01270c771c070484324631690150f9afb22485bfe8489051ff3b9", 428),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 276),("2e5aea6d2aaebdbed333196f39921c1e5030e1b8c784b6505230601f18948986", 827),("91a1ce2949fdb9f7944fa6e6da6b3fb914ecfdf9d4e24ba34911e4d042c6a076", 876),("9805ed18d7b85868894035e602e20faad616e3feeed61b5d87fa26f976b37387", 1108),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 30),("89608f311b29030f940ca52e77c170b1c68b7abf2d62a319e2beb241307470e4", 518),("b5350f33ab7f0f7d2454b48d07154e1d27c37fbd30c277427e81b2fcbbb20d27", 1100),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 1301),("2cdc7f8e283e96b5a8395959805294b29d0fe6e98c214089e1e432df45cbff54", 512),("d4218dee07deeb1e9cdd465aa31dd32fca2d6efe57bfd891c47c03ded60961a1", 1771),("1211b5b39fd89a1e4abcaa74838cd1832c5081a7c204a0b0a21dac5963b7520e", 1181),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 833),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 371),("6dbbac9067218f60a9648b39b39626893a8b0806e007f01b41f804afa1491b9c", 659),("4a9f5a4edc2cc10d2680e70e9d9c574bbc35a2c70fb7c1a2f7f0b2aab2dadf47", 830),("65c5f34a3a20d4934272ac64fc48f7791224224c7c0b9166f41b943be8697ad9", 1460),("0291fd348fae0042e2ebdc63c36198ee46e70113b3def3294d6a2c0e2e86a0a6", 1663),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 1376),("8731a1e20df71883c34cd98c2138b2fd4dff3d4f0e91a1de724f65bc5fa289f3", 1195),("a50b963449a6e8ed4473912013c0dadf08eec9cd80381286fb5323d349292f3e", 1449),("71988b54973c3687840db817ebc6c27e3b6dd7721422d17d0437a191c45672ca", 731),("4ae7f96e7df3009d9356531ddca3a5c9bd4d2d55fce67c9c8c3215b8b1921424", 1284),("946de334a6304e4fa5af84166b6f1826f534494f4d74a2095edca16e4825a5e4", 1491),("2ce68c9a533593c49c64c89507390e7e2e6f60dfcb4f2a199b41b346456d0402", 1966),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 691),("68d08ba041c3ff22268f003e26d35ee9020db7312909cb7f7e39e039981badc7", 1380),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 832),("dda70884eaef138fa049ab78ca55613f4d856764f3946acf4be816df45513eef", 789),("9f1f541c56bad7f5f2cdadd61c14a54efcd65f6254c17c30736c0ffd39bae2f7", 1277),("d57e0e4225ce7c3c79288f85ac8f7c741ddeb98421bfccc0e18c1a15b1e0c7e1", 1315),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1822),("fac39f24ac6b44ca1094db3d5a3826db83c2bf610e9f1bfe9b56be3df175b289", 442),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1791),("048fa6a1e5c24209d950439632ede4bb35d1896b2735c604c309ef3210ceabd5", 1263),("bb96318fd4a6579496b8ace2fd9b19dd4b3a9a554269c7f5d9a20c9945c7bc90", 350),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 768),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 972),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1789),("41bd33b76e8287b5f474d76e3c3e28a39834d61db3fd49046fd49a653edc4794", 940),("0135129fc802ef3be292d9b88f487362cfc9460c1363bb5de31d282ffccf9387", 468),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 1586),("92d597ab474b3180fee5df2d76604599ac2ea86add845e5476e9ff80298dd23a", 1736),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 417),("1892c0bdc426bbf50217a795ffd422a805129cc38861f8d142901e91affaf042", 509),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1149),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1031),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1753),("07c69b16d9f075b61e8d6cdc3d709c1c32d6fb84f0510ea9a97a9b395be5cedd", 1378),("d2d70afd8554fe7b936710226d608be7e918ee21752c33f2792ca6567633358d", 1159),("2a6cdb98d5bb63be7b8c9a6b3d96dd87d1b4e039fea3a811a416948bbe18e49a", 2003),("dbb29a9d73a8426b38b954d317a9f5b72bc41406b2efe609e458a0e97fadea20", 1941),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 1501),("42be0cf9034b660f4e77347548640e29ab0a1402d468e8cd851765bce1bb04f2", 900),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 179),("09792fd06fd0c2ccc0fb0b8bdc9b2ceb1a4c84b99cc68e4c712ff975a249e425", 531),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 889),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 1903),("9cf1cfb010ea423953d0cdb1dfbd6211e76876a8393f4fa81752df9baaeb13b2", 498),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 377),("f6177c4834330f1b958c165750e90e56960a71e4e8c78682bd9b2723918f1009", 1495),("bb96318fd4a6579496b8ace2fd9b19dd4b3a9a554269c7f5d9a20c9945c7bc90", 460),("edd79de62be83a1d86b0aa99939d11d9f2c19b91e36532f51824a6891ccc2772", 1782),("47d6d19264efe042779557cc6ed17e0b2731131b667237ebe63b3678c0547882", 255),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 79),("37dbcd84143ef13fe061297d5c57db4248176e6c3c11220aa26eba53c7bde969", 707),("982f9770d339e21131238391a1489d7ab5ac582cbc40255296b9c851a64ca993", 1415),("3452b6bb00a5c7c266c7abc2bdda234a0e7c6aef6794e0f823df024daaf05018", 470),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1155),("8dad581ad46d78c73a07140e5208ff8132e629dad2beb39ef92da263cce27c2a", 1716),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 479),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 1727),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 1470),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 303),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1618),("9a9445754f59b49f7b4795b772f1f22d4f86b465a6f701344b4244e8377b545f", 426),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 1960),("2bf0223d40e7936249192967f8867a10d40ccb53ad056ef10ceb8ddc42e66580", 1485),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 1148),("5f7769be12de92eb0ad5b4481ec42c48ff33c152a4c9586e33269362c8f072c2", 1114),("a95c91172820e274c9914ec69a0980487ed608c365f5cbb812449039d20826e2", 90),("751952ef84b959eff45ca894cd04baeb9318af458e4e1654e6edac889cb1b4cc", 867),("0b666563052bd9dbde0032059285702402ed3a7d430550141f7da1fccef02fa0", 623),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 635),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 894),("3cbceadec0db1c473c75c68bf3e24f3778c7d08ccf629fb96cad8a948da98475", 1664),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1153),("b54f34bf38605f66cf625c5544df702d8d52038072edd344c9c13284727d29ef", 270),("f7a3cf25cc5fd4fd5e515a59db1b4a1f0066ac4f83a01bd653e4d3af20e40640", 701),("4d22aa93b87ae10aa84326952e8aad609df6675fcfcdaee1b00271c8f1612177", 692),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 1702),("ee5060b689e579d73d50e3be48d1b9097b4e5498fa457a469657ad99fd8b8f93", 1404),("1939ae0d38e14a78290c822c2d98553fae16fff58fbc8dc48c399d7ae169a817", 167),("640ea7b2025bd746cb214a61786de904bd40fd3730a05e34090f1f87866cdb68", 1665),("5fed2d8c8523257aefc20d70e085d30ae06cfa41ca636279e01f19861a468703", 392),("7a194ae2bfd10f7686b8236995448a0995ecde537d3fcd357711ae4d894bc51a", 1515),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 447),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1724),("4bf9c971b401fb68bdb4a009e98bdaee45964d07c81ce2ab11dbd338421d031b", 792),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 1994),("3bb92cd600ac04eca798f7188ddf657ed6340e59593fdb92e0b069eb35e4cea7", 1258),("670feef8245beaf7182780de0b34fe85836bd2f891160e99638b09317d8c853d", 1768),("f9bb68a7106fa814da43f219e06ca0510ea69026a9520a776e9b97b9aa4a11c1", 1342),("d37a2dbd3c0cd1030f1ce6abc83b8cdf8e9f7684a0cbe37e9bd941eb4e96b410", 717),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1949),("4a9e28c7fe343d82d2d31fcb17015d600466091e95aaf716d943e2f5e5f08d6d", 1427),("94e02368944dedb539dbde90baaacbb50c0dc19e95ed00e6705f8e9781086c85", 1902),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 194),("0f1195548c67e42bc94580405493c02f85ea93926f8a679f620bfcdc33c5ab0b", 1473),("355813592d537955a5d11a5cffd853bd7332608a88aea3c9ef29abc65b09ab94", 1743),("f4c60432b4ded258f1f63e8c08060abf08862690977c95b7a0733b5620268b4c", 231),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1835),("ff021988dc4d4fc4b8c33c9143b781824453efb21c709854539d62e918e230f7", 550),("0c16f57c0dcbe63bee210ab9faacd41f1817d9f19ff0cf283aa482ed9e0a7f56", 993),("71e5e40351a1f300f5b1e7ca10f0ae46d9b4558ab40a29a0c28e659f3c75d1af", 1423),("553846454c5ac96ba3966d446d1c2eaf440a59e19dca4d08218e42cb0087a1f3", 1611),("2d1a8deec6e8c417464605468bbd8971c467536412a064b541706ed30bc4feba", 211),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 1429),("de82f6c78a20929bd1045ecd84c583d3bc786ffcb5ae8a6ad05cc4872c6b2cfa", 170),("de27248109f92677a3f3a0a03a200729586da2ed80b78da91ccb56089c7245d5", 1152),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 515),("acd50fe26a641d46757ae1a11fbeeb9a7c52bd40f388b060ed46d199d9d8db24", 86),("a0dbb1b13c39f379176da0b3f9052988681905c89ab07aa3ba884c6d4e87d3af", 1092),("493d0c61c8969a5e5da5b94fe2cbac30cd56046a1961e14670430c655d3721bf", 706),("9875c5b6b074436b92d7c4184d84643ee1a478d37958996942fbe95e912cd7df", 1432),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1156),("96c12609fc9988001445bff1b1783d980ebd80e7f1287d28e26eef376117ef9b", 700),("f7bb5d9d2ad5276ee6fdfdb8f6c34b550c3e954b9958054147f84ef3ddc00483", 1014),("d7a00d6a330053ff80ee2c94d1aff382554b2529b219580fc0abbc38b2f836ef", 66),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 879),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 920),("3accb10c9d6208f59987e99b96f5b8e531a8c168bf70baf8ba967e6a3291fbcc", 1786),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 858),("ec98f89eeac84d0ef0e14f6c0d6065f2332f0ece2376309134bb0055c3f5d3e4", 1335),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 1979),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1196),("1e41f5d0f2c036ec9deb5f9698e673a61771f6ba0be7dadf1d878bd49381d565", 644),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 389),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 123),("857a225fbdc9bd9a193be0026abd820d2403353d7f59c7a1d4fa193baa419dd4", 1686),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 323),("94e02368944dedb539dbde90baaacbb50c0dc19e95ed00e6705f8e9781086c85", 1167),("495f5ce737bc3824b1a4bf13076262102b2a1f99fc70379e15c5e14e4903f3fc", 1124),("a2ff3d5acac75d8bc2e22af2139e417012c6783c0f039e9bc07c74f7a9cb1c93", 1624),("b0209ff50d8a0d3b2500db74cf81d1e2ef3a47196ba6dc28148ec5878f53fbe5", 884),("6a74ad15c74a07b441ab0ab5bf5c8182774421582f3fca2955939484f8e9726b", 1781),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1299),("f5032f7cd694b6fa048aa11ffcd7f3728f0a30d8e723c633a8d56780563d27ca", 560),("3de6767744ee1fbc5ec0a6fecd8fef2becbf536d92861422cf78976d82b3dea8", 600),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 338),("30d08d681dec813f78f736f3e9d1b5894d8fb1009d88ab6ac4af0cafa2df95f9", 709),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 1569),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 1250),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 480),("1f25244f2082f062228422775b76afacf9df45e51dd2c9ff7bacc7e2589e3d04", 427),("78cc47c96b4c2d318002f342fb0658ba4e4183294c0d7f7c3e6489c87d43f064", 1365),("4bcc7c3a3a5dae8ad6a96c7bac306421cc903c5a8c28a0db0077203b6f17e33f", 1406),("0cbbb65cefe16126fcebea58670a9646e6e29c782d9999e0ef995cf23611928a", 3),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 1890),("dbb29a9d73a8426b38b954d317a9f5b72bc41406b2efe609e458a0e97fadea20", 280),("8bed8445bd28a74e151f01d86e096692b9b2ac04ea7dc0a384d686f8fa9c6479", 558),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 13),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 966),("6af9e3a58351bcbdfe5f03f504f4fbd1d3d256d71ca9ba69a965d343a75a6c32", 1865),("fa8efd699b2028670edafec56519f9f21dae259731227cbfd81d14ad40326a3e", 253),("5ed361d45b385cfc0051d0b23f90f9dbbaf31bbf477fd680c50291cf660fdf54", 1776),("946de334a6304e4fa5af84166b6f1826f534494f4d74a2095edca16e4825a5e4", 363),("4743240db87fee6fe5fe0a4e26ae7b434d3e469304b4e3dfea1abc9a6a6c25de", 326),("8fa0166bcdb1b7f1de637b88661e8303eeebb023666c8893dfa260afaee9d524", 294),("bfa6f3d2573dd1380b922b571770efe934e119490902697fed0cc07ff233dba8", 380),("353f3e5c3fd7015fe8d93eb30508cfd3304ef409807ff1a5337e4ca504efd352", 370),("b80a1807be41b098a410eed7420aa73cde35cbc8c4931c6a44c7c4bda12008b8", 1139),("1bbce39b07343675d0aac732e4cf0e691db0bd09a67f393eff9e32b97c008960", 313),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 821),("08a8f7e278b0e210a70c35be3254c3ca4d1cf2e983974ba33d7f61fb84277c50", 679),("a533b2045fef94d7f79e0ff7ac827ddaaae7d5f9888bc487dc3662278d3fe948", 437),("c474ccf8da2c7631345d6b1c405442c531578e1873932904472c54dc352186d9", 259),("4550407696838a6bd752faf9afc14df849c85d312cf0afa7a043f82507308fc0", 58),("0c16f57c0dcbe63bee210ab9faacd41f1817d9f19ff0cf283aa482ed9e0a7f56", 56),("d55e56c6cf896ddbfd2b33d06f9a3fcf4e48f0c184aac990f723a111361b16e8", 1053),("6f95cd01cd47972e74d447d65c3f69a72ec8f5911088e76fb33097aa2470d75a", 1090),("30d08d681dec813f78f736f3e9d1b5894d8fb1009d88ab6ac4af0cafa2df95f9", 1792),("0170fb8ea7801cdf02068804e8ee69d2b35a490f2bd392d8103dc4709eeb0ee6", 1710),("4f792d188451a27336b55111702fc046a71ec2a9940fc4953c131a39baba09ff", 1341),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 24),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 219),("b7b3f9db1cf644f9f3db8f6e60d6d9f0527cf51129a551367297ec676faa1395", 1517),("9e6edf83306f075298e0a361f36e114f89f8763f694a980b3d2b0425ad7a2041", 652),("674386653a61b24ca9f2e014916048297acada149fa2a2ead0b599382e45a809", 693),("0b3a859b21440a4ff71120c6d484eb0b9d0b5da0793a56d333806c5226b63980", 507),("0c16f57c0dcbe63bee210ab9faacd41f1817d9f19ff0cf283aa482ed9e0a7f56", 791),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 1625),("a0d8a49e8c761062797737d78cff7dd5d0868b2b97bf33e171df7fe3566eefdd", 216),("a4ee725cac1e6ba47eb99b0d90b42e33d5400f443b8d72256249dcd969dd055c", 1714),("2c9e74c0f039bb4c217c3d070d4121c6b7daac518b76cc34c500e82deb81e5b5", 557),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 118),("6da0aeac3da9cf2321ea6c9d1de1106decb571632309782386c9cc9cd288b46a", 1696),("951fd0ae99c7148f5d0c9f407e1d6ebe2b71a960ba7b12946064892b104d881b", 1370),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 201),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 1157),("2819b5f2d30bf0750b9a1a0030ad5fa94900b9e638a0f6ac2bce83ceac3da362", 1939),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 459),("04e836d9ed3866c2a2eb2957b330fd289f41b7c6bf2fde9d71bf832a19c9c329", 808),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 836),("461d24a69207c6abb85e6faa284d035b630334d13f9428de027c47c2dbd989e4", 457),("bd3a07e1210d57b129ed99e2cff66766a8dc914f1612ab355407ddd36fc76f7b", 1469),("4f792d188451a27336b55111702fc046a71ec2a9940fc4953c131a39baba09ff", 841),("4e84d8c561dee497e9f66bcb2a9cdca8e7860186d7049ac30e921cb93f9bb2a1", 1119),("a555c4f1e300d5b8b8275c29ff9a8e06f746ed736edef52ecf2debf65f065e36", 4),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 984),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 81),("5066bca3ecf91a11396efbb7bf4bfe81fcec7b973f91701b2b1a34c6952a4812", 342),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 374),("6e4c2073ffa7d3555eb794376f03056f5a14ce347c211d5592b2285c5066b334", 603),("6d139f4193f3c17d654afde57869ba8e53ee4e70141e3d0cd3f1bfe80c279ce3", 1688),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 1318),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 1047),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 1518),("4f9498d627abaa48fc5e313463655fdd77aa8acd2eceeb43b040bcdaf22f484d", 649),("b2417afd245f4fe1c38601b288df0167918170e6156b0416422a57122b60532c", 616),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 367),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 89),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 650),("cce420382003c622ca3265d54f2b4a7ed53cd96e46d9d165969d330d3c665bec", 488),("05c5fbd17e3b265d5223f25e0aa3747fd9d086a75c1ea372e8d7707d26e46232", 106),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 970),("f771bff3e411c13f5fad663330f0f6ca63e72f945fd879d74fb860c84835a00c", 537),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 343),("1211b5b39fd89a1e4abcaa74838cd1832c5081a7c204a0b0a21dac5963b7520e", 890),("369811d3ab47e35bff85a4af644f6fd9794d5c15a89468a956f6969649f5c20f", 278),("95d78500734899e3fd65f56fd087599d0693fa2e04023c41afff2707434f2fbe", 257),("a17fec45df36c7cc45a13a2754e2ab7c7a35a179f1511d44b30753646de69959", 1426),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1943),("e1d3e659d9c9174757db14802bb27bee70b44a3964ae4d490e167fa78b4aaef9", 1721),("05d03259d7a1a4be9f927d7360cbea38ce44b81b236fb97427d24ff4f1da62c6", 539),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1595),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1855),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1845),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 569),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 1528),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 1565),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 1580),("58341d35829ae36f63244fe44f480b9774f8a125ec3e340c1eb3a25e67da04ed", 1568),("62de7855eda19f557978c85befb72391bdd31dd859b73d9dda6e3512f4476801", 296),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 386),("0c16f57c0dcbe63bee210ab9faacd41f1817d9f19ff0cf283aa482ed9e0a7f56", 1997),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 1996),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 1094),("d0c32d7f5b306b1614d8d53426a3f52e8971fe8d56f20e3b435b0198883d642f", 1275),("651f209302d3035b9f0dfd9372ccdb77a30f4ede68bdf91301d99e3c4f7d37c3", 1880),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1972),("ac48165a4f600fc2ed684dcc24182cf66f969da19c32526a9d36ea6a64988068", 51),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1385),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1038),("81537cf105cf802521bca09a93eb555a685f8f1802f1d794a427983e3e4203a3", 892),("199052ca615d343f8e4342894c6c712def6fbb245bf8f188523e6fd9ca11a5ea", 108),("d04e5ad88518457ba027d9a46ff4bdf0785fc0fc2a747fb523cd3c1ac9847576", 995),("c7975e503f9e748ee54565dcc24d4f1170f7b7ef573cd34b22c4f9f7e33e47f1", 1220),("dbb29a9d73a8426b38b954d317a9f5b72bc41406b2efe609e458a0e97fadea20", 1409),("86e7069dc10f9f42a8e1eea1fb870611b2ff8603b8b12f83fd558107a8f3f0d8", 1074),("904826bd329d037d3fcf38e7eb54295cca9f5c68b174c3c750ea5205254ff6bf", 1266),("9cecdf3eb5c4736f3ed21aeed0a9af8a4365c2bd38a88cdb2188af6fef753420", 733),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 1998),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1597),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 22),("16e876522f46df4e847e4298d008a531739860edb29523068a78f0bac32297db", 1799),("a5a18bc5cc204cd6c7850fecc5185378fc62340c625dded4ddca55c972ea97e1", 1608),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 1397),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1121),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 492),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 772),("e406e6e749189e29edd5362d0f0a43d1343bd3761e3ee2da4358b4ffa86b50db", 950),("ed817a1e117bb940e559511add3614958888fe5bf96a7cd5dd9ed2bcc53ea242", 453),("6a74ad15c74a07b441ab0ab5bf5c8182774421582f3fca2955939484f8e9726b", 1078),("bc3ef31fcc7f4119725d3e57d70cacd339e38d93c1331fe13133550e25985910", 1832),("5f83c8afe411267a228b035e3af2e2dbf3e2692eb9ac9773fb268af890ab60f2", 869),("cf34217e1151dd82dba51f02f2bb341bcd6fefb15c87d470764b5b819eb3154a", 1190),("b0401253240415bd6da1c539fc5a8235dc16cd9958e69d3f669ccca96531ac35", 1798),("c203b9202b961634f083e86d85921f403edac480a302134a85a7143ad04f6fe9", 634),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 513),("7085f7970aa793cf5eb3a7d41698dd213749130341c3faf96439aa9b8d3e15f9", 1623),("5d534dccd6513ec0fad7665b7cf9833ee98341da30e17257a9f1d6d1a521b180", 487),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1354),("25b3234c5b1d40ac38758db74e68565e9b477f6e919fc596f77b1a3bf656a926", 1279),("93f53548c48054c5cccf53480629f5f37440a2d3a3d1723c023e44f45fce9501", 1001),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 994),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 154),("6dbbac9067218f60a9648b39b39626893a8b0806e007f01b41f804afa1491b9c", 1567),("758aa79dcc0b9fa3d893624bfe7e184b6964fe3a4c6b09d18c09126f5d552e49", 284),("cc8c70036a5e8a34ff7dc06b54a14bd1b5008e6ea3ffdd7a1bf7e5b361891268", 611),("b35f8b917893c13f557f10b684cae5061fbc5e725e6f483d5cf8dbec8c2b343f", 1472),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 810),("e7b55b0cbd73aa180515560fdd59d435d05cc51cc1f97bfede8a507b435aa3b7", 794),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 191),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 1336),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 1839),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 1191),("c57eb5b8bb1488c2bbf3d0f975719ac5adecdab7a91bbce1685b8e93f7ea9c5b", 146),("ce14034611bb7068be1e4cef5d9f4006eb78008e3e33d193b32da66ebcad6922", 1603),("e378e7b78a62d4f27a8ae1da4d90ff7b2b68b296da4cdd0e84747e98d56b36be", 561),("163336139fb46ed7d9e34e6ab59e091f1c66384da4dc65b35783ce708cef021b", 510),("ebdb2741d815cc87b0f5fd71198b44705a0f740fa9d868b89c8c28b002d7e9c5", 1287),("3496ccc519f4f67acbed07b7ab74757da29f720341f14506db945b64c34c5c43", 1804),("b6648784de37a4f13838630f08e77de6b6e64a5c037217e28b8030cd9006161a", 951),("2d7b5792355d47f36eeffbaca2b0018b1b980e7964c33b76b6b60a4fb04db88a", 668),("9e6edf83306f075298e0a361f36e114f89f8763f694a980b3d2b0425ad7a2041", 760),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 448),("e2fca684313dfba2dca17135e8a472497996078de46596d5a47f1ce4fbca8d81", 264),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1973),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 282),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 646),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 1988),("982f9770d339e21131238391a1489d7ab5ac582cbc40255296b9c851a64ca993", 925),("276151050d1d0c08bcc9cd77ba22d220ab46f690477d73694c26ec3a7781d82a", 1583),("6bf2b451fa42739f9e764d2bad7caef7cbd0f4696b49fd77819a6eb55f1a2e30", 842),("ba88258d4b92a041c5dec03e9654c92c3f58213fef6451d70d9b55f664e55ad0", 1833),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1546),("30d08d681dec813f78f736f3e9d1b5894d8fb1009d88ab6ac4af0cafa2df95f9", 1005),("b5280c8d4c738e4453eefc2188e66e6a6b4faaf4af4831b63304a80a3e3976eb", 716),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 1957),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 969),("c474ccf8da2c7631345d6b1c405442c531578e1873932904472c54dc352186d9", 1343),("b54f34bf38605f66cf625c5544df702d8d52038072edd344c9c13284727d29ef", 413),("19b0fa5d5b8fae64ecdc8a18ad3b242e24b7079803cab01ba96c5b1dc401bb29", 1683),("bd3a07e1210d57b129ed99e2cff66766a8dc914f1612ab355407ddd36fc76f7b", 229),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 856),("e169af5f2c0601fa6c828f886652a6924b677bce2a93b3680c62fab497123019", 458),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1319),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 677),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 113),("c20ada1028b91606129d1147e35bd5f6e6a44f301f2c0af1fce5c6d7e2ea470b", 1056),("cc818989e3891971c7ab80a8106265c379b2b228d32295526e928eb4b4c9c86c", 225),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 614),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 782),("5cc6a424cda96e21f0c9aa57c39882617d941927cf53983adae44281054322f4", 967),("f178ad16fa2f3add030daa2b374552028b87858021baca50b3ffd0d2fee762d0", 1507),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 110),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 1959),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 930),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 88),("a7901e5f411534607e75e97ec17f2c858f17a81fa018589fea3d14b749bda5a2", 1143),("0b927d67d8316ace58aed9e8d7567d4acd4b810cb54aa02ec3a309f9b0417b24", 1101),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 311),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 319),("bba2325d045a8906e49ec65f65220d543213b105c6138111b281ce1370b306ba", 615),("f2730be5d2bfc4e039c74b8e9c9ff38a37e3fd4639c695f12282d98e48e1ba98", 1942),("2277c58049f415f62fcd53af53d111064554fba7b9ebaadd57aa4808e2da032b", 478),("d12682f8d06180dd9dbb7184931348862cd685dfc1b9c28eab2741ce6739fa91", 1170),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 2001),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1920),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 729),("961ed8301afaf93ee4d8a95808eff66e1f28a4821317e3287bbcfbc963d95f23", 1614),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1286),("9805ed18d7b85868894035e602e20faad616e3feeed61b5d87fa26f976b37387", 354),("81cbf0c327f37c2fa3a554fdf43ecb8383276ecec2b887e10a1c640c9d6c528b", 1487),("a0d1478a422143e4c27e92724f4dfc8a3d08314e57de528e8d669972edb1386c", 1535),("20d975013dfb3586676ba15d4b61230585bca8546377c663805f571707e5138b", 1790),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 1928),("ee5060b689e579d73d50e3be48d1b9097b4e5498fa457a469657ad99fd8b8f93", 1533),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 1416),("50cd6289cc2b6eaed6fb7cc3105d3c0c0e33b5cf2939be85d09eb18ec4109525", 1520),("3d3f12959e3a0af0a4bc12c23691742ed65006e9cb132979d2a1f8f1523ae08d", 1161),("c3edcd6119745bfb853354ec27aebeb1745099dd7f6ac1e6855681e1558fffb8", 423),("1eae2dc33b5a5b304b3beb1c4422461a31b6624ff41511ccd7e92e854f6c3862", 99),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 764),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 1317),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 1128),("88764c81da40ee6d51eb6a25f74a49700bc6bf4687cb22aad9ce4cf8b935c926", 285),("a6e48a8939bcf9196c44f75cbc6d291b237fb1f333ff7a5d02ab8b28141085bb", 1417),("4663c4e81c59780ea6c074d9708c1b559e32865ced3cb9e608625780cde636aa", 1482),("5faa53ddca8d2ff328914ceeae0e73763e171ea40e59ba24a1977a7f8b6b9840", 1558),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 1431),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1309),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 637),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 1556),("b815d002304d553477487dd48e7e57803bbb5bde51720d191f3e13f6bb79300e", 1357),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 937),("6ca1c02a31e7617bb3e89e6b0b0577b49f9e840e6676a4905903a50c114a5ac3", 1160),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 683),("d62f83b111a817d5ba779f87ae9248a01dae60d36a5fef5bdad4996a3ac0f681", 1598),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1144),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 1891),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 1541),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 1741),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 562),("b5de44514088a08f608e3c5ea39b48d92a13aec53b74a5eeb2d4f3e0ac83458f", 886),("158a34edd5e5d7cd9911848ff5a740f1c6d6de12a28f418c859fd468e052f39f", 1311),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 960),("d1c8c4771a877c2fe6dce77e3bbc2518700d93ee9e0d2a2c16a7246a515758ae", 690),("12e9b3972c16188aabefed522fe42377e440f5577b5b526e6927dddffcf6b5d8", 1693),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 1711),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 1189),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1554),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 1326),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 1925),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 1524),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 848),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1023),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 403),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 153),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 1783),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1602),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 1779),("e040da9d2fcdebfa3b1ea3bbb6ca8eed614421e0e345eb790172c6b47d674680", 1566),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 1016),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1773),("89608f311b29030f940ca52e77c170b1c68b7abf2d62a319e2beb241307470e4", 534),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1334),("6c8ad86a026095db4707a826522be21c367d1b5a7692ca1b36d5f9a5093c7bb7", 1182),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 1433),("1cf89e4173743a4d7391e91281b3161c43e41ca3b571d2441a23179334e5e6f4", 499),("5cbc75b2be1d3ba33df4380f615b7c818222c086ff2cb8358142e4e6afa62972", 292),("c82d4f6bef32105205c052b3491b45d9ef5ce3fb3b0175542aa8e8d8f5735ca2", 189),("7a85630d874e4748b666ea2db9396e950fa39d5a017afd98a111335c8ec0e2da", 346),("68b4e5190c8168d64c1792a4e956d3ad8628e6b4368d544fc12e9e927bee91d2", 1253),("100b83ac9cfdedcfbbe4f61c67cf23ce30ad617c084481f5c4555d2ecb56c2ec", 1813),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 924),("79026e209b24167118832ae13db00a5ca82634ab80dcffc0c3ec53eb957a01de", 486),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1265),("b47601fffb6952129d143a1ade778dc477004e76617f160a9493b8acb480f981", 705),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 687),("83febf935bf9c771e75c27d21aa68e5300ca6fb4a46da6f857eac120788ed915", 1024),("8f16e5dd849b4c5e4108bdb0dd81f8ade0d96cc9262fe7627d5192c02ee0dbae", 134),("f9c5690d45942816e4979bd43b49a56a0083c13574c3c9e619f261917c8ea1b2", 799),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1425),("1266a9c94dd6febd36d959ce1f3f848c35d0c18bec3f8a6eacd2cb2615df98fc", 1060),("781cca0b42bf1fbdbb01fe1bee0a2679b298de00c9a813471ee1aaf45db51c07", 750),("90d9c31a37abea26b61b2353196acf3232fb11a7c99a52b3dfc587300321dfdf", 28),("641e4601d1002b2535916af90bc5c53cf58639f8108d8064046241c8e327000d", 1689),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 1579),("eb9e8e6131606ddc86e622d6b7954c779fe78be4840a396dcf74c12453c59165", 1247),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1154),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 1722),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 589),("81537cf105cf802521bca09a93eb555a685f8f1802f1d794a427983e3e4203a3", 1111),("4f792d188451a27336b55111702fc046a71ec2a9940fc4953c131a39baba09ff", 1366),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1117),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1345),("67f386271bffec2cf91adeae355a80018a0cc2042c085bddaca27ecc774003f8", 915),("cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e", 434),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 1137),("2b16bf936f8e42429b131d9cc24b72c653320a818bcafcc26ac466c4812386c0", 1645),("2e6eeaa529fe2719818f304218a66e4f148e20f94c2969cdfedb51acdb88a9aa", 1764),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 351),("74e470c0cd4bd98d9e3a3f163719ef83e15f48d14ee91546f846a3f9f9ae0d04", 584),("e7b55b0cbd73aa180515560fdd59d435d05cc51cc1f97bfede8a507b435aa3b7", 161),("6dbbac9067218f60a9648b39b39626893a8b0806e007f01b41f804afa1491b9c", 1620),("95cb93df8e2b200b16e5956a198be8b03a0d04d4c436edd67309350755b1b4d2", 1553),("104017c599bb1d49991f2f9121ac5ce1dcc1310e0f75bcebef5e59669bc04c3b", 508),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1255),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 1436),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 1077),("85de60661b4ebffad9ab6db5c0da516291d0d80a402bbbcd1042a041bd96465d", 1075),("0c16f57c0dcbe63bee210ab9faacd41f1817d9f19ff0cf283aa482ed9e0a7f56", 1172),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1640),("3ae265e805e06081c230d82546785731bcd3e3e974eb6b66a753c12aca933fb9", 1034),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 491),("e1d8cd8f6429b321ae3a4ee5cd7d66779e80595f74a65d760295a9c9fbd276b7", 361),("9ed0bb6a80207a4a8d9dcc723ac311de5e4ff0dda8f0487afa5b9a0affb0ebb4", 1332),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1278),("b87b14669bd5bedb909133d81d68de67e6940fdca422bd28bcb5696c3892b822", 1882),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 450),("e45fb4396fb77f7366e63f7eef3fd063df11984769e802d4f755856346101e33", 624),("6d157e4a76e43cca93450c7954873c3b34ac0473c5f102eb2e6255349611ed95", 372),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 779),("e1c6c6e9817b8dc967e8dcd4f93a379b42d03e8659f41344bd1aec02c582cf74", 1875),("af309e7e2aabb1addc670c1fb6e639c467adb008ddbdbfb934fbfe7253c5617c", 1254),("369811d3ab47e35bff85a4af644f6fd9794d5c15a89468a956f6969649f5c20f", 745),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 1907),("34ee0479bb305f7da135ff9a085ff06adc9ef9cac4bf1dbf0aef986c0ad75a75", 391),("856b155c025211835c58e06b4420308a4254b3b77e947054579bde6684c28ec7", 1004),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 1821),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 643),("42effbfaaa8e3213578c3a84c29cc2e574467e91b4aa7d94121b5b250857b6d2", 1570),("a6e48a8939bcf9196c44f75cbc6d291b237fb1f333ff7a5d02ab8b28141085bb", 766),("dd2a477e400ef58677b39eddd1bfa7ff784af6b6f419b4b9bc72e948b1e89726", 895),("f2730be5d2bfc4e039c74b8e9c9ff38a37e3fd4639c695f12282d98e48e1ba98", 556),("9725e189a78383ae053a24aad82afcb142d01c7524af63b836d754f0ffb2c3a1", 1606),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 1765),("3c1c4335b26c33cafdca346322a110a65dded91d4a11a00ffc3c277385c28a6c", 588),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 1347),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 899),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 689),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 1948),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 74),("5a0a700af7b3cd470ca426084327af9c585985789fac00926b456ed3e17f8588", 1987),("dd6ed84fce2a462db528a5f4ec91269156346ad14beb90786b541cdc61e84503", 93),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 111),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 944),("90d9c31a37abea26b61b2353196acf3232fb11a7c99a52b3dfc587300321dfdf", 1795),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 1476),("3accb10c9d6208f59987e99b96f5b8e531a8c168bf70baf8ba967e6a3291fbcc", 770),("4898eee51fd5e61d859581899296a5515b3da88544bbf063eead7f0cb1801db1", 1375),("9dac022d56d6759155f8a613488d400fbbb93bb9a4920cdd943b1214ec5dd116", 1033),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1956),("7a10a4d3da8081f7234845ed153cdb477e10bd6294cd74bc75b04399c123d99e", 1464),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 1186),("fab0b6d63d89cae616c45b75c2388606d5d07898522200950a00dc47f2e7b0db", 882),("dbb2cb3000fcbfd1ff337ddc5b5851f41734a5d17c63b67746c9462747f6f4e6", 1859),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 1086),("d29386bf67cf4f09411127bcdb4f4888117d9c06fdb82333774c07dab0669d66", 1894),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 314),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 142),("1e41f5d0f2c036ec9deb5f9698e673a61771f6ba0be7dadf1d878bd49381d565", 1666),("80ea9c3db45e3b821fd065a3ac771e21d96296009c9bf046d3bf45d5e71de641", 295),("b7b3f9db1cf644f9f3db8f6e60d6d9f0527cf51129a551367297ec676faa1395", 320),("5d80c0a53cdcb06be2d58084f561af50117e7e7041561f8c02a0159f8eb00b43", 222),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 91),("904826bd329d037d3fcf38e7eb54295cca9f5c68b174c3c750ea5205254ff6bf", 1065),("42506163c317331b52429d961ae3e8b0851d6262b1e069c88740b0a7cca66ec3", 1889),("9dac022d56d6759155f8a613488d400fbbb93bb9a4920cdd943b1214ec5dd116", 1621),("07393d0fe7547f9dfdda2c879c1490dd81675a3d160f9c4940efecbdcb6c11db", 767),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 19),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 538),("25aec99c4a0b83b10c0a3e76518fbdc719f0c929e5221f6d21a3a4bd502f135f", 1383),("ba88258d4b92a041c5dec03e9654c92c3f58213fef6451d70d9b55f664e55ad0", 586),("0f0e00dceeb40bf41e396551fa45d049a4f981a6976c23a06aec4178aef24f09", 1538),("7e0e93ee68c5718601657402bfd68ee488d47768718c7d36812fde6e8e3daaf2", 306),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1840),("d13451b20169fc02bd202693337148e185470c794b5763e749a233b6f4f89732", 217),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 1955),("09e3ca1d8f6231dfe26850103546583d20759a046b7fcef05d392ee01699b8de", 1893),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 309),("a189720c645710834526ff32e988cf5339e8a936ab29fac7a093faa80e9610d4", 735),("7131ae3ba595c77b2ffe1c19ee03df39d0dec387bafc22415c53cad2394080ba", 117),("9cf1cfb010ea423953d0cdb1dfbd6211e76876a8393f4fa81752df9baaeb13b2", 1685),("04c2fc91d98a88ddb7e95c240065c045ead2022d8855291e7debf91876514122", 633),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 495),("b72c702c8315434f2498218a23d4918b64671b7d920c122e68caa86112bf9854", 203),("43f720ca6960ed7af55c1a598bb032e227aedc9a83cc46d47bcdb0f0da0a0583", 1884),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 1576),("1bbce39b07343675d0aac732e4cf0e691db0bd09a67f393eff9e32b97c008960", 1280),("1444cd9c9889865533bef96d10cdf21095deeba600566953fe9644a197c4679c", 1898),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1713),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 1166),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1025),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1916),("560ad1bc3c19036d8daad0365322b33ed8f7ce2427748bee172f28ec48818af5", 565),("114de90473972bb91d0fb22b72cbb97362bd49e8c65d25c4b4cab3a9e173fc28", 286),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1371),("82717670ba5738cade8907afb3cd1ca1916f8b9e4b028ba67695a7ecd80e2771", 1420),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 697),("e43b94ccba7e4b61a5cf93d9ea9db93dc8a65c202a42b3dd49b6f45bf31f5f2f", 1591),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 1369),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 1588),("e4ab8b583355779303b6213098da759ab425b90af578892404a94a5a39a40e3a", 812),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 956),("05d03259d7a1a4be9f927d7360cbea38ce44b81b236fb97427d24ff4f1da62c6", 642),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 1540),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1612),("85a3319f6c185efb8fd30d4b0178730fc1ea11713268bc262ac85b717f9b909d", 708),("f71d402664d4fdad1de144612adbb466c49ca1829f79ffb1495108364d37e438", 1870),("d37a2dbd3c0cd1030f1ce6abc83b8cdf8e9f7684a0cbe37e9bd941eb4e96b410", 1866),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 369),("3cb491bd55a10fea304a2a09830ba7227b099e5071884b020c1f224e477c83cc", 149),("946de334a6304e4fa5af84166b6f1826f534494f4d74a2095edca16e4825a5e4", 1392),("8ae2fbac8482c39a330932a10779964b7c686148f91f3f8139e7565dad49e947", 1179),("2e8bfc43620a1a1e145fc5182fa046d7cbb9dcec18f427a90a72e91b30b893ca", 548),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 456),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1344),("f859a28b9a145aa220a663a3651781b11e6cb0eb91dea4102898a691797d6e02", 801),("b5de44514088a08f608e3c5ea39b48d92a13aec53b74a5eeb2d4f3e0ac83458f", 1630),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1530),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 97),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 1198),("cc818989e3891971c7ab80a8106265c379b2b228d32295526e928eb4b4c9c86c", 1706),("9d94fa091f8549ad530604032766e6b4826ee244c506db160c8d16701dcbda46", 109),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 248),("c0f57c7ec7dde6d94af36cc3d100661841e867cb5ba383fb695c4e65000559ce", 1687),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 473),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 1484),("9b4b32fe9d4fb7724e1780e2050a5fef0454a9eb56d67a7079be931f3b2560da", 260),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 529),("cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e", 653),("3ae265e805e06081c230d82546785731bcd3e3e974eb6b66a753c12aca933fb9", 55),("3172d3c4cae7c53989efa1a9b856bbbff3ccc1ec014259209c7ddce5a4784ba7", 543),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 1512),("f77eb4a043d56bbf885445db2630cf5be27c70bd1d8b963b1205e75542b4c2a4", 135),("8d5f8f2d76dd4117e1ca152d4480e2a618cac22c9660ed8c931f510e814ccff7", 1276),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1730),("94e02368944dedb539dbde90baaacbb50c0dc19e95ed00e6705f8e9781086c85", 1270),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 1230),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 1243),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 1648),("e81bff1a405a0f3c810de451284469e1ce4ca70a80aff2ad4c899376bee4179e", 1989),("b815d002304d553477487dd48e7e57803bbb5bde51720d191f3e13f6bb79300e", 1970),("55d43d9439fed57e31b6cbb79b579498caf2a230052fd474bccbbbff0b2aa34e", 905),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1142),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1971),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1788),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 332),("877626391363e2beba383a5c746a10d22c94030ab22c0d99bacb973521ff277e", 1358),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 1200),("4223b92d2cf8bbf4ae96fc35f8dbb7b3d953a61b43ec29865b9d8d7a94120f93", 36),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 609),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 439),("981dd287f8db86e7d066155007ba391781af9592bbf03e506e5255fc8bacb263", 822),("eff83341e00950b5fc55ba22d6b48584c0d65e196fe7ca6863ae68776010d79a", 162),("1bb8b5071c2d84173fce16bd67dc5343a32aa9eca92eb36dc6b3c65c47c6fb3e", 1547),("e4b97cd3d5071d9e7cfdbf42bd496d1e5850afe2ee92f1a16db61c99083a639d", 736),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 1384),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 310),("08c2bf84f0e092ce4f34af6aefc54e2b9abcc303474159ed35a6a3e4af2c6f56", 425),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 46),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1626),("ea539175b7730c14ddec5b2ef70378f84b3c91ab8e58eba6d00c4ed4f63f694b", 871),("0450d96e77ef4cc20d7885977127ae26bc66404077780f1e03f0b6dbc57199c0", 839),("a8f927681f4ffe266c05526f358625fd7b2f3df83e31a6d94a740e2d71e510ba", 752),("6a74ad15c74a07b441ab0ab5bf5c8182774421582f3fca2955939484f8e9726b", 670),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 1757),("ff021988dc4d4fc4b8c33c9143b781824453efb21c709854539d62e918e230f7", 61),("1215e19404a9b41b1059d38f72b14e5e7bfd1f45d32f9f91024fe1b3c23791b8", 501),("100b83ac9cfdedcfbbe4f61c67cf23ce30ad617c084481f5c4555d2ecb56c2ec", 1224),("bbd103c777b5ad50de82d049c1041e5516a79b9aec94a6365e4bded62ea209a4", 1600),("4bd38fc7d48793506191aabf38cf276ab708ee42292b9c3711e37983e263ed29", 1691),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1915),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1709),("574cb968c82d8874fd15a3a15bd33bdfcf6438af817e9ac06d1e1596e979e6ff", 703),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 1831),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 1508),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 982),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 185),("bbd103c777b5ad50de82d049c1041e5516a79b9aec94a6365e4bded62ea209a4", 1837),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 800),("9f45c84e0a81fdb5b39d47a5c3e54d90608506043f4488a236dd7ab8cc128f35", 1617),("ff18b8930fa0053b61402a4ae5fd68dbbff9888bc5afdbdce2a73c7dfb95c8c4", 1248),("cd856f5d0669ef87adfde1d62a2d4135b380c12141cd71eb749dede46f0d42f6", 607),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1911),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 321),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 726),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1802),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 696),("c6434bbbbb95ab2ecfcea7983a7f3779d6121f92e78e562950d5b7aa01cb220b", 1184),("aeffae3744a37e03236d26b2eff3f1a0aeeff778937fbc1745062d05b09e863a", 1850),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 554),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1751),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 1424),("bdda500d2e06cf06d452ce9476bddc8d85f678687c022bbc47e988cfa4e734ca", 1851),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1188),("8f16e5dd849b4c5e4108bdb0dd81f8ade0d96cc9262fe7627d5192c02ee0dbae", 177),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 158),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 1042),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 192),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1208),("cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e", 235),("5bc597acd28eb1ddf02a419f20206e9bf5169059fde41812a632dfd2e5b95d9b", 721),("1591caae8a36e310a1becea365cff6d96c15bf10b430048799ed57649c54ce59", 43),("37dd2822378a5fa728e4fbe20d60bcd7df7bb691ee5454bd0a7813a3599fa3e5", 1807),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 381),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 1964),("5a826202247c89768798b7a6b41aa21146f9133ed3cf4dca7601c7be10c1c1e8", 825),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 1234),("acd50fe26a641d46757ae1a11fbeeb9a7c52bd40f388b060ed46d199d9d8db24", 1418),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 1289),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 988),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 368),("5a0a700af7b3cd470ca426084327af9c585985789fac00926b456ed3e17f8588", 352),("300276876905c4c6fcdbe64b103420a96d5eaf2bd2b3045216369888089e7a81", 1168),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 204),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 773),("b80ff2df1bfcf170777d8a3f4902092866d245814d107624ab84fffb1db5bf70", 92),("68a81562e45e914b63e25b3156d6948d144cfbe80e9f2fb4ea9c7d5321bb093c", 1022),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 411),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 445),("f8dc2d17deb2c327c13968d4c1f730f9933ac4fec188c18daea537f976ba5452", 1628),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 1780),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 957),("37dbcd84143ef13fe061297d5c57db4248176e6c3c11220aa26eba53c7bde969", 1758),("255025132ea85e65a754e109d9d0976777a458f7f52a2c4f00d9eb742523484a", 467),("da1959fa4f5480c3f40ca9949adbce031bb59afe072b5c18a6d53200ac1f617a", 999),("ce14034611bb7068be1e4cef5d9f4006eb78008e3e33d193b32da66ebcad6922", 1922),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1761),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 1944),("5c98dba48f5dfb97ebecb44efd6b97008cd3ba0426a9ac752a66db1d0caf5f86", 44),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1468),("1266a9c94dd6febd36d959ce1f3f848c35d0c18bec3f8a6eacd2cb2615df98fc", 552),("60262f93525e89e70bc1847e7af95dedd76196038b48f1f05b83717525ab4ff7", 891),("62c4051e0f73d790a599882da383765968a986aefb9c88a1a754f6f252e0ad2e", 1070),("05d03259d7a1a4be9f927d7360cbea38ce44b81b236fb97427d24ff4f1da62c6", 335),("71e5e40351a1f300f5b1e7ca10f0ae46d9b4558ab40a29a0c28e659f3c75d1af", 1876),("048fa6a1e5c24209d950439632ede4bb35d1896b2735c604c309ef3210ceabd5", 506),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 152),("fa8efd699b2028670edafec56519f9f21dae259731227cbfd81d14ad40326a3e", 1461),("d55e56c6cf896ddbfd2b33d06f9a3fcf4e48f0c184aac990f723a111361b16e8", 1249),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 598),("b5c49136ac33abc56c431d4a9b003d77def2838f81a45b158b43b23c09bfa703", 590),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1465),("c164c2f53d2f86c9881eb90a659489219a5ce4453653d0d256c6338a1806a00e", 1158),("465b4702524cd7fc491ace7a9334d36f9bb330c77746d22b0b001a3ee7525c56", 630),("8a1cb95a263c461fa5d9a0bae43132caca10339e73487d964397d8b7e5519228", 1701),("100b83ac9cfdedcfbbe4f61c67cf23ce30ad617c084481f5c4555d2ecb56c2ec", 1393),("c2da501ca59e94f5d2ebfa8027ed1372ae27b574ed1cb86db249f5222e4d2f0e", 564),("69444a25f831d748977099e84d8f5fb6a69f2790f82a285ca1aa7a0a9e547910", 359),("560ad1bc3c19036d8daad0365322b33ed8f7ce2427748bee172f28ec48818af5", 141),("43f720ca6960ed7af55c1a598bb032e227aedc9a83cc46d47bcdb0f0da0a0583", 18),("07393d0fe7547f9dfdda2c879c1490dd81675a3d160f9c4940efecbdcb6c11db", 393),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 163),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 1422),("b31a40899bfbc84c7ba780b6780bff5f33d44ce8798084db23db1a9038f9375d", 977),("15f7efbbc6883dfb1795ac950af66e3bf168563c552985fc1cd9c7ffb040f597", 419),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 664),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1018),("bf754da5c2b0b9194f9869752323b3d4c2044351d9b5cb4074e988456ea63572", 786),("5a0a700af7b3cd470ca426084327af9c585985789fac00926b456ed3e17f8588", 1087),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 1673),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 959),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 433),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 913),("b24e6fe6c7e677ac697a28cb782cf4c018de46528b4bdbfc93b718c38b4fa255", 327),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 1660),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 1654),("9805ed18d7b85868894035e602e20faad616e3feeed61b5d87fa26f976b37387", 418),("65aafbcd15eb331ec434a3707cd938cf44afb59025241834ff0b286cc0a88541", 128),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 919),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 496),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 42),("daac133112093834cd8bf44530a68002f39ea3b902665193d978336853a76811", 1187),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 1215),("94e02368944dedb539dbde90baaacbb50c0dc19e95ed00e6705f8e9781086c85", 287),("15f7efbbc6883dfb1795ac950af66e3bf168563c552985fc1cd9c7ffb040f597", 622),("d61d3b62806bcf1d2d74c4869e531e6380b40b9a62b20dc1fcfa9b22e56f00ac", 329),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 2004),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1209),("977485b905706501ecec24b098049d2dc790356a449afe08c2d8046db7f674a8", 376),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 1428),("c1e4dd88e14ad2396d5550f0274ccaaaa4a40b218253528fa8817a60a4ad3e50", 1669),("53e6a3d5a8a2cc17c1bef9a2023a67c1bcdcfc170b116596c9f9cdaa3aeba6af", 820),("14f8baa65eb528bf5290d7618a05fe83b4da4ac4e7496d829f20d34d69167e3e", 797),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1048),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 387),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 1131),("bd3a07e1210d57b129ed99e2cff66766a8dc914f1612ab355407ddd36fc76f7b", 523),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 1694),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 865),("a32af11d16bc2f0423728f7f0f0d34a73dbe61b6c62d5d346e9eab2c5f98ee7b", 1411),("d9bd89f58e8f17cda5ea2f8c441518962bfbaceb3cf1e585e898f59dfe00bbe2", 1615),("63d8e042b82bb0e89c09a1d0867012f48a044306c789d2cdb84fc8028c69443f", 1582),("22239b378a431648ae42ea83274870fd7cfeaa77cc745822681f1132412fa9ea", 1313),("238b3f9aa95a8564b8de709a2b3f7f90d07f90e1c0d6ff9342c56e75a5f88c9c", 37),("53d7a235d601d3b3ebcddf9a29c80abdb86f66954a09f8adb382b5e253b0a906", 200),("f45cc8edd48b41f84574613c7ebab04c255d5a854b484263f380b8bf31718dbe", 1514),("15de08324e03476775927164563b57d2748bb80e21a657e0190142c6ace10676", 530),("becad565708efcbeafa2919193792642d94f5dd64428223b5b670f35d35284e1", 1129),("a4ee725cac1e6ba47eb99b0d90b42e33d5400f443b8d72256249dcd969dd055c", 348),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 946),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 345),("c60b9a71620abbf10f2e8733b03bd70af49ebcb385052dff8a70adf8759d52c1", 221),("5e6c6e7fe2dc272b5b75e636c82fe187be9c46eb3dd81b896a1eef537c37fdbd", 206),("f56645d9544343bc3ba98217cf7e06d310db93cdf982f366108bdffefd6d73bc", 1635),("6178d34c473bfaa14b0fa57f4e18698c98c5bde8ea69c35b98c9f7b5b3d2b5d5", 1492),("71a44ae41ff3d56dad19945e4a1ee2c204f378faa19f1a35e8b3744ff9c10848", 1320),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 355),("42c8bdc473694069f16125c0dc4449e4275efd48eee6f4a6c12f236d0f6ce42f", 835),("5892301b21ec691592a1e97f05a1a42a6ecfced1afb762b420ad5e63096cef12", 742),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 272),("54ac425f65c79013ba1655c678b2d8a97dd6c526241852afe9cc77f4a2ffccca", 379),("6bf8f02b901d9989017212ad42d20c546e10bd81fda6d7ca05802c411da69187", 1073),("c474ccf8da2c7631345d6b1c405442c531578e1873932904472c54dc352186d9", 94),("c6434bbbbb95ab2ecfcea7983a7f3779d6121f92e78e562950d5b7aa01cb220b", 1917),("8b2e781f3c8f690284f112ac7fb288c1b140b07af4c312c401d2e71b7dbfd8d5", 681),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 1705),("4ae7f96e7df3009d9356531ddca3a5c9bd4d2d55fce67c9c8c3215b8b1921424", 1532),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 638),("b0237c9872f84a4dbc30c3ee18d583dfa2ca84086ff3a50660dce29b93a46792", 0),("9b336dd4c1ff01a745cc09d63e144d30247cf591dd059ce9177fc48e55f3d164", 908),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 936),("0a30f95acc63a0aebf1176b3b358f8c415abd7bcfe30b45eb8f8684f05902433", 1587),("04e836d9ed3866c2a2eb2957b330fd289f41b7c6bf2fde9d71bf832a19c9c329", 528),("bfa6f3d2573dd1380b922b571770efe934e119490902697fed0cc07ff233dba8", 1557),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 1439),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1169),("cf7e1c65b5ba50f06340c98c59b2db3ad3150d1c800e493355cfc05d3454fb2e", 232),("37dd2822378a5fa728e4fbe20d60bcd7df7bb691ee5454bd0a7813a3599fa3e5", 1199),("85a3319f6c185efb8fd30d4b0178730fc1ea11713268bc262ac85b717f9b909d", 1744),("7278416c0fd229045a5fba219abfdf963e28c494df1b290a312bd5552e22acd5", 1483),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 710),("640ea7b2025bd746cb214a61786de904bd40fd3730a05e34090f1f87866cdb68", 803),("3452b6bb00a5c7c266c7abc2bdda234a0e7c6aef6794e0f823df024daaf05018", 751),("d1968c5c23df75d3677eb096f3221b151c5f926caca0f1261f6b754d0d2e6521", 365),("7131ae3ba595c77b2ffe1c19ee03df39d0dec387bafc22415c53cad2394080ba", 522),("c8ae53f54252133bbad17084106a8819c63c285e4d201c49fe0014b274246fc1", 1881),("8d5a29bb5fef3db96a896dbe0e77790d627c2df094facb67460ca0d8d67ae6e8", 1684),("5066bca3ecf91a11396efbb7bf4bfe81fcec7b973f91701b2b1a34c6952a4812", 40),("292dc574dccfb127860008f35763b9aa2bf4b35046353d71108ba7490da5ca82", 449),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 587),("200b154a0da363ca3ab74697ce941619d8fda211f6f59741117cb7dc5d65cd90", 573),("e5c8dc40979a1c9f8ccfee1cafeca464e12bae9302d39cc62fafdb0def17ce58", 1272),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 1823),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1102),("f8dc2d17deb2c327c13968d4c1f730f9933ac4fec188c18daea537f976ba5452", 1924),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 1044),("7e96e9721101f0b3b934d5bb6ee9746c95c47b0b313eb843ebb9751568fc89cb", 1046),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 220),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 516),("07393d0fe7547f9dfdda2c879c1490dd81675a3d160f9c4940efecbdcb6c11db", 1886),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 1599),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1794),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 601),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 563),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1126),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 866),("0a30f95acc63a0aebf1176b3b358f8c415abd7bcfe30b45eb8f8684f05902433", 1677),("af5c3e7860c49bfc763f61000944256b5b345172729310964972d9ee32f13d56", 1098),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 1895),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 1359),("68134373bed8f81703485987e20a5f87fa5f9e5a1eb1436f905a1bf34e893dcf", 931),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 762),("961ed8301afaf93ee4d8a95808eff66e1f28a4821317e3287bbcfbc963d95f23", 575),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1991),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 1390),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 1105),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 198),("104017c599bb1d49991f2f9121ac5ce1dcc1310e0f75bcebef5e59669bc04c3b", 1488),("fbbf79da0adbfda890d3f41e3f72185c4f642215ec09a7142ee3d17d4bdc25ce", 1030),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 8),("9e6edf83306f075298e0a361f36e114f89f8763f694a980b3d2b0425ad7a2041", 1516),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1647),("d2e9bd9e9cebb3c85c8c0dd9d5370cf86cff70404f37464ff6dd89e6e1a3682d", 1841),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 990),("0c27ba3c5eae1e8a2b10148f322e9eb625ef2d50842cb86b811aa64ae0effaa3", 1127),("93d154e693340076760517d9dbec141ce99bf1dc7de816ebc07521ec80bafb49", 806),("1bb8b5071c2d84173fce16bd67dc5343a32aa9eca92eb36dc6b3c65c47c6fb3e", 325),("4c057afe02e1df74e690d24474a91d8a48d65b470d179599bf19081dfc9f8fc7", 1437),("1f46afe1899621a0fd8e0e5e7847ac0956003383b91a6c168676c5dcabdfc4e2", 582),("591946bc18a9b3dd788d9858a7e4cce0a4f85ec484c2b67ca74c79a52374a0a3", 408),("092122204430fe95043eebb43bc63c14500eb7bc6ef72ba578d8e99e7d7f01ee", 148),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 975),("6f7b1ed97f29c19850f2798dbabd1a2bd3eefce14fc95e4c9c125f018e72191d", 1921),("18a7031bbad9ad8f5db596946c893c660abd4d431f3bd9ad3ec287cd000cbeb1", 948),("90d9c31a37abea26b61b2353196acf3232fb11a7c99a52b3dfc587300321dfdf", 845),("00f2f41b11b156245d2aac2dd87de60776204ce6186a6649423f63792df112e8", 1646),("85d95e71fa0e630cc0cd3e44546f5f8cf2291d6f8afe67af1ec61c0a88a9cfc4", 1980),("c60b9a71620abbf10f2e8733b03bd70af49ebcb385052dff8a70adf8759d52c1", 122),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1531),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 945),("da24f12adb47c0dbb6c60ee3c4d6c58a65e7d3ca436d90610dd74eb51329b184", 414),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 1720),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1551),("360907217814bae548fc4fb596a8b0f9f7e31f3555edad4dbcf9d892c972e92a", 686),("7275235f9fc85dfd87765fd89dc7b5f59cd406968d0e0a6ce8891d21d58cef76", 1493),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 682),("4624c5bea2c45753e504df76a9eeb8509ea471ad6b2f6f30d114af22ac31d204", 535),("53c829762875940d60ee039faf6115a3cf379ff991a12cb8ab8e6db0f6424a20", 568),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1863),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 852),("276151050d1d0c08bcc9cd77ba22d220ab46f690477d73694c26ec3a7781d82a", 1830),("8cb5df48a9acf983571b87832d5e4533fe769bc7ec0b4f7c0dc5c90d333b0142", 1029),("ff0ff344e58069dc12f67a8b1ef1a498fcb8a5adb0918486ca582d3ae9926656", 1527),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 1017),("4f792d188451a27336b55111702fc046a71ec2a9940fc4953c131a39baba09ff", 197),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 262),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 258),("f61e15cdcaf0325bbaeb9a23a9f49d5447b33e6feee9763c2fdfe3a986142912", 1338),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 1339),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 1252),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 205),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 10),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1377),("d3384e49afb0cb561c38190ea6df983161591704625d5688b86ea2692df6b9ab", 1009),("ac48165a4f600fc2ed684dcc24182cf66f969da19c32526a9d36ea6a64988068", 1552),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 673),("09e3ca1d8f6231dfe26850103546583d20759a046b7fcef05d392ee01699b8de", 654),("857a225fbdc9bd9a193be0026abd820d2403353d7f59c7a1d4fa193baa419dd4", 230),("e09c8d5ad3bc4c94546df0124ca26ad1439ed05dba322389fe3b222bcfff3136", 1610),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1489),("a49b3634402f3579fcabe5619ba31e5df23936498aab48f4177f7de2e4ff4be7", 511),("1ed14807230b7eace71620b8594f7e9eab3a948d90ecd5ff7c01c7e0ec4b34b1", 938),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 734),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1185),("d7b0efc67fc8281e505b65861cea03b90c5c9ed17367503ad95d9398f08b9995", 466),("4663c4e81c59780ea6c074d9708c1b559e32865ced3cb9e608625780cde636aa", 1308),("b092c864faa50648b87dbb3ee868a792700e8b0dd19e45607d29133335d842f1", 1096),("0f9b09909e429de3ab1a25ce1792fa5abce7ceda7ce6a005365515be81c68192", 464),("8b5965ade4481f65323d495f693148f7ae27d48ea3d620f2a244be42b6f6415d", 1715),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 1401),("46fc5cdd21f0b9f14b40383a47b5c0930788fdb4018c917b0a1716b83670870d", 1021),("b97a3ffff2662f52aafff4908805941b265afd8b9cfb1fdecee8101483269e32", 698),("4223b92d2cf8bbf4ae96fc35f8dbb7b3d953a61b43ec29865b9d8d7a94120f93", 1451),("b74b2380ff90de9d91e64dea8c7db4339e732044d56c2cade9fe55fb2a79acf4", 1581),("6fde90e205e782e27b3d45fce30af531afd652d1e4b5e4e02e2ed44269cc4769", 971),("e913cb9bc367230f2dbafb8623d9e03516aa40dc68dd5ecce9805f35717db66a", 754),("acc899f0d9ea43ede30547e302e5fca7d0cdf6b1e087f19f496c7dfc55fe175d", 1290),("7ba18d318a52d0adbde26ac3b80c3e685c26de4268218c0f879d0890e4ac5baa", 181),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 1842),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 289),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1202),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1945),("ac48165a4f600fc2ed684dcc24182cf66f969da19c32526a9d36ea6a64988068", 1205),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1180),("591946bc18a9b3dd788d9858a7e4cce0a4f85ec484c2b67ca74c79a52374a0a3", 1269),("e040da9d2fcdebfa3b1ea3bbb6ca8eed614421e0e345eb790172c6b47d674680", 976),("0ab0470f1e57abdac680f2610e5cf770b1dd1f29aff40dbc6b7a7446c31e0260", 1932),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 1650),("53c829762875940d60ee039faf6115a3cf379ff991a12cb8ab8e6db0f6424a20", 1227),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 299),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 432),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1523),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1826),("bbd103c777b5ad50de82d049c1041e5516a79b9aec94a6365e4bded62ea209a4", 1399),("c3933aca077011853185b79de2ca6d6787da15a8cb8bc40b1d6a959b566b17eb", 174),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 497),("32d00ddeb59f42481a382df21b035ac00002cbf6a24bb04138c8a2670b73adbc", 747),("ff021988dc4d4fc4b8c33c9143b781824453efb21c709854539d62e918e230f7", 1930),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 199),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1176),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 559),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1896),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 661),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 790),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1405),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 1692),("3246fd0c1eaec63454069230c275d4ad7a8581683793f30fbcdcc6cc14c84c06", 1093),("65c5f34a3a20d4934272ac64fc48f7791224224c7c0b9166f41b943be8697ad9", 1537),("60a58226cc9e1425e41d6f68ed9f4695c0dcb20a4087240220e41bac111ad696", 104),("8fc6c4888f9c103a577f2b812676022510385603d373b0e639fd6aa1133c4463", 904),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 1434),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 1819),("12cb34d03f40d2e9073baafe2a0cee0130cce96c8f4cab3f681a904ada68eaf4", 252),("42527b1172e1c9551a333d73662af2b7a13c8574bf8d112f987bc60e69ab6e8b", 64),("641e4601d1002b2535916af90bc5c53cf58639f8108d8064046241c8e327000d", 1310),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1088),("df531e38324b8aed256e529eff465fdba43058c29500bb93cced66daa7db3e7c", 1297),("a6e48a8939bcf9196c44f75cbc6d291b237fb1f333ff7a5d02ab8b28141085bb", 1918),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 1690),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 843),("4b13222cca9dbcc157400ea4b62a41c164e3f404250d32cd32b08e0c768d38aa", 101),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 308),("e1d8cd8f6429b321ae3a4ee5cd7d66779e80595f74a65d760295a9c9fbd276b7", 769),("ba88258d4b92a041c5dec03e9654c92c3f58213fef6451d70d9b55f664e55ad0", 680),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1067),("f2f1eaed7b0a813dc222158a0a68f2122d421acb0bc47c103648d178397d6fe5", 277),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1480),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 632),("364d679ebeca06dcc13f3d162baeb717ed42c68bdb587401f2104930bd1cd823", 477),("41bd33b76e8287b5f474d76e3c3e28a39834d61db3fd49046fd49a653edc4794", 213),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1977),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 1828),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 932),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 619),("d24e01491766b6fe9ff78860af3642df3e80748a0db6fcd843d1022dce274fc2", 1843),("ed8524bc916001cbc7b1e1b9e5c75b99906084352589e28940d3581e7e5562b1", 1947),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1446),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 935),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 954),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 985),("60a58226cc9e1425e41d6f68ed9f4695c0dcb20a4087240220e41bac111ad696", 1976),("6f7b1ed97f29c19850f2798dbabd1a2bd3eefce14fc95e4c9c125f018e72191d", 1656),("cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e", 776),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1759),("ea539175b7730c14ddec5b2ef70378f84b3c91ab8e58eba6d00c4ed4f63f694b", 1731),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1536),("f8cff2c80d272da5f44d2b2244f46d0b27b7d6e50882f204dfe65566df14bb1c", 279),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 475),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 98),("31c9c22db1e1eec9e1534f2dea360c3b666d4a9c183319b2a319743bcb6d1810", 242),("8d5f8f2d76dd4117e1ca152d4480e2a618cac22c9660ed8c931f510e814ccff7", 1292),("9dac022d56d6759155f8a613488d400fbbb93bb9a4920cdd943b1214ec5dd116", 711),("775d474c99cb81177af6773905bd2f5b6e29a7846a09f1821141790423f265b6", 503),("81537cf105cf802521bca09a93eb555a685f8f1802f1d794a427983e3e4203a3", 1421),("b20365ac0bf13c220aa981188ff4683fc6b9c8839abe8b1e577039a5b74bf7cb", 1058),("8fc6c4888f9c103a577f2b812676022510385603d373b0e639fd6aa1133c4463", 504),("95cb93df8e2b200b16e5956a198be8b03a0d04d4c436edd67309350755b1b4d2", 1478),("30f629db352d4bf138b31911ac3e3e56d5c6b16796225d9948c1639eaa24f079", 16),("162cebbbfd207306b46adddc9e6a0e02b31d47b1ffa2a1072352d3de66c9dacd", 1812),("f9c5690d45942816e4979bd43b49a56a0083c13574c3c9e619f261917c8ea1b2", 636),("a50b963449a6e8ed4473912013c0dadf08eec9cd80381286fb5323d349292f3e", 471),("a3c3e51da1f347966b407f0804a6f71f66210967d2bd4b32084931dcaa475e11", 923),("951fd0ae99c7148f5d0c9f407e1d6ebe2b71a960ba7b12946064892b104d881b", 130),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 490),("7ea072b3b8eb85931b09848b58bc4930db1fd8207cfec9e8537eb5edb1f66cc9", 169),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 2006),("a0e801bfd8fa33e714e09cca2ad01dc72d36df775879c4950607fc103e19a885", 1355),("71436a82564ac44eeba8145eb2a85b8e55ab94b3305f1e97831a73008cff6d53", 463),("3b4d5eb7169d680443ed9fcf0d4072c2a0681d364b53cc788ca03a1a37353966", 1340),("d8c14588f95cd4f38bb01382db396b0e44e66e1ad20e5b27db2722d6ab52523e", 1899),("68d08ba041c3ff22268f003e26d35ee9020db7312909cb7f7e39e039981badc7", 1402),("b5de44514088a08f608e3c5ea39b48d92a13aec53b74a5eeb2d4f3e0ac83458f", 1081),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 274),("670feef8245beaf7182780de0b34fe85836bd2f891160e99638b09317d8c853d", 1352),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 1522),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1303),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 1274),("e4b97cd3d5071d9e7cfdbf42bd496d1e5850afe2ee92f1a16db61c99083a639d", 1235),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 176),("d6efd493dbac5f808e9ffdcba4ca27caabfb60256f8483e659bb764207d3e05b", 145),("1a1ec2c7fa352c4498abc335b8fd92fdccb1b5950cde740eacfc257fb07713f1", 53),("e4ee65fd997382da80c771a976ac13f9828e195be1f92cb0620526bc38f09760", 1069),("2ce68c9a533593c49c64c89507390e7e2e6f60dfcb4f2a199b41b346456d0402", 1985),("dc9314868376c10356121d425780bd2ee57f448157d8a33c4114aecb697ddd9b", 1285),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1412),("f859a28b9a145aa220a663a3651781b11e6cb0eb91dea4102898a691797d6e02", 1006),("6e4c2073ffa7d3555eb794376f03056f5a14ce347c211d5592b2285c5066b334", 802),("ce574886c452658382390f9ebe64b785ba7ee5e6af0bf463714bd6993b1623d3", 1244),("b8405d1964a35f12c00414b7bca78e216b9d0d65f9194f6b600bad5340a323df", 103),("af9d1bbd36defc07ea0b73e50722926de4e185707018f794bc9f23393eb74900", 2),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 542),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1653),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 17),("ab8f4f9e1c720e220693d6a60274fbc50858b53a33b85014821a7c810e24fad4", 446),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 50),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1007),("8d5f8f2d76dd4117e1ca152d4480e2a618cac22c9660ed8c931f510e814ccff7", 1068),("3b4d5eb7169d680443ed9fcf0d4072c2a0681d364b53cc788ca03a1a37353966", 243),("88b58da24366d5b18fed3910b61c3d6e779d72854c60100b5310ad30fc03e6a1", 1739),("5413567d5af78eb1dc0da87d0be68d156e6652de700d2661a8d41268010ff6d2", 384),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 757),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1467),("e1d8cd8f6429b321ae3a4ee5cd7d66779e80595f74a65d760295a9c9fbd276b7", 483),("961ed8301afaf93ee4d8a95808eff66e1f28a4821317e3287bbcfbc963d95f23", 59),("38135e574c9d6c91f80060b56666f903e8afba60d14c464e95ceb042d48af9cc", 926),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 861),("6f7b1ed97f29c19850f2798dbabd1a2bd3eefce14fc95e4c9c125f018e72191d", 1221),("93f39d1a5df51e3696b742f6e18c3db037b946b06142db9a4194d5d07ed3bee3", 824),("37dbcd84143ef13fe061297d5c57db4248176e6c3c11220aa26eba53c7bde969", 195),("495f5ce737bc3824b1a4bf13076262102b2a1f99fc70379e15c5e14e4903f3fc", 1962),("36117a4102b241effedeaaf3b7757967f994ec9fdfca1217b307716891ecdad7", 663),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 787),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 244),("670852640878fe777c5ef024bbfe4c022faf58893a3aeb1e84fcb1df5026a1df", 1000),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 748),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 1107),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1555),("70927afdde8633d1bd8f3a71fbe1590c57a332025b27f8a94e3de126dfb9213f", 903),("8fc6c4888f9c103a577f2b812676022510385603d373b0e639fd6aa1133c4463", 1815),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 144),("dbb2cb3000fcbfd1ff337ddc5b5851f41734a5d17c63b67746c9462747f6f4e6", 545),("ac48165a4f600fc2ed684dcc24182cf66f969da19c32526a9d36ea6a64988068", 873),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 1497),("4e70293429275c643f12c5c470681432a6dfb9c178361ca38799af3a34d2add8", 1388),("cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e", 1778),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 1063),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 640),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 1041),("7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3", 813),("5a826202247c89768798b7a6b41aa21146f9133ed3cf4dca7601c7be10c1c1e8", 883),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 1613),("129156f08ed2b6da3be2ec0269fb24daa4156908fbf4ab6e6f4d00d6a6cdce4c", 744),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1373),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1226),("0a07df8b0b4a94ccd0a7a2cfed6b35bc45a515457d5bdd016e4b452bd7e4d2c9", 481),("ff18b8930fa0053b61402a4ae5fd68dbbff9888bc5afdbdce2a73c7dfb95c8c4", 183),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1836),("2c9e74c0f039bb4c217c3d070d4121c6b7daac518b76cc34c500e82deb81e5b5", 1655),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 430),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 70),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 45),("8911e50477db04ee96be81130a226f11aaced4adae3f46db09768a7b9c327469", 267),("9cb0db639d8bccb930814df5e31f0a7bbd10b95d57d7311589bf9ba3cadabeb8", 399),("26a86bd1ee718e81218d9b029a5df7958021a4ea1dc3df871acf6f23b9d23004", 353),("c3edcd6119745bfb853354ec27aebeb1745099dd7f6ac1e6855681e1558fffb8", 1494),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 421),("a49b3634402f3579fcabe5619ba31e5df23936498aab48f4177f7de2e4ff4be7", 778),("ef73650d04258cd780c3bcfa892ac72b2184fd2badf7c0c6a0046e9ecaae5ae7", 1194),("423bec3ffdae663a75e9e96a33aabd7369323f88b599eaba11cd662e6b53682e", 1238),("775d474c99cb81177af6773905bd2f5b6e29a7846a09f1821141790423f265b6", 119),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 1809),("0c16f57c0dcbe63bee210ab9faacd41f1817d9f19ff0cf283aa482ed9e0a7f56", 963),("6ecdb5bd9ad50391682a2160774fb66fceeb10934f752e489bcacf6a053ac5d0", 728),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 857),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1283),("5cdc03ac412954d96d5556220b61024d2b163ffbf21b75b6ac9c104da313d862", 107),("c72db1bcf9b1ce3aae024560a28340903d8b19efd86184ff51c4ce62479cb660", 1305),("6874630a622156a15c0deaa98b083b9fefdcaeb060a92c01ea4cdb99189e7149", 139),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1360),("2c9e74c0f039bb4c217c3d070d4121c6b7daac518b76cc34c500e82deb81e5b5", 608),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 610),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 798),("379222393514b2e170164b02ce8fc87d5907dda5a27e4e0ff8aa97de516c44da", 863),("f40194ff972c0753b9e9bda596ece95faf91c821214c8882240d6315d2cc42a8", 1140),("3b4d5eb7169d680443ed9fcf0d4072c2a0681d364b53cc788ca03a1a37353966", 1622),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 1457),("7ada07a0a64bff17b8e057b0d51a21e376c76607a16da88cd3f75656bc6b5b0b", 911),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 1438),("495f5ce737bc3824b1a4bf13076262102b2a1f99fc70379e15c5e14e4903f3fc", 1993),("71e5e40351a1f300f5b1e7ca10f0ae46d9b4558ab40a29a0c28e659f3c75d1af", 1204),("8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3", 597),("16e876522f46df4e847e4298d008a531739860edb29523068a78f0bac32297db", 1861),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 719),("cf34217e1151dd82dba51f02f2bb341bcd6fefb15c87d470764b5b819eb3154a", 699),("0cfb359661b97ec1704640795a7e9cfed7361635215df0c0a129fe8a8da44fd4", 1231),("19e73e555c3dbe1b60efb37bcd6ca00a628dc12d4b692ba313317d82a1ff727b", 826),("ea539175b7730c14ddec5b2ef70378f84b3c91ab8e58eba6d00c4ed4f63f694b", 656),("ab8f4f9e1c720e220693d6a60274fbc50858b53a33b85014821a7c810e24fad4", 1503),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 627),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 39),("f8dc2d17deb2c327c13968d4c1f730f9933ac4fec188c18daea537f976ba5452", 761),("b815d002304d553477487dd48e7e57803bbb5bde51720d191f3e13f6bb79300e", 1849),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1619),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 440),("d1d4f9ca32d08924b9dd837d11a1751af844fca49971c99619abe665a7e1e7e0", 1084),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 336),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1329),("38317edea5f41a8413cfa0d2457e788e71d1979335c8519b63da27fdfa315aec", 1210),("d2e9bd9e9cebb3c85c8c0dd9d5370cf86cff70404f37464ff6dd89e6e1a3682d", 712),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 840),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1785),("5f1f28a589c8a4158327cdc091319d4ca6cfeffc833e493e344f8d4556e0d2c1", 1293),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1967),("b24e6fe6c7e677ac697a28cb782cf4c018de46528b4bdbfc93b718c38b4fa255", 1403),("56009b67648295be0719abcfe01ba5dbf62b366c9dd4123863778cb5dfcbdbb0", 738),("41bd33b76e8287b5f474d76e3c3e28a39834d61db3fd49046fd49a653edc4794", 1887),("775d474c99cb81177af6773905bd2f5b6e29a7846a09f1821141790423f265b6", 87),("7131ae3ba595c77b2ffe1c19ee03df39d0dec387bafc22415c53cad2394080ba", 1857),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 1892),("1cf6d43dea93271d4242d25309d5e77cafcdf4a0e7a21a352a5b8daa077b5994", 1575),("65cba732a9daf594b07cbecfb82aca329628e071de78fe9ee75ba31749aa78fe", 1550),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 254),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 1649),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1135),("4db213a4c511a635235da06a60bd13d20ca1206dfe5ba7ee1c147cff0acae43a", 1906),("e7b55b0cbd73aa180515560fdd59d435d05cc51cc1f97bfede8a507b435aa3b7", 1496),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 1738),("2d065e6a6370d8611032c332ebdb7709eb49a458b833d3009adeee65837e229c", 1331),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1314),("d37a2dbd3c0cd1030f1ce6abc83b8cdf8e9f7684a0cbe37e9bd941eb4e96b410", 1020),("d55e56c6cf896ddbfd2b33d06f9a3fcf4e48f0c184aac990f723a111361b16e8", 1458),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1937),("70954a4b30dddf56ba4abbfa56dee64f4ea86d25155c4aa0689a544668b83c2a", 76),("26a86bd1ee718e81218d9b029a5df7958021a4ea1dc3df871acf6f23b9d23004", 1192),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 1039),("b434421e4f651038f4603ae329a61fcd8a115e53d93fb7014a0d333d02dc53d2", 1978),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 302),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 157),("5ee1f41d2a09f19767cc0360c147bcf66bb71c79364f66e0def964ec5d2d17ac", 1717),("cac771cb1214125f7e261b7b480a124a05c15c36818636724d639fc60dc8cd89", 245),("c8e7f3d0f4b244d4a4e46b91af87fe29e05859f458d41684f8cc32a93fd2e037", 315),("68d08ba041c3ff22268f003e26d35ee9020db7312909cb7f7e39e039981badc7", 1627),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1633),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 1681),("028cbd50e8d3cccd20183c50d16a8cb770334d9e7c980ce3d5499000cb05ed52", 1413),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 500),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 837),("08a8f7e278b0e210a70c35be3254c3ca4d1cf2e983974ba33d7f61fb84277c50", 763),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1777),("9d5161e9f6cf8defae51046dd8dabe473bfde429474e33e126689d93b7ac27f8", 1165),("a2ff3d5acac75d8bc2e22af2139e417012c6783c0f039e9bc07c74f7a9cb1c93", 961),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 334),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 27),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 186),("423bec3ffdae663a75e9e96a33aabd7369323f88b599eaba11cd662e6b53682e", 574),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 171),("9cf1cfb010ea423953d0cdb1dfbd6211e76876a8393f4fa81752df9baaeb13b2", 888),("38135e574c9d6c91f80060b56666f903e8afba60d14c464e95ceb042d48af9cc", 1877),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 412),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 1452),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1037),("541b41cf0e3050e6420a080bd3bdaa1dc1a270e97fd3be1ebae9eef400424181", 1330),("dc89f749a66a430224cca1270fc132861fb0634eeddce1ef16aa8a818881b6db", 1800),("85a3319f6c185efb8fd30d4b0178730fc1ea11713268bc262ac85b717f9b909d", 580),("b434421e4f651038f4603ae329a61fcd8a115e53d93fb7014a0d333d02dc53d2", 943),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 811),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 132),("92d18dc83b424db46675fae75704fd16885e3ddfadb827f88505a84fb061fc8f", 1858),("d989e0edbaaace9e9f5d7b6f1eeaeb3243a0944cd2446bbf2c64434eb7a215b7", 11),("9dac022d56d6759155f8a613488d400fbbb93bb9a4920cdd943b1214ec5dd116", 160),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1026),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 783),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1982),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 240),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 702),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1712),("7e469c13219435d22d463a3bbfe3203de453a60abac0c0cc03f5065ebd845cbc", 727),("99288e3d69ed59840c19fbe6a201d74f6261d57ef6c20edde62c008a1151ca99", 60),("4a9e28c7fe343d82d2d31fcb17015d600466091e95aaf716d943e2f5e5f08d6d", 300),("d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d", 41),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 9),("e75001b78170959550e7943ff987163f9dc164bd88626855abba578befe08fd2", 1549),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1817),("26a86bd1ee718e81218d9b029a5df7958021a4ea1dc3df871acf6f23b9d23004", 859),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1389),("5ee1f41d2a09f19767cc0360c147bcf66bb71c79364f66e0def964ec5d2d17ac", 1936),("7ed274485973a7f234f190ec30b7553b39a39f3e888b5b5fd7766f15730c2575", 136),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 740),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 1396),("59c2d99eeee43761850629e5cbc00f897b0bf039f29536aa0f066ddddd858108", 1162),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 269),("9295880fbb9850a362e5244404b1f4e7f0124e916633162a5086a9b2b0428d63", 6),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 739),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 261),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1511),("becad565708efcbeafa2919193792642d94f5dd64428223b5b670f35d35284e1", 1734),("b372d89e856f1ff2b1313b633ab25526b9024057ca422454f7c226f2cc8e914d", 1307),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 318),("21c601f9be1e116f80b00b68420fb5d2c94e176b823492b44e201ded4da21439", 667),("6d374eeae46a5264a62c485172ce2654259f5385e7d8e203246a48170a0e8dea", 725),("b47601fffb6952129d143a1ade778dc477004e76617f160a9493b8acb480f981", 156),("dfc4c3d01dbf34c8e7bb2fb74eeafbaf3893a14e2f2b6374de397b920da4950e", 1975),("ce1ca66fc3a4026587c8f893cbbc6c84957d68586a1df0dcb632780acee8699a", 604),("ba88258d4b92a041c5dec03e9654c92c3f58213fef6451d70d9b55f664e55ad0", 1233),("4b314e1341fa94effd6ecc5ca77b56aa281c54552b36c331c86e9b034fb5848a", 1095),("6d139f4193f3c17d654afde57869ba8e53ee4e70141e3d0cd3f1bfe80c279ce3", 218),("c43278d9070d0b78377a05ec89f240931fca99df4f3a81441b3f1638e9d858a3", 1219),("e7b55b0cbd73aa180515560fdd59d435d05cc51cc1f97bfede8a507b435aa3b7", 1207),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 847),("62de7855eda19f557978c85befb72391bdd31dd859b73d9dda6e3512f4476801", 435),("bbd103c777b5ad50de82d049c1041e5516a79b9aec94a6365e4bded62ea209a4", 5),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 375),("436b3f668cab2b3aeb85b67c27e080d14fd8be541c87af8bd9bb9b71aa1d29d5", 1242),("234eac2a7b55eb5a2c8c535e6872e8e4b780b6b691c2acef8fb0684086b58fc1", 469),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 1983),("b36fdfc65baa3c4c88650d13fee1622dace79cde6a5a6bfdd380d1eb3cffe9e3", 1543),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 1356),("41de038d20227fa6bb6f21aa00da8e718c1c7930d4db81a5a0ccca258ee6f3ee", 606),("3de6767744ee1fbc5ec0a6fecd8fef2becbf536d92861422cf78976d82b3dea8", 872),("d57e0e4225ce7c3c79288f85ac8f7c741ddeb98421bfccc0e18c1a15b1e0c7e1", 617),("5141ee1571633f74a7926ddedec3ae8049c6e208b2f8800022914756ad4780a6", 986),("cc818989e3891971c7ab80a8106265c379b2b228d32295526e928eb4b4c9c86c", 1151),("300276876905c4c6fcdbe64b103420a96d5eaf2bd2b3045216369888089e7a81", 551),("3846cfd5846bbcc675a3ce922f3c2d5cbbb9dadb6a6749c2301ff26c10f9e553", 1079),("e45fb4396fb77f7366e63f7eef3fd063df11984769e802d4f755856346101e33", 23),("6cd96a7cb0d753da32d62c38b0bb9c59e2102300bb3bf7b34f20d1a86abec653", 102),("ab8f4f9e1c720e220693d6a60274fbc50858b53a33b85014821a7c810e24fad4", 1387),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 126),("a2457c6056d20ceb73e118ebe68ac98da3a179f13c68dfaf73676211c11565c6", 443),("c991a7886006a1ef23db2251d25013f5ece994ce1ae20b05e2dab31e9f3e2365", 1239),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 33),("3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e", 714),("640ea7b2025bd746cb214a61786de904bd40fd3730a05e34090f1f87866cdb68", 1138),("d55e56c6cf896ddbfd2b33d06f9a3fcf4e48f0c184aac990f723a111361b16e8", 1361),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 1590),("65c5f34a3a20d4934272ac64fc48f7791224224c7c0b9166f41b943be8697ad9", 1573),("c43278d9070d0b78377a05ec89f240931fca99df4f3a81441b3f1638e9d858a3", 519),("15f742473c793cd219bfe3291dcc3cc3d6050084ce7f101b523c191cebf22bbf", 1680),("292dc574dccfb127860008f35763b9aa2bf4b35046353d71108ba7490da5ca82", 1953),("178469345a47d70aaeea49189a814467c3a8f740db88c19d6fc1c4d2867dc235", 1003),("4a9f5a4edc2cc10d2680e70e9d9c574bbc35a2c70fb7c1a2f7f0b2aab2dadf47", 964),("eef7205ec54e9405bf6dc9661ff913bb02418132a0143890f66bd823b49eb290", 577),("e1d3e659d9c9174757db14802bb27bee70b44a3964ae4d490e167fa78b4aaef9", 344),("08a8f7e278b0e210a70c35be3254c3ca4d1cf2e983974ba33d7f61fb84277c50", 1642),("76edaadc2a472e2a53e8994f6e318c1e88065c043d4687a9c70942b281ae147b", 2005),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 431),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1728),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1193),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1109),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 80),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 465),("22239b378a431648ae42ea83274870fd7cfeaa77cc745822681f1132412fa9ea", 20),("d7ed67ecf402312d8751b0e4f8e4e2dd973bdd09e9967a404528bdaad862ba1e", 1441),("ff021988dc4d4fc4b8c33c9143b781824453efb21c709854539d62e918e230f7", 1262),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 212),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1934),("d50837a232e4e516d9f58b3e801b54de4d12fd6f4f5538c640be5c1f88107bd6", 1766),("bbd103c777b5ad50de82d049c1041e5516a79b9aec94a6365e4bded62ea209a4", 337),("6bf2b451fa42739f9e764d2bad7caef7cbd0f4696b49fd77819a6eb55f1a2e30", 83),("781cca0b42bf1fbdbb01fe1bee0a2679b298de00c9a813471ee1aaf45db51c07", 1754),("3172d3c4cae7c53989efa1a9b856bbbff3ccc1ec014259209c7ddce5a4784ba7", 973),("98eb72fa60820f221add424a91b0f7192631a67a1e05d6605bb16fa52ba77273", 949),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 775),("c166fa7b9bd9e858baccac2c10e4fb1ebb14be8f17760e7d266b4983affc435a", 669),("ded8010a8f2bcae0be67544d9fb369609cbf8fa527d1ba2ec3108faadaabdc1f", 405),("3a1ee5d046ef68b89e4e39a6f11c67faa4f62a60e0fa01a05348f86c36683635", 1099),("eff83341e00950b5fc55ba22d6b48584c0d65e196fe7ca6863ae68776010d79a", 1282),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 68),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 955),("bdda500d2e06cf06d452ce9476bddc8d85f678687c022bbc47e988cfa4e734ca", 1986),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 862),("fa8efd699b2028670edafec56519f9f21dae259731227cbfd81d14ad40326a3e", 1008),("b0209ff50d8a0d3b2500db74cf81d1e2ef3a47196ba6dc28148ec5878f53fbe5", 684),("47d6d19264efe042779557cc6ed17e0b2731131b667237ebe63b3678c0547882", 724),("7a10a4d3da8081f7234845ed153cdb477e10bd6294cd74bc75b04399c123d99e", 1145),("ce1ca66fc3a4026587c8f893cbbc6c84957d68586a1df0dcb632780acee8699a", 1197),("601286fddf2a5e8ece8c62345536167b739edbcdecc637e0862e4f49de83d092", 875),("9f45c84e0a81fdb5b39d47a5c3e54d90608506043f4488a236dd7ab8cc128f35", 402),("904826bd329d037d3fcf38e7eb54295cca9f5c68b174c3c750ea5205254ff6bf", 1913),("47d6d19264efe042779557cc6ed17e0b2731131b667237ebe63b3678c0547882", 1271),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 1963),("857a225fbdc9bd9a193be0026abd820d2403353d7f59c7a1d4fa193baa419dd4", 759),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1364),("db09eaba6a0d3e3ce7f029e49518ceee6e533e5fc47677de1b839ae59757467d", 1475),("2b01f833f0a7a1b0524478a26511e4dfd018d438c671b3b850fed12aa0b8f47a", 1175),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 138),("cc3b948fe47bc42ed959228f4884a59eabd7a9a1d06fdc2d80405bae46b70960", 385),("ab755cef00f9c034221806160a7fcd4cd712720eb9c9b1960475e3068ef2b494", 1883),("4a3d951f0e419992f40a281b48459b368d0df0dd22eccd8020f12fd94f36ad98", 1723),("162cebbbfd207306b46adddc9e6a0e02b31d47b1ffa2a1072352d3de66c9dacd", 918),("2bdcaebf7846e5f55e52edb886da929aebd9c1d3e31fd81eb5784cf8ce20d6d5", 12),("5e4b483e6293f98fc08bacde48e3b7e1bfa55b95861de2498d202f944bdf9b32", 1323),("31c9c22db1e1eec9e1534f2dea360c3b666d4a9c183319b2a319743bcb6d1810", 1002),("8911e50477db04ee96be81130a226f11aaced4adae3f46db09768a7b9c327469", 362),("e4b97cd3d5071d9e7cfdbf42bd496d1e5850afe2ee92f1a16db61c99083a639d", 48),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 1572),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 196),("7d1c7b9289d44ecb475a1c0a0367289fade34934d0a1ee7299abc7847c4097c5", 1719),("6927db2edd9b0f20e1de53eb52697cf5620abe1ff8111beb02282710b065bd9a", 251),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1050),("212680235551887e6f2ea1b79a504c19b83f0855e9fc78c4b8ee94b460990786", 1659),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1259),("350653e8cba6a4a6a408e973fda85b7fac55d1a372d875a9b8fc361cd4202bd4", 846),("bfce376bbebcca2987befdd0a787205d496e3650801111e8ad110dbcc4c5a41b", 1641),("70927afdde8633d1bd8f3a71fbe1590c57a332025b27f8a94e3de126dfb9213f", 7),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1574),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1146),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 1969),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 578),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 180),("a2ff3d5acac75d8bc2e22af2139e417012c6783c0f039e9bc07c74f7a9cb1c93", 1150),("4ae7f96e7df3009d9356531ddca3a5c9bd4d2d55fce67c9c8c3215b8b1921424", 916),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1950),("a0d1478a422143e4c27e92724f4dfc8a3d08314e57de528e8d669972edb1386c", 193),("30d08d681dec813f78f736f3e9d1b5894d8fb1009d88ab6ac4af0cafa2df95f9", 1062),("97af6e0da6e8a25aca9788a474c5dbcd7f7d5b9026e6b1ab5cc81e23eb684c85", 1513),("59851c5fdc4e478add558604e37b0b0dc1babda4007eae00a28b35057334519d", 227),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 1212),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 1241),("5d43b0b8b3a236e847079fd8c729ee4e17808bfd8a9f4b31b2c11ae3da3a1c31", 1534),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 834),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1965),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 1134),("a27b065628d4c85665319f73e2eb2a53f79dccfad96d9c256c98c73e2d284c50", 1793),("6d157e4a76e43cca93450c7954873c3b34ac0473c5f102eb2e6255349611ed95", 1348),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 493),("85de60661b4ebffad9ab6db5c0da516291d0d80a402bbbcd1042a041bd96465d", 52),("e9c9f5c581fe1440aed68dfa70b79db8ee7632e5c6d872b32e185e4256232c9a", 631),("ff18b8930fa0053b61402a4ae5fd68dbbff9888bc5afdbdce2a73c7dfb95c8c4", 234),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 1089),("b76f6029c0000c88950abd47beffc911e2d3ed7eecbfc21790bacfe826b95e2a", 133),("3ae5f4f296874c82a626cc0ee6cae31d608489e4ac25622a37117a629a81cfad", 593),("e43b94ccba7e4b61a5cf93d9ea9db93dc8a65c202a42b3dd49b6f45bf31f5f2f", 1400),("e5f5fabcc4101cdd4a3e9e74b6ad974a3a7cfe82aaa8d7a5b1a60f9bfd725947", 120),("8cb5df48a9acf983571b87832d5e4533fe769bc7ec0b4f7c0dc5c90d333b0142", 1430),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 1767),("bfa6f3d2573dd1380b922b571770efe934e119490902697fed0cc07ff233dba8", 997),("0403ee5bf4aad18db5c378479e1fa028cf0323ec1f381191e53f5a9fd2027778", 293),("d877c19bc0bf1bbe76f19da30eb1cc58646f6d055f73331d5678f6a1705466e9", 1477),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 1214),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1036),("f2730be5d2bfc4e039c74b8e9c9ff38a37e3fd4639c695f12282d98e48e1ba98", 239),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1750),("1cb09132aca235c098cf6fc6939833b2ea3541ceb5d252f2615d197aca6379ec", 1862),("7b82dbafd125dce422a54fe9ba66c35d3df169d6f686ff79d077e5a9ce0f2ad2", 32),("c309eac442c74b7a83c88067b1c7dd5cf85ffa7b6c841c10736d76053ebc2e67", 804),("1e29a5456b345f0b6bd5f80aedd056782ef8635cc28d3f38413edabf7c2fdd29", 1133),("1583f7fc13601a37caa879ef441e87a9c258818b3a5a4aea488cac8112ea6112", 1559),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 647),("89608f311b29030f940ca52e77c170b1c68b7abf2d62a319e2beb241307470e4", 214),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 1658),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1015),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 151),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 1040),("7f5a3ec3d9e24445e0ae66348a9ecab34cde0f43c7747a5532f01e0f66c0bc43", 71),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 921),("6d139f4193f3c17d654afde57869ba8e53ee4e70141e3d0cd3f1bfe80c279ce3", 1112),("ff021988dc4d4fc4b8c33c9143b781824453efb21c709854539d62e918e230f7", 1049),("fc6bb5cc654dcfdc33e8a6af472bcb25015af5b17b80ef35be9714e866bab8c1", 373),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 618),("c60b9a71620abbf10f2e8733b03bd70af49ebcb385052dff8a70adf8759d52c1", 1481),("7a194ae2bfd10f7686b8236995448a0995ecde537d3fcd357711ae4d894bc51a", 1810),("73c57c23f3bbea7ea2c649a25704a038314d02005a90a75daac991ffa3536b90", 1755),("c39565e0dd3eff00a4c9c07a488b3ab392883bb1d7be994ef5d5ba3ca786eade", 137),("e4f9437731f17a1ced0550542b2b380f176ce0bc8c0449623770e08819e82d46", 933),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 1066),("5083135838385ba28cc4c6614674d0aaab070969cc8a8b9937482bd5d4ae16ce", 1856),("85de60661b4ebffad9ab6db5c0da516291d0d80a402bbbcd1042a041bd96465d", 655),("fb8220e13a4dd11ca62257489ee521834c58cf070de5e8ee4effd87d733863ff", 1662),("7e469c13219435d22d463a3bbfe3203de453a60abac0c0cc03f5065ebd845cbc", 893),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 1760),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 1661),("b4be27cac04a4e2b2e90ca2f55e033092edf53a67a7ed228044eddb3726eb24f", 476),("a17fec45df36c7cc45a13a2754e2ab7c7a35a179f1511d44b30753646de69959", 1749),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 599),("fa8efd699b2028670edafec56519f9f21dae259731227cbfd81d14ad40326a3e", 424),("6d139f4193f3c17d654afde57869ba8e53ee4e70141e3d0cd3f1bfe80c279ce3", 1510),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 474),("2365f82ded2845748c2c2706cdb44963238ae33031dd326c89d5a334e91d35e6", 555),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 38),("961ed8301afaf93ee4d8a95808eff66e1f28a4821317e3287bbcfbc963d95f23", 1867),("c9050b4884be2b9045be8cbb4157b19160775cad17dd9641df0c7740c7815626", 1639),("8d5f8f2d76dd4117e1ca152d4480e2a618cac22c9660ed8c931f510e814ccff7", 1386),("961ed8301afaf93ee4d8a95808eff66e1f28a4821317e3287bbcfbc963d95f23", 415),("30d08d681dec813f78f736f3e9d1b5894d8fb1009d88ab6ac4af0cafa2df95f9", 1368),("05d03259d7a1a4be9f927d7360cbea38ce44b81b236fb97427d24ff4f1da62c6", 1592),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 981),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 1374),("07393d0fe7547f9dfdda2c879c1490dd81675a3d160f9c4940efecbdcb6c11db", 1498),("cb9a77478c93a47b9ee1e7a41de0a3bfc397811f17be71907b70ab7f917fff1b", 114),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 585),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 1544),("02655d3d38c7a452fafd403dfa1ecd1965f2fecf585f38375ded956a06451e1b", 980),("6d157e4a76e43cca93450c7954873c3b34ac0473c5f102eb2e6255349611ed95", 1506),("2ce68c9a533593c49c64c89507390e7e2e6f60dfcb4f2a199b41b346456d0402", 784),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 105),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 54),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 69),("6c8ad86a026095db4707a826522be21c367d1b5a7692ca1b36d5f9a5093c7bb7", 1605),("cb233652d31b78603f79f6b90ea65d6d217fdafb462aeaf9de12a3958b8dd1e9", 454),("53df52c40028aa5f4afc4c9353306873abb2df1e7a68a39102af8403a9b6cb9f", 1801),("6dbbac9067218f60a9648b39b39626893a8b0806e007f01b41f804afa1491b9c", 583),("982f9770d339e21131238391a1489d7ab5ac582cbc40255296b9c851a64ca993", 347),("2d00c735893f9746b8f42733f7beee397148becc06408788479ca5d4cef8a660", 753),("3496ccc519f4f67acbed07b7ab74757da29f720341f14506db945b64c34c5c43", 520),("bea17a658c777ed2969a27c82fc1d89d8ca00c0105c92d8c53b25cd87bde3891", 1076),("dbb2cb3000fcbfd1ff337ddc5b5851f41734a5d17c63b67746c9462747f6f4e6", 1171),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1141),("bcc8b08a9b68038d4bc3ed26fd5c9a96dc533b57e8a848e7aa627d0d79404a8d", 952),("f07142c3caa2c9529ef7fc2c58bdedc800e894579dbfc4b38af2878940463375", 987),("7552d3701668ab67c3fef81454073544b353bf7f9973d7c5d045f68a890f0ad3", 685),("a2457c6056d20ceb73e118ebe68ac98da3a179f13c68dfaf73676211c11565c6", 1203),("44b5e76fc63f6c2ab12228eb9f7cde95e58921411ca2471c6bc7b848810cbcf4", 400),("d78f43906c01381e99b6d41745bfde62c87aef8c9d1bf9d4c7e37baee000ec7e", 1726),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 749),("743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9", 150),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1747),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 864),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 125),("a606a46671171af24a9286e27affe9ed91fab669a165bc119c02182e65c06001", 730),("2c9e74c0f039bb4c217c3d070d4121c6b7daac518b76cc34c500e82deb81e5b5", 851),("37dbcd84143ef13fe061297d5c57db4248176e6c3c11220aa26eba53c7bde969", 1236),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1294),("7688d27a065b31f318e03d8a45e0c22edd4a1eecfd5c02b15ed768e00b13a5b8", 1490),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 1578),("00b88de70c23b618a053036081c8463884beb3eb6930f9d5f14c83cf38d61552", 420),("757f3fe03a1529b773f3cd2f636998c2e50f7b29e9d14b3c8dc5ddc82765e2e9", 648),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 121),("81537cf105cf802521bca09a93eb555a685f8f1802f1d794a427983e3e4203a3", 1115),("9cf1cfb010ea423953d0cdb1dfbd6211e76876a8393f4fa81752df9baaeb13b2", 1729),("53e6a3d5a8a2cc17c1bef9a2023a67c1bcdcfc170b116596c9f9cdaa3aeba6af", 1453),("4761229f192a38af05c4298a2c2592ad5053ceb0ad78cb249a8ee928f5943c91", 35),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 175),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 965),("dc299b03650a78966002771995bb8073e9a034d2ddd1d7c8520f42902ad620b7", 572),("fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc", 928),("ea539175b7730c14ddec5b2ef70378f84b3c91ab8e58eba6d00c4ed4f63f694b", 397),("62c4051e0f73d790a599882da383765968a986aefb9c88a1a754f6f252e0ad2e", 1703),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1043),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 1444),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 547),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 536),("6d1bb8513cfb0306bbc4adabc4a59e99a7a8ff5f9d1a3bbe62740437fb846661", 829),("63c7fc887eb36dd61a2e7e424f73b5b132e7eb3b121c4b4de8d8e58847621d9b", 1218),("83cc76dba5ed09250d9bf5f97c68dd5dea609e8d60eeecef0c2cd0f19f2a22ac", 15),("d6efd493dbac5f808e9ffdcba4ca27caabfb60256f8483e659bb764207d3e05b", 1300),("89608f311b29030f940ca52e77c170b1c68b7abf2d62a319e2beb241307470e4", 1448),("c1ba3076392cb1d73624644c6494fe3aa0aadbf4109d4f94805621989c6285cb", 674),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 409),("eb9e8e6131606ddc86e622d6b7954c779fe78be4840a396dcf74c12453c59165", 1325),("a0c55088d85e7a720788540449fb4a6d2b1c205fa539c1fe5f271685a172f2ab", 996),("39dc1e6fdb112dd83dbf3537ca33a3632b1c7e7d1976be1e5e5082837f268c78", 1251),("0cfb359661b97ec1704640795a7e9cfed7361635215df0c0a129fe8a8da44fd4", 1363),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 1362),("629da11afbc5a178269828072de5b6daff248e1d160d35883ee6314560a9d3a7", 780),("37dd2822378a5fa728e4fbe20d60bcd7df7bb691ee5454bd0a7813a3599fa3e5", 1116),("d13451b20169fc02bd202693337148e185470c794b5763e749a233b6f4f89732", 224),("bfa6f3d2573dd1380b922b571770efe934e119490902697fed0cc07ff233dba8", 526),("f7424f5ad2f280f079660adc98158c3e83fd34c3893d8e59d8738681eafd3bfb", 1853),("d55e56c6cf896ddbfd2b33d06f9a3fcf4e48f0c184aac990f723a111361b16e8", 247),("24d40a7e6ee1e84e3680df3e8f7e938fae3d24c71eb95e231fe91a604aeba475", 1847),("9ed0bb6a80207a4a8d9dcc723ac311de5e4ff0dda8f0487afa5b9a0affb0ebb4", 1222),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 1931),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 34),("d55e56c6cf896ddbfd2b33d06f9a3fcf4e48f0c184aac990f723a111361b16e8", 849),("70927afdde8633d1bd8f3a71fbe1590c57a332025b27f8a94e3de126dfb9213f", 1904),("dbb2cb3000fcbfd1ff337ddc5b5851f41734a5d17c63b67746c9462747f6f4e6", 1122),("ea13c703dcc75c26a8182a923306cb8200cb97599e6a02bde21ac7121aab03a3", 2000),("0403ee5bf4aad18db5c378479e1fa028cf0323ec1f381191e53f5a9fd2027778", 484),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 1951),("8942a79a4cd137c0e979143fdaae45f7da7717f8f0af0c89cab054838367b01d", 1908),("246a0bc9fa4c0d58f195cd56a6ab01a565f480be258fe2f1e2ddb5f53cd90072", 620),("3ebd60223028a57b9c439ab880d1c25ffaf22d6cb2a8fcf4db9348d69e33c2c0", 1609),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 1011),("d9f88f42059b5f2cdad8b01a4b5d44ad5b616c52d321a994301fbb1725cd1fb5", 1878),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1968),("8b5965ade4481f65323d495f693148f7ae27d48ea3d620f2a244be42b6f6415d", 838),("527f4857f88275bfd38cf471c51f3506e68e9fc173e9223407ec250a99c80cde", 1746),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 25),("d3c8f62bdea77876a9f812511fcc23ce380fbe410c4b7380cd5e8bcc79c2cfe4", 250),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 1539),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 188),("b31a40899bfbc84c7ba780b6780bff5f33d44ce8798084db23db1a9038f9375d", 675),("43177eadc1985a577ae5fd7a93cee273364a83194df18a51961dbe262c1e159e", 1391),("142089e646a464b62338c3b077d2c24b675565a67e068478463ac5de2c18dfe6", 1912),("7c8056bdbdefa9d0a661088a77c72468f5b54f951098d18a80ded9637505b968", 1455),("21950a5f962128579a503ca4767d08bce9a32b373300dc8e73275da6ead23f51", 805),("5b0d0ef3f5ceb39709e808dab2923390826fbedcc62da14085a7a641a654cdb5", 364),("f07142c3caa2c9529ef7fc2c58bdedc800e894579dbfc4b38af2878940463375", 809),("2e5aea6d2aaebdbed333196f39921c1e5030e1b8c784b6505230601f18948986", 339),("344a205ea06716edf21467c194821647f7dc9e378f156c30830deacef7e4b62c", 1560),("6bf2b451fa42739f9e764d2bad7caef7cbd0f4696b49fd77819a6eb55f1a2e30", 1561),("37dd2822378a5fa728e4fbe20d60bcd7df7bb691ee5454bd0a7813a3599fa3e5", 1905),("819ef785c7584df995629aef88fed217d11e54aab054e2f13400c7b4b5eb8d9a", 1806),("c8ae53f54252133bbad17084106a8819c63c285e4d201c49fe0014b274246fc1", 233),("2c32168791e5c3d6b44f12b00a38e43ac7fc72ec80f8632f9a7a4701a7b23abb", 1228),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1763),("640ea7b2025bd746cb214a61786de904bd40fd3730a05e34090f1f87866cdb68", 1456),("d13451b20169fc02bd202693337148e185470c794b5763e749a233b6f4f89732", 1848),("b33ebb4136bcfc6d06841d6f8ec4a049acb37b70120e63a7d8f66afba4b6ed9c", 1382),("9bd3d71796296e6e735dc54c47d458a80f71c0eff1019b10ae29495447e2f2b1", 1607),("c15bfcc5100060143de313def76e748cf02e0c7f42cce614bae672cc11122bb0", 1147),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1657),("e4b97cd3d5071d9e7cfdbf42bd496d1e5850afe2ee92f1a16db61c99083a639d", 75),("4dc51d0373d9c60c3a708ae7cc904c2de4061f6fc09fd4b84649a222acfe63ac", 340),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1756),("a17fec45df36c7cc45a13a2754e2ab7c7a35a179f1511d44b30753646de69959", 1735),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 1019),("e110e4cdc16a81ce1e3f2a0d663aecfe173c9e4f2e86db1b69378e198a89cccd", 626),("eff83341e00950b5fc55ba22d6b48584c0d65e196fe7ca6863ae68776010d79a", 823),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 461),("3369a46f14ed50fc9c5964d1470a2b8d50c70b40048613da4d08a8eb1f6870a1", 934),("951fd0ae99c7148f5d0c9f407e1d6ebe2b71a960ba7b12946064892b104d881b", 1733),("8650598b8aaea27cfd2615c8a30d30b8d9278a0a32c55568d69370c321cc48a1", 1064),("4e2926fbdaadf155a9a9350e06787f59403bd45d416ce32f8b87da5c5603a64f", 1261),("640ea7b2025bd746cb214a61786de904bd40fd3730a05e34090f1f87866cdb68", 1901),("5b85bbe6e67a069963fbb95762f5caecd3a7c9388f8abc174ab0ba63cd8b9aa8", 1312),("5d8e8995cfa4c99c76729df1faab0c3784eaacbbf3e869db1aae4c97465bc62a", 695),("3553f4b46fe9719bb5a489435de3d19c9d880a54df177154ed59bdf8bb3f1aff", 357),("e0942307c0a44f9f0d1ba707bee6a4a47dd721f38086e8065070fc8774b8c07c", 795),("77bae19aa3febb090c58e937358ef2987272e86b5f7ce6ca0ea8f43ec44e0e6f", 1346),("a73a2e4f357894473cf2cf5f072dfeda6660a36e700c482c3f82698fa09fdfac", 816),("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 594),("b0b2f221e2e017b2969e97a34f72fd752ab03cfb897f31352b9c16aa17bed05c", 1938),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 612),("3e3cde8cb66e15af9185c2af2d156a23d49f2d50f237de1fa9a63c810ec6afd7", 451),("a5740901ff0bf4668dd17036a16c641310e0b577d8278520a890d7bd104a8ef3", 1699),("7417e67fadf786b86dc99a02f798ea46ba90d6bcef0076a8f85d2734795526a9", 998),("516bf5fab1f106a79b45439206fdfec47bcc1b958618ff2e7f1ada9bd1725d76", 796),("3b4d5eb7169d680443ed9fcf0d4072c2a0681d364b53cc788ca03a1a37353966", 1631),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1440),("5c98dba48f5dfb97ebecb44efd6b97008cd3ba0426a9ac752a66db1d0caf5f86", 645),("13c684fd29ca5da4b98e0ffd967b2257f882577f4eb96be7701ec27782f8ce86", 1216),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 494),("d989e0edbaaace9e9f5d7b6f1eeaeb3243a0944cd2446bbf2c64434eb7a215b7", 164),("d7f444c59ddeacd4783dfdd05892efd1710418ae4ba8587db72f8f4595fc03f0", 1466),("70927afdde8633d1bd8f3a71fbe1590c57a332025b27f8a94e3de126dfb9213f", 989),("7ea7d5c4c9b3dec57817678e8472a94a20dc56db0b6ca7162a9cf4a5f381f07e", 1981),("2ce68c9a533593c49c64c89507390e7e2e6f60dfcb4f2a199b41b346456d0402", 249),("58831e30b591b316646a84d08023b44ca1f0ac769da75ae4cb8f270fae9e02c4", 73),("43177eadc1985a577ae5fd7a93cee273364a83194df18a51961dbe262c1e159e", 1104),("cea5d597d1938855297d642f8009c2e6ffc281f91e6eb4c23b71eac220b498be", 100),("5605e6d71588d1dcb0d80e0ebf30c0b140cd9c273ce227893941b0a1621d6796", 1820),("8795113c70c29285fb83d2b016fb12d8ee0d3e4fb19fca6b1c014c2f5096c17c", 1337),("d5fe80e77a09629f17a1dffc3366cfdfff8b4b721a31fa76d0d7686a00b4ea87", 1295),("71e1bc1b70b5ee028539ade4a6c0164188aa69c008577b9ded656b6bc141a249", 1372),("8d5f8f2d76dd4117e1ca152d4480e2a618cac22c9660ed8c931f510e814ccff7", 962),("6032b02d17a5a8e9cdad3c8e6686fa4b4eba6db4894b5769289f5e461bd50771", 1328),("063860b464c5599b7e0402505312bf8d7e88cfb01f031a92a8745aa17ce85900", 1027),("5dd1943d29e31697288d07b1d85eed3be8e828c2668ad0291336d9b956ed91e2", 1774),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1260),("2ea4635f076d902644440309bbaa764f8940b341acdbffdd2ab2b94009e181f1", 571),("60a58226cc9e1425e41d6f68ed9f4695c0dcb20a4087240220e41bac111ad696", 1486),("2cb6c992eb0884fbdbb3ff7a6b14d496db0f746049f40ee4387ae038e6608287", 1671),("cda27e7cf67394d383227913a7ebd1d7a39357d9d10500c6633c69f450b0f4ce", 429),("62352f90c5acab0fc6abecec1c59bb1c910f797185669e86622333fcf0b7bd80", 47),("47cc2920ad53367bfd602c86ef72e8876fe6324da56a49df1f66233756a6fb09", 1990),("fb56ab0071af648a16b7d8176b29d4837aab690d1abcbf8c6fb041e51e61aa9c", 65),("1939ae0d38e14a78290c822c2d98553fae16fff58fbc8dc48c399d7ae169a817", 1125),("5ed361d45b385cfc0051d0b23f90f9dbbaf31bbf477fd680c50291cf660fdf54", 388),("5f6f7398021dd0d51a84cae64621cac68ce4ff0528073a5fdfd5df5d37793cb7", 1974),("f6cd7b988af9f72913ee38a76a8c9aba69e8d92ba7b436df8f39af4943129e40", 1745),("bf7815c50f5ece3f3358d1ed119ba4dd89ec1c64a3054a959690895f58cf2424", 927),("8d5a29bb5fef3db96a896dbe0e77790d627c2df094facb67460ca0d8d67ae6e8", 1526),("4f0253e5a5a15d0462271b9cdc3e46f81450b4e830c953cbae6e0ee7d409626f", 256),("48527a2aa76342bab25315c141a93952004e8d2814a788ac072f2101ebaf9336", 1874),("7a10a4d3da8081f7234845ed153cdb477e10bd6294cd74bc75b04399c123d99e", 929),("a652a7a396f2905e5240f673f7c34b87975642ff4a731a27bb460f4dbb3977ef", 29),("c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704", 732),("4b521156c58cc121e0694972b748a938ad8e89fd71bb4554525a2828f2a018c0", 1676),("edd79de62be83a1d86b0aa99939d11d9f2c19b91e36532f51824a6891ccc2772", 275),("7d1ea7fe069edaa7b37aa494239a901713440736c167ff0e17e0089e5dac3732", 541),("1a96f7857beef47553c674737b90a73acf087147ff68e480e5170344df8f9b0f", 1634)];

    for(a in airdrop.vals()){
      _transferTokenToUser(a.1, a.0);
    };
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