import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Cycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Float "mo:base/Float";
import HashMap "mo:base/HashMap";
import Int "mo:base/Int";
import Int8 "mo:base/Int8";
import Iter "mo:base/Iter";
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

import Cap "mo:cap/Cap";
import Root "mo:cap/Root";
import Router "mo:cap/Router";
import Types "mo:cap/Types";

import AID "./toniq-labs/util/AccountIdentifier";
import ExtAllowance "./toniq-labs/ext/Allowance";
import ExtCommon "./toniq-labs/ext/Common";
import ExtCore "./toniq-labs/ext/Core";
import ExtNonFungible "./toniq-labs/ext/NonFungible";

actor class Canister(init_minter: Principal) = this {
  
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
  
  let LEDGER_CANISTER = actor "ryjl3-tyaaa-aaaaa-aaaba-cai" : actor { account_balance_dfx : shared query AccountBalanceArgs -> async ICPTs };
  
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
  private var ESCROWDELAY : Time = 10 * 60 * 1_000_000_000;
	private stable var _usedPaymentAddressess : [(AccountIdentifier, Principal, SubAccount)] = [];
	private stable var _transactions : [Transaction] = [];
  private stable var _supply : Balance  = 0;
  private stable var _minter : Principal  = init_minter;
  private stable var _nextTokenId : TokenIndex  = 0;
	private stable var _assets : [Asset] = [];
  //_assets := [];

  // start custom
  private stable var isShuffled : Bool = false;
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

    _salesSettlementsState := Iter.toArray(_salesSettlements.entries());
  };
  system func postupgrade() {
    _registryState := [];
    _tokenMetadataState := [];
    _ownersState := [];
    _tokenListingState := [];
    _tokenSettlementState := [];
    _paymentsState := [];
    _refundsState := [];
    
    _salesSettlementsState := [];
  };
  
  //Sale Marketplace Code
  type Sale = {
    tokens : [TokenIndex];
    price : Nat64;
    subaccount : SubAccount;
    buyer : AccountIdentifier;
    expires : Time;
  };
  
  type SaleTransaction = {
    tokens : [TokenIndex];
    seller : Principal;
    price : Nat64;
    buyer : AccountIdentifier;
    time : Time;
  };
	private stable var _saleTransactions : [SaleTransaction] = [];
  private stable var _salesSettlementsState : [(AccountIdentifier, Sale)] = [];
  private var _salesSettlements : HashMap.HashMap<AccountIdentifier, Sale> = HashMap.fromIter(_salesSettlementsState.vals(), 0, AID.equal, AID.hash);
  private stable var _failedSales : [(AccountIdentifier, SubAccount)] = [];
  var price : Nat64 = 500000000;
  var whitelistprice : Nat64 = 300000000;
  var saleStart : Time = 1642906800000000000;
  var whitelistEnd : Time = 1642950000000000000;
  stable var _tokensForSale : [TokenIndex] = [];
  stable var _soldIcp : Nat64 = 0;
  stable var _whitelist : [AccountIdentifier] = [];
  
  func nextTokens(qty : Nat64) : [TokenIndex] {
    if (_tokensForSale.size() >= Nat64.toNat(qty)) {
      var ret : [TokenIndex] = [];
      while(ret.size() < Nat64.toNat(qty)) {        
        var token : TokenIndex = _tokensForSale[0];
        _tokensForSale := Array.filter(_tokensForSale, func(x : TokenIndex) : Bool { x != token } );
        ret := Array.append(ret, [token]);
      };
      ret;
    } else {
      [];
    }
  };
  func isWhitelisted(address : AccountIdentifier) : Bool {
    Option.isSome(Array.find(_whitelist, func (a : AccountIdentifier) : Bool { a == address }));
  };
  func removeFromWhitelist(address : AccountIdentifier) : () {
    var found : Bool = false;
    _whitelist := Array.filter(_whitelist, func (a : AccountIdentifier) : Bool { 
      if (found) { 
        return true; 
      } else { 
        if (a != address) return true;
        found := true;
        return false;
      } 
    });
  };
  func addToWhitelist(address : AccountIdentifier) : () {
    _whitelist := Array.append(_whitelist, [address]);
  };
  public query(msg) func saleTransactions() : async [SaleTransaction] {
    _saleTransactions;
  };
  public query(msg) func salesStats(address : AccountIdentifier) : async (Time, Nat64, Nat) {
    if (Time.now() >= whitelistEnd) {
      (saleStart, price, _tokensForSale.size());
    } else {
      if (isWhitelisted(address)) {
        (saleStart, whitelistprice, _tokensForSale.size());        
      } else {
        (saleStart, price, _tokensForSale.size());        
      };
    };
  };
  public shared(msg) func reserve(amount : Nat64, quantity : Nat64, address : AccountIdentifier, subaccount : SubAccount) : async Result.Result<(AccountIdentifier, Nat64), Text> {
    var c : Nat = 0;
    var failed : Bool = true;
    while(c < 29) {
      if (failed) {
        if (subaccount[c] > 0) { 
          failed := false;
        };
      };
      c += 1;
    };
    if (failed) {
      return #err("Invalid subaccount");
    };
    var _wlr : Bool = false;
    if (Time.now() < saleStart) {
      return #err("The sale has not started yet");
    };
    if (quantity != 1) {
      return #err("Quantity error!");
    };
    if (Time.now() >= whitelistEnd) {
      if (_tokensForSale.size() == 0) {
        return #err("No more NFTs available right now!");
      };
    } else {
      if (isWhitelisted(address)) {
        _wlr := true;
      } else {
        return #err("No more NFTs available right now for non whitelisted users. These will become available soon!");
      };
    };
    var total : Nat64 = (price * quantity);
    if (_wlr == true) {
      total := whitelistprice;
    };
    if (total > amount) {
      return #err("Price mismatch!");
    };

    let paymentAddress : AccountIdentifier = AID.fromPrincipal(Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe"), ?subaccount);
    if (Option.isSome(Array.find<(AccountIdentifier, Principal, SubAccount)>(_usedPaymentAddressess, func (a : (AccountIdentifier, Principal, SubAccount)) : Bool { a.0 == paymentAddress}))) {
      return #err("Payment address has been used");
    };

    let tokens : [TokenIndex] = nextTokens(quantity);
    if (tokens.size() == 0) {
      return #err("Not enough NFTs available!");
    };
    if (tokens.size() != Nat64.toNat(quantity)) {
      _tokensForSale := Array.append(_tokensForSale, tokens);
      return #err("Quantity error");
    };
    if (_wlr == true) {
      removeFromWhitelist(address);
    };
    
    _usedPaymentAddressess := Array.append(_usedPaymentAddressess, [(paymentAddress, Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe"), subaccount)]);
    _salesSettlements.put(paymentAddress, {
      tokens = tokens;
      price = total;
      subaccount = subaccount;
      buyer = address;
      expires = (Time.now() + ESCROWDELAY);
    });
    #ok((paymentAddress, total));
  };
  public shared(msg) func retreive(paymentaddress : AccountIdentifier) : async Result.Result<(), Text> {
    switch(_salesSettlements.get(paymentaddress)) {
      case(?settlement){
        let response : ICPTs = await LEDGER_CANISTER.account_balance_dfx({account = paymentaddress});
        switch(_salesSettlements.get(paymentaddress)) {
          case(?settlement){
            if (response.e8s >= settlement.price){
              _payments.put(Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe"), switch(_payments.get(Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe"))) {
                case(?p) Array.append(p, [settlement.subaccount]);
                case(_) [settlement.subaccount];
              });
              for (a in settlement.tokens.vals()){
                _transferTokenToUser(a, settlement.buyer);
              };
              _saleTransactions := Array.append(_saleTransactions, [{
                tokens = settlement.tokens;
                seller = Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe");
                price = settlement.price;
                buyer = settlement.buyer;
                time = Time.now();
              }]);
              _soldIcp += settlement.price;
              _salesSettlements.delete(paymentaddress);
              // start custom
              let event : IndefiniteEvent = {
                operation = "mint";
                details = [
                  ("to", #Text(settlement.buyer)),
                  ("price_decimals", #U64(8)),
                  ("price_currency", #Text("ICP")),
                  ("price", #U64(settlement.price)),
                  // there can only be one token in tokens due to the reserve function
                  ("token_id", #Text(_indexToIdentifier(settlement.tokens[0]))),
                  ];
                caller = msg.caller;
              };
              ignore cap.insert(event);
              // end custom
              return #ok();
            } else {
              if (settlement.expires < Time.now()) {
                _failedSales := Array.append(_failedSales, [(settlement.buyer, settlement.subaccount)]);
                _tokensForSale := Array.append(_tokensForSale, settlement.tokens);
                _salesSettlements.delete(paymentaddress);
                if (settlement.price == whitelistprice) {
                  addToWhitelist(settlement.buyer);
                };
                return #err("Expired");
              } else {
                return #err("Insufficient funds sent");
              }
            };
          };
          case(_) return #err("Nothing to settle");
        };
      };
      case(_) return #err("Nothing to settle");
    };
  };
  
  public query func salesSettlements() : async [(AccountIdentifier, Sale)] {
    Iter.toArray(_salesSettlements.entries());
  };
  public query func failedSales() : async [(AccountIdentifier, SubAccount)] {
    _failedSales;
  };
  
  // cap
  // start custom
  public shared(msg) func initCap() : async Result.Result<(), Text> {
    assert(msg.caller == _minter);
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
  public shared(msg) func lock(tokenid : TokenIdentifier, price : Nat64, address : AccountIdentifier, subaccount : SubAccount) : async Result.Result<AccountIdentifier, CommonError> {
		if (ExtCore.TokenIdentifier.isPrincipal(tokenid, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(tokenid));
		};
    var c : Nat = 0;
    var failed : Bool = true;
    while(c < 29) {
      if (failed) {
        if (subaccount[c] > 0) { 
          failed := false;
        };
      };
      c += 1;
    };
    if (failed) {
      return #err(#Other("Invalid subaccount"));
    };
		if (subaccount.size() != 32) {
			return #err(#Other("Wrong subaccount"));				
		};
		let token = ExtCore.TokenIdentifier.getIndex(tokenid);
    if (_isLocked(token)) {					
      return #err(#Other("Listing is locked"));				
    };
		switch(_tokenListing.get(token)) {
			case (?listing) {
        if (listing.price != price) {
          return #err(#Other("Price has changed!"));
        } else {
          let paymentAddress : AccountIdentifier = AID.fromPrincipal(listing.seller, ?subaccount);
          if (Option.isSome(Array.find<(AccountIdentifier, Principal, SubAccount)>(_usedPaymentAddressess, func (a : (AccountIdentifier, Principal, SubAccount)) : Bool { a.0 == paymentAddress}))) {
            return #err(#Other("Payment address has been used"));
          };
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
                  return #err(#Other("Listing as sold"));
                };
                case(#err _) {
                  //If settled outside of here...
                  if (Option.isNull(_tokenListing.get(token))) return #err(#Other("Listing as sold"));
                };
              };
            };
            case(_){};
          };
          _usedPaymentAddressess := Array.append(_usedPaymentAddressess, [(paymentAddress, listing.seller, subaccount)]);
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
		if (ExtCore.TokenIdentifier.isPrincipal(tokenid, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(tokenid));
		};
		let token = ExtCore.TokenIdentifier.getIndex(tokenid);
    switch(_tokenSettlement.get(token)) {
      case(?settlement){
        let response : ICPTs = await LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(settlement.seller, ?settlement.subaccount)});
        switch(_tokenSettlement.get(token)) {
          case(?settlement){
            if (response.e8s >= settlement.price){
              //We can settle!
              _payments.put(settlement.seller, switch(_payments.get(settlement.seller)) {
                case(?p) Array.append(p, [settlement.subaccount]);
                case(_) [settlement.subaccount];
              });
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
            } else {
              return #err(#Other("Insufficient funds sent"));
            };
          };
          case(_) return #err(#Other("Nothing to settle"));
        };
      };
      case(_) return #err(#Other("Nothing to settle"));
    };
  };
  public shared(msg) func list(request: ListRequest) : async Result.Result<(), CommonError> {
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
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
  
  public shared(msg) func removePayments(toremove : [SubAccount]) : async () {};

	public shared(msg) func setMinter(minter : Principal) : async () {
		assert(msg.caller == _minter);
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

  public shared(msg) func shuffleAssets() :async () {
    assert(msg.caller == _minter and isShuffled == false);
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
    //Mint
    while(_nextTokenId < 2009) {
      _tokenMetadata.put(_nextTokenId, #nonfungible({
        // we start with asset 1, as index 0
        // contains the seed animation and is not being shuffled
        metadata = ?_nat32ToBlob(_nextTokenId+1);
      }));
      _transferTokenToUser(_nextTokenId, "0000");
      _supply := _supply + 1;
      _nextTokenId := _nextTokenId + 1;
    };
    
    //Whitelist
    _whitelist := ["7ada07a0a64bff17b8e057b0d51a21e376c76607a16da88cd3f75656bc6b5b0b","bfe89d86ad4332363524a8b0e4bebb5f52403aa1814ea807bd4602443737a456","4b210f95fe16ad398cf6419ad139a369286d82ae6d4478f6f39fb1b74d554e75","13bde6f7e043e68912add03f344e34d9efc315faedb58722913452958ce54517","553846454c5ac96ba3966d446d1c2eaf440a59e19dca4d08218e42cb0087a1f3","3510be402ab0b290251729c5fa48e4e991f5f327ac1ea6e6dfa98862696f80f5","d1795d19da19987b3ec2dc081e548823e4d6cd3bcb66a8d04674a08685cc6d6d","096ed2e3f34a88c48938e413840d49450d8d2cfcddd51a4e6de199d363530b20","ef73650d04258cd780c3bcfa892ac72b2184fd2badf7c0c6a0046e9ecaae5ae7","3033418d86b83ce29be3e3f5763843c376728ee1ab6179dce14d64570fb4e36e","6d157e4a76e43cca93450c7954873c3b34ac0473c5f102eb2e6255349611ed95","0f15a14cac5a4db8b94a80298fb4bb4e7667ae2d58cf6a36e3aef64ab3cfc08c","4a9f5a4edc2cc10d2680e70e9d9c574bbc35a2c70fb7c1a2f7f0b2aab2dadf47","ba269dc70371d4cc156a316204bbebd7cd0b6238bd08ce1ab4fad39852b33429","1a96f7857beef47553c674737b90a73acf087147ff68e480e5170344df8f9b0f","d83bbb01c80d1a387ea099fcb0468b3ab46c0777723f3bcfa27350f301994274","01f8883e5c38047eb661a6c84347c6c77e4f2ca9f659a0235906b5ed8cda576f","7fb01a5e86b5919923ab518656c85e361d878e7395b32f7f05a66b5b94ae8cf3","ce14034611bb7068be1e4cef5d9f4006eb78008e3e33d193b32da66ebcad6922","e378e7b78a62d4f27a8ae1da4d90ff7b2b68b296da4cdd0e84747e98d56b36be","b87b14669bd5bedb909133d81d68de67e6940fdca422bd28bcb5696c3892b822","c75fe787358afb58a91da76436fec66f97ca6fde4d471c5ff189e620908eb704","e798c9f254b18d10074068594090ebc047cc95dd7acb53b98cd41bdf172f6c2a","d989e0edbaaace9e9f5d7b6f1eeaeb3243a0944cd2446bbf2c64434eb7a215b7","08a8f7e278b0e210a70c35be3254c3ca4d1cf2e983974ba33d7f61fb84277c50","71cded453d6cc8c68ead3c407f5a2bc6d89b8d69f18c199feaa2ca58ec13f802","d12682f8d06180dd9dbb7184931348862cd685dfc1b9c28eab2741ce6739fa91","0fc2b3811f84c8287bcad15f88590f0a063b24cdbb3dc8010f94749524374329","cea5d597d1938855297d642f8009c2e6ffc281f91e6eb4c23b71eac220b498be","5a0a700af7b3cd470ca426084327af9c585985789fac00926b456ed3e17f8588","cdf3277dcfd7beb5fe8cfa54f50885fb9bf59ef26774ad4e59ed6276bdba203e","32c2ba5a8adc3d73e1f06ca03a19c045c1121b3b6757f5a124709fd972b0cb07","5b17fe4c900aab6a8df828d34d7a3576f3120378e23d80a0f8d2f9e283cc4699","66bd2c9cf5069dd49734f9f8f89e64a144a4eafa1818c9c6801c069160c4d49b","ac11d0f8198b9987286668990a97fb1d13becc182f45418763780fb7559e24a2","c4db622e9aed193b788ecf1a8c0b5179e917c13157d95fc369607d73013147ca","db29c8469fda1453327c31b97cea981bd9459530bc63ec783150a0c7edef696d","3a1efaf73535cf6e6135b39b95a46b3240cab2bef1f90ba32ea622b863abda80","e75001b78170959550e7943ff987163f9dc164bd88626855abba578befe08fd2","38135e574c9d6c91f80060b56666f903e8afba60d14c464e95ceb042d48af9cc","c2dad45409ef4682f4d087243198f007e4f8ef38f35661e59913d2594a6c444e","29649aee63b827a72ee62118c502d1e6a98052391fa68b57988608c07ebda6a1","98d4009daf774ea5c32ef246f1e4202d404c01051346bda0c08cc5d5cff6cc3d","cf7e1c65b5ba50f06340c98c59b2db3ad3150d1c800e493355cfc05d3454fb2e","f1d60ed2ac8720cfd53f3d32da2e765ae76d1948ff7edb2eff056d81570eba74","f859a28b9a145aa220a663a3651781b11e6cb0eb91dea4102898a691797d6e02","8ebeda57d5dce58f9d18c00b7a117471391dd03e25a509c5dd4a51b5cc2ad3b3","d9bd89f58e8f17cda5ea2f8c441518962bfbaceb3cf1e585e898f59dfe00bbe2","d04e5ad88518457ba027d9a46ff4bdf0785fc0fc2a747fb523cd3c1ac9847576","6cd96a7cb0d753da32d62c38b0bb9c59e2102300bb3bf7b34f20d1a86abec653","ac79b1ca1db9f0c2b41c53720bdec80791543155c474da9fe735032b881258ac","6751a319e060b2a6e521161cd731e2b3230492a2172a6780eaec0a7d39dece6a","59c7b7e4f441a63879940321770d1ba3fedce2db7e150f8ab966afcd54b6820","5cc6a424cda96e21f0c9aa57c39882617d941927cf53983adae44281054322f4","0afae8bb629beb70d3ea1aa54e089f8a7d3fcdae944e8bfc8c8f5bbcacba6472","6da0aeac3da9cf2321ea6c9d1de1106decb571632309782386c9cc9cd288b46a","8a40336b7f2ddf28924f9b1f8c4d41a6f7752e482d5d877661677db3724040ec","a68114e5f9afd127d933fe55db998e839652bfa82502328006ae8403633d5a3b","23c2954703a2e6843ef085f8d529db94e0c9d6ed0361e01308b83e9828346baa","afacebad884369499060fdb19ecbaafde227e2712e8d9f24ba674067c3f71f51","60262f93525e89e70bc1847e7af95dedd76196038b48f1f05b83717525ab4ff7","07393d0fe7547f9dfdda2c879c1490dd81675a3d160f9c4940efecbdcb6c11db","09dbace0e7ccbb14c5fb222a2d1858bfee1b76bc6fa37a67c1ef62dbc70af516","fe98ddfe2520af31409d8085952a53fc14fbfb7224c37c109f677b56feb48ffb","b3cafc165809571ca8ed108cc5b235f05c92ce6c49095ccbdc06c9f9161fae54","3243ec7364e3f1afc38c0fd1df3bca8be0982f2362690d759e34540f017cadf5","0682d596c67269f29789776c30959065b6224ddeaecbea2abbeba7d922f6c973","03849f6af2b01270c771c070484324631690150f9afb22485bfe8489051ff3b9","495f5ce737bc3824b1a4bf13076262102b2a1f99fc70379e15c5e14e4903f3fc","38d28ff21dbdca67cd9a3594d7bad1bac90e7c137c30d6ca7b9a1c40a5d1daf8","94177171dbdadc31a5172b36534362c558b8c5b71acb1960e75d45764e764acd","65cba732a9daf594b07cbecfb82aca329628e071de78fe9ee75ba31749aa78fe","a17fec45df36c7cc45a13a2754e2ab7c7a35a179f1511d44b30753646de69959","5c98dba48f5dfb97ebecb44efd6b97008cd3ba0426a9ac752a66db1d0caf5f86","ac9eb89322f48f73411b429259e98f6c7c64811d4c4c0529491fce93ddf4addb","c1be62a1f2b6bc42837894b54d8d8862253bbe77e9965d4ea0a03f05c2b1dc3e","5892301b21ec691592a1e97f05a1a42a6ecfced1afb762b420ad5e63096cef12","ed817a1e117bb940e559511add3614958888fe5bf96a7cd5dd9ed2bcc53ea242","4f0a40f1e24fdac90cfa87d7a0a3427aea78dc0cd6b046ba3497689343a2852b","9d281bda01f37e9e4c370a25f30fee0ea6fdd9c4c4664872090a8a793e95e5dc","124711eb79229b34a9ac7bbc639cec4605e8f3f972668e14554622c48525edb8","b34f44c0211046f1bb9c4cc341174fd872473e1053f8abc987681b2feadb17de","f9f0a4e1168757a5e924e2368ec9515fe8ebac0e0ada6a9ce0933c5e777fc2a4","04860ea8d8d730a1a1c8c3564e2dc1dd0b55c4d14735e58611feb60547e2b915","13c27ba39a5f7e79bd79682bb4439a2e6f5ad6850e2a3b537f6eb1579957fe25","d6f70bab2de2fad7f9a77514fd3e0a636cbd997d99252687ec8f93d361942513","95d09a2fb0f220a70217a7752b8c899e4066605a953d1fa97afde0008165ea5d","65520da3a5dff827d56fcc5c3b0e6e175e0ced6bac34735a45ce4aca927a74c3","a576363ec09c08adf32680cb330b4d295813d5fe23c78b2e9895b3e0468f3129","724d0aadf8de7351661227f90384861a8acdbab76de7c20f943c690e30d01c42","a555c4f1e300d5b8b8275c29ff9a8e06f746ed736edef52ecf2debf65f065e36","350653e8cba6a4a6a408e973fda85b7fac55d1a372d875a9b8fc361cd4202bd4","0aa3f7c95eafd19ee4dd8b2dc2f9438f0f9316beb899063928e5d41aaaac0199","ee5154288dcb91d9c879d267d034c68139bc8e10a3201a1e46060df015da6ec1","5066bca3ecf91a11396efbb7bf4bfe81fcec7b973f91701b2b1a34c6952a4812","a50b963449a6e8ed4473912013c0dadf08eec9cd80381286fb5323d349292f3e","e4e3a145bfcaae7fc00a4153e7e67c7c252c1a804e717f8cbfa00d83a7e7e0a2","1d0401393dd7ba0c723b686fb1be5982069ca2e1628198067f585e3be7afd7e3","f81d78a581ed0805a1fe3989658ec527a78fddc45c4efeffa90c4088b20cb407","c60b9a71620abbf10f2e8733b03bd70af49ebcb385052dff8a70adf8759d52c1","b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8","a8c2ec218d9f56471af293f82330ef1d139a578b28fc0f1e8dd13f03736930ca","2c9e74c0f039bb4c217c3d070d4121c6b7daac518b76cc34c500e82deb81e5b5","10a44d510fff9c29dde52cc2303eb4bbfa08ca1eca81d1fc6eb3023fa668e85e","904b04ec604b3fdf854822713f1f6c693b8ac4bc6d4db5ec78c8848dad30e63f","37b0cf1221a953e65955a927ee21455eddc00ef79abb929dc678f6278318cb82","35dc4e3729f568399a45e26b5c294a3e9d3b50be304a4ee7139f1695b1a142cf","8dad581ad46d78c73a07140e5208ff8132e629dad2beb39ef92da263cce27c2a","6d697135f52a34e58510868a0bde2503e4044d9616f2048d61237e7a4ac7bb1c","f5966a0ec93e7cb196c9be71ff324d7e93312b7de3ccc29c70540171d4b788d6","46495845866754672a28bf3f1c272e145b04b5920fce13bd18868876313e1605","70927afdde8633d1bdaf3a71fbe1590c57a332025b27f8a94e3de126dfb9213f","70954a4b30dddf56ba4abbfa56dee64f4ea86d25155c4aa0689a544668b83c2a","dd2a477e400ef58677b39eddd1bfa7ff784af6b6f419b4b9bc72e948b1e89726","7b325ae7036eb3c7d990ab70cbc38755863f17c6ec743405e5a27af39272cba8","fa81d0f67385b9eebc40e3e8f11cd493238a20fcdace7d8925238044fa654902","ccd3c64d6b1341488e71de5a03c83b31e56800c675fb05d6a4d1fcac73d0eef2","5844cee93e3ba6776edefe080483a62de41f9805dd8af441f9b42ead28169810","dfc4c3d01dbf34c8e7bb2fb74eeafbaf3893a14e2f2b6374de397b920da4950e","7ac8e0f803ead3fcba77328ebb4278b70e806e83b25fca8b66548f939b80d565","3f00a1caed00cd13290ce00af07409782e1e271b9f4db25f690fa9227038b38e","120ade894dfb0d2937f734f8654a386dd6709e3fe3866bfa7104f0b424b1d1db","6a81e740609ecf26d606b6ccb7623d7bc02c417e81b5d65993b250d835af6bd3","81c1a1a902811b758b5de42cc5a671c4acd091011ab20d59ecb38337ebd3d997","775d474c99cb81177af6773905bd2f5b6e29a7846a09f1821141790423f265b6","ba8588f74119e8d1ce53ec5ba1229e1cdbda8cfe72c8c4cfd0fa256319de114c","adf37be915462b33c162466327fe1064e42c948a444fb98eae47ed677cc5f927","def89a505c18fa12af885484780f94fd0bdedb99063470eafdbc2001965737d6","b3440763715aaf1ca4ab24f8dad939387108e53f6360d5d6cf93d95296eb953d","1860feaeef462fd896ad1b16b9883f1cdec48f2c5d6b89ff04b2874e7fe99c21","6ac89d0ee05bb8b102490a0ffdc3ff46744e61ed8131cdaa3a08319592187ae8","873b06b87805708a61f506b9b63d123f691bb2a8c0acd0ac8a19401da12db0db","391ef05675113533cbd3b592109aee948a8726c53a12790d9927689afa920b25","743b072344a6e2ecb43c5a7b2cd84cc2cdfbec2f925784cd890e74645748d1b9","b86ff968f2feabbb4f5c8e52cacfa2236d198f28916a6aee222b368674f7e106","8918b070a4605831df412ba916784b4e7c20e81fe5f96d25347057c5334e6501","6b2301fb5c0922045bb0e73b0bf46eee420aa4d9ab580768ea3f2d8e9b4db0f2","ee3b8acda535268d9f1595cc4c80cb517d1d9547d4e7c8d828db29957d3b4b8e","de712aab2b85e1bb07cf92ff656204a44e6d96f85edf6f9953de05242ad1f667","4a66eed26601245b3c3c90578aee7c547399619ffdf99d83599109b592a6e33b","7cca158ff3eb53bf8697994da645f690d8c4324d81cd7141e6ce175ee83bf008","3e206c885069a733558c6ffadbfe71411f1393e3f2ac3570d4f59899b1e07ce1","6aab36fadc9724b8115cbb13ff484e6de70eb9c69e38e54a20938a7fea2425e4","e288e0c7743f81bb279b0fae5575588700e0245b51091e13b808887b34e0737c","53f22bcb0d0fd088851ea25ace1e9125364a8e542d9e7f38e21df5e5c0aab50b","808cf7b7a95c98713188dadc80b8807087e743e6e3b4d2fbff6ac2fd89d8f3f4","ba56ae3dd35eea3cc7f8bfe1d8d234638390588674157fe4302ad1775200f187","da60d631b557bcc363d4ecd74ffb2be34579e7b57ff932bcd6824bec0fb28f90","9ab27009c83a3a74373e3dd45b11d482daf1ab77549cea4e28c3f6bae14a19bd","af77b31dfd68d7bbb3b928ccb7bb73a546e7132338edd0e5b19d7d51cea2a80c","fdab27844b32357777a88c87e6bbc0ef8bca9e5ba4b400decb9c889f267de28d","cc98123ec0a4bee4f36bfcd84c7fa5d6c21b9edf51a344b82e5c1ef5ee5a0ef2","0b927d67d8316ace58aed9e8d7567d4acd4b810cb54aa02ec3a309f9b0417b24","a3c3e51da1f347966b407f0804a6f71f66210967d2bd4b32084931dcaa475e11","9f1f541c56bad7f5f2cdadd61c14a54efcd65f6254c17c30736c0ffd39bae2f7","0f1195548c67e42bc94580405493c02f85ea93926f8a679f620bfcdc33c5ab0b","09e3ca1d8f6231dfe26850103546583d20759a046b7fcef05d392ee01699b8de","736a8cf1eb92fe04eca2efa00219c615f17b7991ab76dde965223b0138973312","e9a5845c3dbb0a8ff857dd187d9a4cbfe474b9b45036bd1d1b3bee0bdd71bc7b","fc400aa29ba34f4966d1f081b9dfdf55b7442f39cf1d4332fc6b103f966d92cc","e5c8dc40979a1c9f8ccfee1cafeca464e12bae9302d39cc62fafdb0def17ce58","19e73e555c3dbe1b60efb37bcd6ca00a628dc12d4b692ba313317d82a1ff727b","5e266697d49bcfad8ea207ac46ef7d8b27253b166d36703df3a3645a535ac6d0","ec2cb204b2f1e7473338a5db2a654502543c24ea7c96bd15185293e07f0cadfe","d6fb36317e83f6697c3088f892258e7bc0766a3169c03b6e52d5a15b50d52feb","2a372c9033a8cdf1a387ed708a338b50d74681daa6e96c7f41de268e96ba557e","e045a277a5f67a9f117f432cba448854f218fbecb501733a72af360cf49ac768","6a82f7e8c2285591ccfd9c20ffdbd710f0f5186d1e0334d3b03f639e53f087ac","12bc158b6aad670322f4bfc177b6fc61bbb51d02816fcc26fb187ed9301df7ce","4f792d188451a27336b55111702fc046a71ec2a9940fc4953c131a39baba09ff"];
    
    //Airdrop
    var airdrop : [(AccountIdentifier, TokenIndex)] = [("05bf8280738163ef12ecb600f8a0e889738fb2808f7154b45107633c00116c18", 737), ("d83bbb01c80d1a387ea099fcb0468b3ab46c0777723f3bcfa27350f301994274", 1096), ("72d6be118bb00b82a29a4ab59bf3157191ac3e0226f29bf95fc9bed92789f747", 178), ("4743240db87fee6fe5fe0a4e26ae7b434d3e469304b4e3dfea1abc9a6a6c25de", 617), ("941da6fae559c18392da1450df04b676f3fb0951dbaabe205406fa4a182f7ddb", 1192), ("60a58226cc9e1425e41d6f68ed9f4695c0dcb20a4087240220e41bac111ad696", 1988), ("b7b2625265b54cfe088c580f2fb5be220ccfaa601a221cd327a878e4ee7e4ca8", 1958), ("bdda500d2e06cf06d452ce9476bddc8d85f678687c022bbc47e988cfa4e734ca", 1087), ("8cb5df48a9acf983571b87832d5e4533fe769bc7ec0b4f7c0dc5c90d333b0142", 77), ("360907217814bae548fc4fb596a8b0f9f7e31f3555edad4dbcf9d892c972e92a", 1288), ("482d349fd1415c4bb73114291f7b42fea346843041360843c67798e06194302f", 1417), ("3510be402ab0b290251729c5fa48e4e991f5f327ac1ea6e6dfa98862696f80f5", 1532), ("5cbc75b2be1d3ba33df4380f615b7c818222c086ff2cb8358142e4e6afa62972", 319), ("e7be1beaeab10384581be8cf671406a7c1f86f8faf671eac443e3fefb2efb0e4", 1934), ("7a73e601203e751ede46aa18391cae651adaeab0e170b6dabdfaafd54f12d99e", 1755), ("dd2a477e400ef58677b39eddd1bfa7ff784af6b6f419b4b9bc72e948b1e89726", 925), ("ddf33f6f4c734f7f8f305937a403cc2c5e2bd0d069832faea96faabf71f10431", 1643), ("33630775e17c103a8a4bb8b2b353d07b7233b3d6fba4506f75c68812433b418d", 810), ("c6434bbbbb95ab2ecfcea7983a7f3779d6121f92e78e562950d5b7aa01cb220b", 1082)];
    for(a in airdrop.vals()){
        _transferTokenToUser(a.1, a.0);
    };
    
    //For sale
    _tokensForSale := [1913,455,210,772,2008,1850,106,1685,190,760,238,327,143,614,1311,1603,2004,1830,782,1530,826,762,1856,913,345,633,11,278,1347,565,797,137,205,1091,1047,1461,1500,1,32,519,1232,1123,207,218,910,1694,1565,124,1416,42,1567,120,1616,1582,598,1310,470,1181,1141,1388,1025,1090,1459,748,321,866,1857,136,1106,1752,1778,161,24,378,1266,1407,1353,247,858,1653,1048,1601,1180,676,788,655,1465,722,949,672,556,646,1568,661,1335,428,1551,1665,1493,784,158,1701,449,2,496,1078,1833,1391,225,1053,1341,295,1108,1346,276,1492,242,1503,47,1566,52,471,584,897,1149,530,1203,1063,334,922,1977,668,1533,1599,592,89,1937,911,1397,1069,558,689,623,1080,155,25,320,71,1949,550,1738,865,395,955,199,860,1974,196,412,1183,394,100,1715,1494,515,643,590,534,945,419,244,1011,847,355,1033,1893,1932,901,803,1839,567,973,472,560,1455,1615,1756,591,1518,843,1077,1710,778,892,1521,718,1529,882,1030,462,561,1142,634,1277,1268,1920,1907,150,1594,636,273,1022,729,1732,1973,1086,841,785,1188,139,1536,1929,967,1479,113,13,717,1519,1039,317,916,1855,1957,539,1610,495,825,1511,1193,1557,1194,725,1491,1050,62,1411,1059,1238,1509,175,402,270,1696,881,160,1284,434,1005,1100,1798,855,1962,1071,1862,702,651,1797,1553,694,777,1609,1157,1031,176,223,1880,817,536,678,1487,582,789,952,1445,95,39,1480,474,172,818,659,1781,1403,1773,1094,1334,1070,1878,522,1213,1449,383,845,670,695,1556,1056,1642,1154,888,1786,1933,1290,1804,1502,112,1590,1434,1736,595,406,1667,451,1985,1370,805,468,1882,919,1357,1302,1555,904,1161,615,500,1308,800,1613,557,1668,293,1906,1938,691,1431,1490,523,1410,873,518,1236,346,144,129,1293,1501,329,1815,744,1622,1254,1386,5,938,742,1178,1187,1499,1819,570,1222,1322,450,134,844,821,145,1774,926,1631,1362,1364,263,241,924,1393,438,1286,1246,1712,363,812,1828,1965,544,734,1042,1002,1368,1267,1375,222,299,1104,1823,1734,1816,681,1474,1214,616,528,1331,596,2001,170,1894,1419,1396,975,870,1923,1864,1458,669,219,0,458,1735,1787,1088,1124,1746,1430,1851,1940,1488,17,872,1092,1475,937,485,1910,84,1759,1841,1931,154,149,972,1697,902,1084,325,1744,1146,1757,1265,221,1476,769,1515,97,1674,146,245,907,915,1552,1722,361,618,1604,459,1372,850,2002,917,357,836,1170,940,1677,1120,733,1126,1343,1848,1292,513,1805,1854,795,1014,22,114,1133,951,204,132,1176,1761,475,1428,288,1018,839,1704,1460,773,819,1173,642,716,756,1983,1255,1444,696,1650,107,988,68,1682,999,553,63,1993,36,1017,559,1579,832,1592,385,1670,126,1162,399,1624,1956,1497,358,1147,1424,1951,1380,891,1189,31,1109,645,1228,517,193,1725,1272,690,710,1211,1817,1023,439,57,1065,991,1259,607,1275,1358,1420,1262,1248,504,1611,58,66,85,1952,1981,1800,337,1574,743,189,1516,1085,1680,704,483,1524,420,816,489,1635,1365,1723,1719,234,1225,454,1175,445,1838,580,20,1258,1117,1463,1874,1914,1285,698,1649,1486,1093,1111,45,683,92,1301,1621,1478,862,896,1794,1573,1997,1992,1089,1221,853,102,1523,305,1899,1001,1942,15,10,217,992,1858,1692,1994,1765,254,1749,1593,1102,181,151,441,728,1482,216,1163,775,1131,1978,1766,791,1508,487,989,1394,721,14,415,1373,1889,720,1753,1583,456,1803,1395,1655,1342,8,1990,387,1636,138,879,498,1560,349,372,16,298,287,699,64,1045,1742,281,1374,793,1384,6,1156,1586,503,1269,1004,333,1627,849,674,1687,376,828,786,153,950,1671,183,1740,40,133,543,1646,255,407,1659,687,766,529,624,1252,1596,362,759,647,452,1689,679,91,165,1404,1908,1208,1068,108,864,1859,1546,1166,38,1947,1215,370,1724,306,889,1159,393,1727,1720,604,116,842,322,1950,1991,1698,177,1782,44,87,1645,613,1598,1240,541,994,348,589,1186,509,823,719,1699,827,1473,608,1197,1826,1383,1337,1779,966,505,1896,693,371,356,750,933,1520,620,1814,1304,388,978,476,1470,639,9,776,1013,736,627,1591,391,946,524,1072,1136,1437,927,814,239,1382,1009,220,1924,1371,1242,754,379,257,1253,1405,1328,876,1741,1748,246,1658,1400,1897,1718,706,1783,1763,1247,1633,1672,1245,859,163,1885,1945,610,1705,545,198,833,1169,1626,1837,1201,1691,1795,656,1367,1843,763,162,1489,1531,1961,1514,629,1822,1545,746,1948,1498,1325,2007,1101,851,547,637,974,884,28,1339,1619,29,1936,118,1891,993,237,1326,831,482,1321,554,576,1034,1297,403,1709,286,1019,1448,1892,467,186,632,774,982,1656,1870,1190,1199,1776,1241,1539,657,1657,426,131,1327,179,1588,1902,1239,959,1456,852,1660,780,1471,1299,1651,1079,1441,990,240,1426,401,1105,829,562,764,1278,939,995,1200,184,279,708,249,1703,1054,1138,1260,463,497,1538,481,1496,1912,1158,770,1361,1641,1450,1413,779,1849,713,586,806,1809,1647,1359,1853,732,110,1295,1550,1903,301,1644,1925,1257,1900,1605,1879,55,912,1224,1865,1484,861,332,54,236,353,197,1332,1206,43,1414,484,1873,1036,478,1825,588,1344,572,1119,259,359,1250,1714,652,418,963,285,752,1015,171,436,1026,86,1066,122,1057,1129,1580,1770,1684,735,956,1220,232,935,1505,417,1303,26,820,1237,1256,424,375,1887,1812,1966,499,1585,1745,1860,1340,274,135,1584,1116,404,1043,81,19,548,156,1172,1481,1412,111,88,1329,1184,600,2000,243,1693,1998,490,1202,392,1165,1235,292,1802,908,525,564,408,1984,105,1152,1690,1562,1423,711,1464,1145,874,382,1928,390,1398,330,641,654,1345,834,1287,256,1073,1695,1074,1960,1743,1174,1283,906,1881,1941,857,1160,1249,1280,492,1185,1541,1352,996,1436,1052,535,70,1845,1517,18,164,1112,625,1234,521,1113,1867,1366,1890,606,73,807,1686,121,920,51,148,1564,1355,1861,262,1442,96,368,867,712,594,267,551,1348,1967,430,251,943,747,1439,1866,1177,213,1360,1062,1844,269,369,1883,1637,1673,640,1261,1628,1041,277,1979,638,501,1107,1223,1829,169,1016,688,585,1700,700,1534,701,48,1399,1029,1044,1318,1209,1462,1939,1707,53,381,542,173,1678,1711,1294,962,1729,563,648,511,1721,435,578,1467,909,1320,67,1791,802,318,1121,1911,486,103,469,1716,1935,350,1824,119,1789,970,1953,260,1926,366,581,667,1930,1971,577,671,1378,1229,352,1457,1904,1640,1764,1537,893,180,1155,360,303,1333,1281,442,1477,2003,1544,82,1847,252,663,811,284,1167,1989,1832,1127,1046,1032,188,227,976,460,1008,1777,1780,231,1469,1028,934,1606,871,1876,1575,1576,423,61,751,302,894,1429,90,1270,1754,533,948,1587,1219,1012,531,187,868,869,1468,1351,214,1067,1708,1784,1768,1982,431,597,794,1440,1148,1205,7,684,835,326,1569,488,880,1452,715,796,1683,1418,538,174,347,1315,1909,705,1793,532,1522,2006,1946,1835,429,364,1726,1349,801,1662,1064,1130,311,971,1421,709,480,1296,1775,448,50,309,1218,168,380,1548,941,653,932,899,1263,605,335,1669,1807,1681,1135,1676,510,1212,413,128,1652,621,929,477,328,308,1309,1406,697,1769,1387,1443,275,294,757,1376,33,1577,1230,1713,1051,918,840,507,147,342,1506,123,49,1571,1006,425,101,1099,1504,677,206,1279,437,741,1020,1179,527,914,159,1289,1153,265,1884,27,268,1852,1298,1790,1679,1888,1227,1547,1728,1535,1075,258,1438,74,117,271,964,283,264,1943,1453,631,1987,1137,1975,960,1589,491,1999,1563,626,457,1661,140,905,516,1390,307,1483,895,724,680,628,1409,1024,1632,384,1821,1549,703,579,1425,727,738,1663,856,1618,115,1115,1919,405,731,958,936,1035,1799,783,1785,1542,1454,1251,374,1954,846,658,1731,297,1762,1630,1638,1543,566,1706,1846,365,997,21,1134,890,1317,1314,1038,341,1191,253,977,1271,464,583,1319,1140,157,1877,875,1316,1806,202,1243,854,1435,75,739,753,1128,765,1664,1666,768,30,808,1540,1196,798,282,192,1422,1818,1528,968,885,130,494,1195,1607,1922,331,804,675,1963,453,953,723,987,1595,12,1217,1772,1986,1808,1827,1231,1612,447,94,1021,1003,235,1872,883,1995,540,1512,1401,526,1139,886,863,261,1597,1323,979,969,1834,1625,1558,1210,1895,1233,127,1369,212,209,1363,466,1760,555,1608,339,508,998,568,1970,986,23,771,792,552,59,1602,400,649,984,1408,954,1688,1831,409,313,248,1040,622,224,411,1918,745,76,1264,201,1639,233,142,878,398,1415,1972,664,465,344,1273,1061,1614,662,1840,1648,815,1466,1447,266,1306,1836,824,1513,166,903,573,83,685,1274,1000,603,787,1623,1095,1324,1915,1132,726,427,575,338,185,1871,1750,1381,714,377,799,300,316,673,1578,1801,635,1730,1917,1717,1507,983,506,280,900,1291,250,1125,416,182,609,4,1526,755,1402,1810,1055,1955,1675,602,1058,1312,1216,692,1629,1313,34,125,666,312,1901,1143,1472,310,1282,78,1433,1792,1150,1307,1751,60,1561,1559,961,432,1432,601,593,1207,947,1144,767,1118,290,838,749,1875,1980,1389,830,37,650,1996,1168,665,1049,965,272,512,790,1305,1485,1377,1868,98,520,1510,386,1379,1527,315,1733,740,1905,730,1171,1495,502,569,443,1968,1572,1898,1276,414,944,898,644,340,1969,928,289,69,1244,72,41,957,1110,351,1747,587,1737,296,1702,1976,367,611,619,226,422,1796,1204,1959,35,228,1820,1027,1300,1739,1330,1336,686,537,2005,109,1554,1151,1010,923,1122,981,1198,493,1083,599,813,942,1869,343,809,3,473,230,215,1164,546,446,761,1007,1916,99,1863,630,1788,549,1921,396,141,203,682,479,1964,195,1813,80,461,1356,1771,1350,660,1446,931,1944,985,323,194,1811,397,314,79,440,758,291,1620,1097,612,46,354,1758,93,930,887,191,152,1886,1114,1226,1385,1842,1451,304,822,1634,1525,211,1427,1354,848,1103,1927,1392,514,208,324,167,65,921,1037,1581,1617,1182,56,980,707,1076,410,433,1060,373,1338,1098,389,571,229,1654,336,104,877,1570,1767,444,200,574,421,1600,781,1081,837];
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