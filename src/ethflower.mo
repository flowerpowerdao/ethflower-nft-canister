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
import AssetTypes "Assets/Types";
import Assets "Assets";
import Buffer "./Buffer";
import ExtAllowance "./toniq-labs/ext/Allowance";
import ExtCommon "./toniq-labs/ext/Common";
import ExtCore "./toniq-labs/ext/Core";
import ExtNonFungible "./toniq-labs/ext/NonFungible";
import Http "Http";
import HttpTypes "Http/Types";
import Utils "./Utils";

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
  
  let LEDGER_CANISTER = actor "ryjl3-tyaaa-aaaaa-aaaba-cai" : actor { account_balance_dfx : shared query AccountBalanceArgs -> async ICPTs };
  
  // cap
  // start custom
  private stable var rootBucketId : ?Text = null;
  let _Cap = Cap.Cap(null, rootBucketId);
  let creationCycles : Nat = 1_000_000_000_000;
  // end custom
  
  private let EXTENSIONS : [Extension] = ["@ext/common", "@ext/nonfungible"];
  
  /****************
  * STABLE STATE *
  ****************/

  private stable var _registryState : [(TokenIndex, AccountIdentifier)] = [];
  private var _registry : HashMap.HashMap<TokenIndex, AccountIdentifier> = HashMap.fromIter(_registryState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

	private stable var _tokenMetadataState : [(TokenIndex, Metadata)] = [];
  private var _tokenMetadata : HashMap.HashMap<TokenIndex, Metadata> = HashMap.fromIter(_tokenMetadataState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

  private stable var _ownersState : [(AccountIdentifier, [TokenIndex])] = [];
	private var _owners : HashMap.HashMap<AccountIdentifier, Buffer.Buffer<TokenIndex>> = Utils.BufferHashMapFromIter(_ownersState.vals(), 0, AID.equal, AID.hash);
  
	private stable var _tokenListingState : [(TokenIndex, Listing)] = [];
  private var _tokenListing : HashMap.HashMap<TokenIndex, Listing> = HashMap.fromIter(_tokenListingState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

	private stable var _tokenSettlementState : [(TokenIndex, Settlement)] = [];
  private var _tokenSettlement : HashMap.HashMap<TokenIndex, Settlement> = HashMap.fromIter(_tokenSettlementState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);

	private stable var _paymentsState : [(Principal, [SubAccount])] = [];
  private var _payments : HashMap.HashMap<Principal, Buffer.Buffer<SubAccount>> = Utils.BufferHashMapFromIter(_paymentsState.vals(), 0, Principal.equal, Principal.hash);

	private stable var _refundsState : [(Principal, [SubAccount])] = [];
  private var _refunds : HashMap.HashMap<Principal, [SubAccount]> = HashMap.fromIter(_refundsState.vals(), 0, Principal.equal, Principal.hash);
  
	private stable var _usedPaymentAddressessState : [(AccountIdentifier, Principal, SubAccount)] = [];
	private var _usedPaymentAddressess : Buffer.Buffer<(AccountIdentifier, Principal, SubAccount)> = Utils.bufferFromArray<(AccountIdentifier, Principal, SubAccount)>(_usedPaymentAddressessState);

	private stable var _transactionsState : [Transaction] = [];
	private var _transactions : Buffer.Buffer<Transaction> = Utils.bufferFromArray(_transactionsState);

	private stable var _assetsState : [AssetTypes.Asset] = [];

  private var ESCROWDELAY : Time = 10 * 60 * 1_000_000_000;

  private stable var _supply : Balance  = 0;
  private stable var _minter : Principal  = init_minter;
  private stable var _nextTokenId : TokenIndex  = 0;
  // start custom
  private stable var shuffled : Bool = false;
  // end custom

  //State functions
  system func preupgrade() {
    _registryState := Iter.toArray(_registry.entries());
    _tokenMetadataState := Iter.toArray(_tokenMetadata.entries());
    _ownersState := Iter.toArray(Iter.map<(AccountIdentifier, Buffer.Buffer<TokenIndex>), (AccountIdentifier, [TokenIndex])>(
      _owners.entries(), 
      func (owner) {
        return (owner.0, owner.1.toArray());
      }));
    _tokenListingState := Iter.toArray(_tokenListing.entries());
    _tokenSettlementState := Iter.toArray(_tokenSettlement.entries());
    _paymentsState := Iter.toArray(Iter.map<(Principal, Buffer.Buffer<SubAccount>), (Principal, [SubAccount])>(
      _payments.entries(), 
      func (payment) {
        return (payment.0, payment.1.toArray());
      }));
    _refundsState := Iter.toArray(_refunds.entries());
    _whitelistState := _whitelist.toArray();
    _tokensForSaleState := _tokensForSale.toArray();
    _usedPaymentAddressessState := _usedPaymentAddressess.toArray();
    _saleTransactionsState := _saleTransactions.toArray();
    _transactionsState := _transactions.toArray();
    _failedSalesState := _failedSales.toArray();
    _assetsState := _Assets.toStable();
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
    _whitelistState := [];
    _tokensForSaleState := [];
    _usedPaymentAddressessState := [];
    _saleTransactionsState := [];
    _transactionsState := [];
    _failedSalesState := [];
    _assetsState := [];
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
	private stable var _saleTransactionsState : [SaleTransaction] = [];
	private var _saleTransactions: Buffer.Buffer<SaleTransaction> = Utils.bufferFromArray<SaleTransaction>(_saleTransactionsState);

  private stable var _salesSettlementsState : [(AccountIdentifier, Sale)] = [];
  private var _salesSettlements : HashMap.HashMap<AccountIdentifier, Sale> = HashMap.fromIter(_salesSettlementsState.vals(), 0, AID.equal, AID.hash);
  
  private stable var _failedSalesState : [(AccountIdentifier, SubAccount)] = [];
  private var _failedSales : Buffer.Buffer<(AccountIdentifier, SubAccount)> = Utils.bufferFromArray<(AccountIdentifier, SubAccount)>(_failedSalesState);

  var price : Nat64 = 500000000;
  var whitelistprice : Nat64 = 300000000;
  var saleStart : Time = 1642906800000000000;
  var whitelistEnd : Time = 1642950000000000000;
  stable var _soldIcp : Nat64 = 0;
  
  private stable var _tokensForSaleState : [TokenIndex] = [];
  private var _tokensForSale: Buffer.Buffer<TokenIndex> = Utils.bufferFromArray<TokenIndex>(_tokensForSaleState);

  private stable var _whitelistState : [AccountIdentifier] = [];
  private var _whitelist : Buffer.Buffer<AccountIdentifier> = Utils.bufferFromArray<AccountIdentifier>(_whitelistState);
  

  func nextTokens(qty : Nat64) : [TokenIndex] {
    if (_tokensForSale.size() >= Nat64.toNat(qty)) {
      let ret : Buffer.Buffer<TokenIndex> = Buffer.Buffer(Nat64.toNat(qty));
      while(ret.size() < Nat64.toNat(qty)) {        
        var token : TokenIndex = _tokensForSale.get(0);
        _tokensForSale := _tokensForSale.filter(func(x : TokenIndex) : Bool { x != token } );
        ret.add(token);
      };
      ret.toArray();
    } else {
      [];
    }
  };
  func isWhitelisted(address : AccountIdentifier) : Bool {
    Option.isSome(_whitelist.find(func (a : AccountIdentifier) : Bool { a == address }));
  };
  func removeFromWhitelist(address : AccountIdentifier) : () {
    var found : Bool = false;
    _whitelist := _whitelist.filter(func (a : AccountIdentifier) : Bool { 
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
    _whitelist.add(address);
  };
  public query(msg) func saleTransactions() : async [SaleTransaction] {
    _saleTransactions.toArray();
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
    if (Option.isSome(_usedPaymentAddressess.find(func (a : (AccountIdentifier, Principal, SubAccount)) : Bool { a.0 == paymentAddress}))) {
      return #err("Payment address has been used");
    };

    let tokens : [TokenIndex] = nextTokens(quantity);
    if (tokens.size() == 0) {
      return #err("Not enough NFTs available!");
    };
    if (tokens.size() != Nat64.toNat(quantity)) {
      _tokensForSale.append(Utils.bufferFromArray(tokens));
      return #err("Quantity error");
    };
    if (_wlr == true) {
      removeFromWhitelist(address);
    };
    
    _usedPaymentAddressess.add((paymentAddress, Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe"), subaccount));
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
                case(?p) { p.add(settlement.subaccount); p};
                case(_) Utils.bufferFromArray<SubAccount>([settlement.subaccount]);
              });
              for (a in settlement.tokens.vals()){
                _transferTokenToUser(a, settlement.buyer);
              };
              _saleTransactions.add({
                tokens = settlement.tokens;
                seller = Principal.fromText("jdfjg-amcja-wo3zr-6li5k-o4e5f-ymqfk-f4xk2-37o3d-2mezb-45y3t-5qe");
                price = settlement.price;
                buyer = settlement.buyer;
                time = Time.now();
              });
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
                  ("token_id", #Text(Utils.indexToIdentifier(settlement.tokens[0], Principal.fromActor(this)))),
                  ];
                caller = msg.caller;
              };
              ignore cap.insert(event);
              // end custom
              return #ok();
            } else {
              if (settlement.expires < Time.now()) {
                _failedSales.add((settlement.buyer, settlement.subaccount));
                _tokensForSale.append(Utils.bufferFromArray(settlement.tokens));
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
    _failedSales.toArray();
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
          if (Option.isSome(_usedPaymentAddressess.find(func (a : (AccountIdentifier, Principal, SubAccount)) : Bool { a.0 == paymentAddress}))) {
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
          _usedPaymentAddressess.add((paymentAddress, listing.seller, subaccount));
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
                case(?p) {p.add(settlement.subaccount); p};
                case(_) Utils.bufferFromArray([settlement.subaccount]);
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
              _transactions.add({
                token = tokenid;
                seller = settlement.seller;
                price = settlement.price;
                buyer = settlement.buyer;
                time = Time.now();
              });
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
  
	public shared(msg) func setMinter(minter : Principal) : async () {
		assert(msg.caller == _minter);
		_minter := minter;
	};

  // start custom
  public shared(msg) func shuffleAssets() :async () {
    assert(msg.caller == _minter and isShuffled == false);
    // get a random seed from the IC
    let seed: Blob = await Random.blob();
    // use that seed to generate a truly random number
    var randomNumber : Nat8 = Random.byteFrom(seed);
    // get the number of available assets
    var currentIndex : Nat = _Assets.size();

    // shuffle the assets array using the random beacon
    while (currentIndex != 1){
      // create a pseudo random number between 0-99
      randomNumber := Utils.prng(randomNumber);
      // use that number to calculate a random index between 0 and currentIndex
      var randomIndex : Nat = Int.abs(Float.toInt(Float.floor(Float.fromInt(Utils.fromNat8ToInt(randomNumber)* currentIndex/100))));
      assert(randomIndex < currentIndex);
      currentIndex -= 1;
      // we never want to touch the 0 index
      // as it contains the seed video
      if (randomIndex == 0) {
        randomIndex += 1;
      };
      assert((randomIndex != 0) and (currentIndex != 0));
      let temporaryValue = _Assets.get(currentIndex);
      _Assets.put(currentIndex, _Assets.get(randomIndex));
      _Assets.put(randomIndex,temporaryValue);
    };

    isShuffled := true;
  };
  // end custom

	public shared(msg) func streamAsset(id : Nat, isThumb : Bool, payload : Blob) : async () {
    assert(msg.caller == _minter);
    var asset : AssetTypes.Asset = _Assets.get(id);
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
    _Assets.put(id, asset);
  };
  public shared(msg) func updateThumb(name : Text, file : AssetTypes.File) : async ?Nat {
    assert(msg.caller == _minter);
    var i : Nat = 0;
    for(a in _Assets.vals()){
      if (a.name == name) {
        var asset : AssetTypes.Asset = _Assets.get(i);
        asset := {
          name = asset.name;
          thumbnail = ?file;
          payload = asset.payload;
          metadata = asset.metadata;
        };
        _Assets.put(i, asset);
        return ?i;
      };
      i += 1;
    };
    return null;
  };
  public shared(msg) func addAsset(asset : AssetTypes.Asset) : async Nat {
    assert(msg.caller == _minter);
    _Assets.add(asset);
    _Assets.size() - 1;
  };
  public shared(msg) func initMint() : async () {
		assert(msg.caller == _minter and _nextTokenId == 0);
    //Mint
    while(_nextTokenId < 2009) {
      _tokenMetadata.put(_nextTokenId, #nonfungible({
        // we start with asset 1, as index 0
        // contains the seed animation and is not being shuffled
        metadata = ?Utils.nat32ToBlob(_nextTokenId+1);
      }));
      _transferTokenToUser(_nextTokenId, "0000");
      _supply := _supply + 1;
      _nextTokenId := _nextTokenId + 1;
    };
    
    //Whitelist
    let whitelist_adresses = ["7ada07a0a64bff17b8e057b0d51a21e376c76607a16da88cd3f75656bc6b5b0b"];
    _whitelist := Utils.bufferFromArray<AccountIdentifier>(whitelist_adresses);
    
    //Airdrop
    var airdrop : [(AccountIdentifier, TokenIndex)] = [("05bf8280738163ef12ecb600f8a0e889738fb2808f7154b45107633c00116c18", 737)];
    for(a in airdrop.vals()){
        _transferTokenToUser(a.1, a.0);
    };
    
    //For sale
    _tokensForSale := Utils.bufferFromArray<TokenIndex>([1913,455,210,772,2008]);
	};

  /**********
  * ASSETS *
  **********/

  let _Assets = Assets.Assets({_assetsState});

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
    var resp : Buffer.Buffer<(TokenIndex, Text)> = Buffer.Buffer(0);
    for(e in _tokenMetadata.entries()){
      let assetid = _Assets.get(Nat32.toNat(e.0)+1).name;
      resp.add((e.0, assetid));
    };
    resp.toArray();
  };
  // end custom
  public query func tokens(aid : AccountIdentifier) : async Result.Result<[TokenIndex], CommonError> {
    switch(_owners.get(aid)) {
      case(?tokens) return #ok(tokens.toArray());
      case(_) return #err(#Other("No tokens"));
    };
  };
  
  public query func tokens_ext(aid : AccountIdentifier) : async Result.Result<[(TokenIndex, ?Listing, ?Blob)], CommonError> {
		switch(_owners.get(aid)) {
      case(?tokens) {
        var resp : Buffer.Buffer<(TokenIndex, ?Listing, ?Blob)> = Buffer.Buffer(0);
        for (a in tokens.vals()){
          resp.add((a, _tokenListing.get(a), null));
        };
        return #ok(resp.toArray());
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
    _transactions.toArray();
  };
  public query func settlements() : async [(TokenIndex, AccountIdentifier, Nat64)] {
    //Lock to admin?
    var result : Buffer.Buffer<(TokenIndex, AccountIdentifier, Nat64)> = Buffer.Buffer(0);
    for((token, listing) in _tokenListing.entries()) {
      if(_isLocked(token)){
        switch(_tokenSettlement.get(token)) {
          case(?settlement) {
            result.add((token, AID.fromPrincipal(settlement.seller, ?settlement.subaccount), settlement.price));
          };
          case(_) {};
        };
      };
    };
    result.toArray();
  };
  public query(msg) func payments() : async ?[SubAccount] {
    let buffer = _payments.get(msg.caller);
    switch (buffer) {
      case (?buffer) {?buffer.toArray()};
      case (_) {null};
    }
  };
  public query func listings() : async [(TokenIndex, Listing, Metadata)] {
    var results : Buffer.Buffer<(TokenIndex, Listing, Metadata)> = Buffer.Buffer(0);
    for(a in _tokenListing.entries()) {
      results.add((a.0, a.1, #nonfungible({ metadata = null })));
    };
    results.toArray();
  };
  public query(msg) func allSettlements() : async [(TokenIndex, Settlement)] {
    Iter.toArray(_tokenSettlement.entries())
  };
  public query(msg) func allPayments() : async [(Principal, [SubAccount])] {
    let transformedPayments : Iter.Iter<(Principal, [SubAccount])> = Iter.map<(Principal, Buffer.Buffer<SubAccount>), (Principal, [SubAccount])>(
      _payments.entries(), 
      func (payment) {
        return (payment.0, payment.1.toArray());
    });
    Iter.toArray(transformedPayments)
  };
  public shared(msg) func clearPayments(seller : Principal, payments : [SubAccount]) : async () {
    let removedPayments : Buffer.Buffer<SubAccount> = Buffer.Buffer(0);
    for (p in payments.vals()){
      let response : ICPTs = await LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(seller, ?p)});
      if (response.e8s < 10_000){
        removedPayments.add(p);
      };
    };
    switch(_payments.get(seller)) {
      case(?sellerPayments) {
        var newPayments : Buffer.Buffer<SubAccount> = Buffer.Buffer(0);
        for (p in sellerPayments.vals()){
          if (Option.isNull(removedPayments.find(func(a : SubAccount) : Bool {
            Array.equal(a, p, Nat8.equal);
          }))) {
            newPayments.add(p);
          };
        };
        _payments.put(seller, newPayments)
      };
      case(_){};
    };
  };

  /********
  * HTTP *
  ********/

  let _HttpHandler = Http.HttpHandler(this, {_Assets});
  
  public query func http_request_streaming_callbackrequest(request : HttpTypes.HttpRequest) : async HttpTypes.HttpResponse {
    _HttpHandler.http_request_streaming_callbackrequest(request);
  };

  public query func http_request_streaming_callback(token : HttpTypes.HttpStreamingCallbackToken) : async HttpTypes.HttpStreamingCallbackResponse {
    _HttpHandler.http_request_streaming_callback(token);
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
    var res : (Nat64, Nat64, Nat64) = Array.foldLeft<Transaction, (Nat64, Nat64, Nat64)>(_transactions.toArray(), (0,0,0), func (b : (Nat64, Nat64, Nat64), a : Transaction) : (Nat64, Nat64, Nat64) {
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