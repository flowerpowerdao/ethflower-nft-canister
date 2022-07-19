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
import AssetTypes "CanisterAssets/Types";
import Assets "CanisterAssets";
import Buffer "./Buffer";
import ExtAllowance "./toniq-labs/ext/Allowance";
import ExtCommon "./toniq-labs/ext/Common";
import ExtCore "./toniq-labs/ext/Core";
import ExtNonFungible "./toniq-labs/ext/NonFungible";
import Http "Http";
import HttpTypes "Http/Types";
import Marketplace "Marketplace";
import Sale "Sale";
import SaleTypes "Sale/Types";
import Shuffle "Shuffle";
import Tokens "Tokens";
import Utils "./Utils";

shared ({ caller = init_minter}) actor class Canister(cid: Principal) = myCanister {

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

	private stable var _transactionsState : [Transaction] = [];
	private var _transactions : Buffer.Buffer<Transaction> = Utils.bufferFromArray(_transactionsState);

// sale
	private stable var _saleTransactionsState : [SaleTypes.SaleTransaction] = [];

  private stable var _salesSettlementsState : [(AccountIdentifier, SaleTypes.Sale)] = [];
  
  private stable var _failedSalesState : [(AccountIdentifier, SubAccount)] = [];

  private stable var _tokensForSaleState : [TokenIndex] = [];

  private stable var _whitelistState : [AccountIdentifier] = [];

  private stable var _soldIcpState : Nat64 = 0;
 
//
	private stable var _assetsState : [AssetTypes.Asset] = [];

  private stable var _supplyState : Balance  = 0;
  private stable var _minterState : Principal  = init_minter;
  private stable var _nextTokenIdState : TokenIndex  = 0;

  private stable var _isShuffledState : Bool = false;

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

    // is this really overwriting the state?
    let { 
      _saleTransactionsState; 
      _salesSettlementsState;
      _failedSalesState; 
      _tokensForSaleState; 
      _whitelistState;
      _soldIcpState;
    } = _Sale.toStable();

    let {
      _assetsState;
    } = _Assets.toStable();
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

  /*************
  * CONSTANTS *
  *************/

  let ESCROWDELAY : Time.Time = 10 * 60 * 1_000_000_000;
  let LEDGER_CANISTER = actor "ryjl3-tyaaa-aaaaa-aaaba-cai" : actor { account_balance_dfx : shared query AccountBalanceArgs -> async ICPTs };

  /***********
  * CLASSES *
  ***********/
  
// Tokens
  let _Tokens = Tokens.Factory(
    cid,
    {
      _minterState;
      _nextTokenIdState;
      _registryState;
      _tokenMetadataState;
      _supplyState;
      _ownersState;
    }
  );
    
  public query func balance(request : Types.BalanceRequest) : async Types.BalanceResponse {

  public shared (msg) func setMinter(minter: Principal) {
    _Tokens.setMinter(msg.caller, minter);
  }


// Marketplace
  let _Marketplace = Marketplace.Factory(
    cid,
    {
      _paymentsState;
      _tokenListingState;
      _tokenSettlementState;
      _transactionsState;
      _usedPaymentAddressessState
    },
    {
      _Tokens;
      _Cap;
    },
    {
      ESCROWDELAY;
      LEDGER_CANISTER;
    }
  );

//Sale 
  let _Sale = Sale.Factory(
    cid,
    {
      _minterState;
      _whitelistState;
      _tokensForSaleState;
      _usedPaymentAddressessState;
      _saleTransactionsState;
      _transactionsState;
      _failedSalesState;
      _salesSettlementsState;
      _soldIcpState;
    },
    {
      _Cap;
      _Marketplace;
      _Tokens;
    },
    {
      ESCROWDELAY;
      LEDGER_CANISTER;
    }
  );
  

  
  public shared(msg) func initCap() : async Result.Result<(), Text> {
    assert(msg.caller == _minterState);
    let pid = Principal.fromActor(myCanister);
    let tokenContractId = Principal.toText(pid);

    try {
        rootBucketId := await _Cap.handshake(
            tokenContractId,
            creationCycles
        );

        return #ok();
    } catch e {
        throw e;
    };
  };

  /**********
  * ASSETS *
  **********/

  let _Assets = Assets.Assets(
    {
      _assetsState;
      _isShuffledState;
    },
    {
      _Tokens
    }
  );
  
  /***********
  * SHUFFLE *
  ***********/

  let _Shuffle = Shuffle.Shuffle(
    {
      _isShuffledState;
    },
    {
      _Assets;
      _Tokens;
    }
  );

  /********
  * HTTP *
  ********/

  let _HttpHandler = Http.HttpHandler(
    cid,
    {
      _Assets; 
      _Marketplace; 
      _Shuffle; 
      _Tokens
    }
  );

  
  public query func getMinter() : async Principal {
    _Tokens.getMinter();
  };
  public query func extensions() : async [Extension] {
    EXTENSIONS;
  };

  // start custom
  public query func supply() : async Result.Result<Balance, CommonError> {
  // end custom
    #ok(_Tokens.getSupply());
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
    if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(myCanister)) == false) {
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