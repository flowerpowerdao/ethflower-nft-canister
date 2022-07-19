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
import Assets "CanisterAssets";
import AssetsTypes "CanisterAssets/Types";
import Buffer "./Buffer";
import EXT "Ext";
import ExtAllowance "./toniq-labs/ext/Allowance";
import ExtCommon "./toniq-labs/ext/Common";
import ExtCore "./toniq-labs/ext/Core";
import ExtNonFungible "./toniq-labs/ext/NonFungible";
import Http "Http";
import HttpTypes "Http/Types";
import Marketplace "Marketplace";
import MarketplaceTypes "Marketplace/Types";
import Sale "Sale";
import SaleTypes "Sale/Types";
import Shuffle "Shuffle";
import TokenTypes "Tokens/Types";
import Tokens "Tokens";
import Utils "./Utils";

shared ({ caller = init_minter}) actor class Canister(cid: Principal) = myCanister {

/*********
* TYPES *
*********/
  type AccountIdentifier = ExtCore.AccountIdentifier;
  type AccountBalanceArgs = { account : AccountIdentifier };
  type ICPTs = { e8s : Nat64 };
  
  
/****************
* STABLE STATE *
****************/

 // Tokens
	private stable var _tokenMetadataState : [(TokenTypes.TokenIndex, TokenTypes.Metadata)] = [];
  private stable var _ownersState : [(AccountIdentifier, [TokenTypes.TokenIndex])] = [];
  private stable var _registryState : [(TokenTypes.TokenIndex, AccountIdentifier)] = [];
  private stable var _nextTokenIdState : TokenTypes.TokenIndex  = 0;
  private stable var _minterState : Principal  = init_minter;
  private stable var _supplyState : TokenTypes.Balance  = 0;

 // Sale
	private stable var _saleTransactionsState : [SaleTypes.SaleTransaction] = [];
  private stable var _salesSettlementsState : [(AccountIdentifier, SaleTypes.Sale)] = [];
  private stable var _failedSalesState : [(AccountIdentifier, TokenTypes.SubAccount)] = [];
  private stable var _tokensForSaleState : [TokenTypes.TokenIndex] = [];
  private stable var _whitelistState : [AccountIdentifier] = [];
  private stable var _soldIcpState : Nat64 = 0;

 // Marketplace
	private stable var _transactionsState : [MarketplaceTypes.Transaction] = [];
	private stable var _tokenSettlementState : [(TokenTypes.TokenIndex, MarketplaceTypes.Settlement)] = [];
	private stable var _usedPaymentAddressessState : [(AccountIdentifier, Principal, TokenTypes.SubAccount)] = [];
	private stable var _paymentsState : [(Principal, [TokenTypes.SubAccount])] = [];
	private stable var _tokenListingState : [(TokenTypes.TokenIndex, MarketplaceTypes.Listing)] = [];

 // Assets
	private stable var _assetsState : [AssetsTypes.Asset] = [];

 // Shuffle
  private stable var _isShuffledState : Bool = false;

 // Cap
  private stable var rootBucketId : ?Text = null;

 //State functions
  system func preupgrade() {
   // Tokens  
    let {
      _tokenMetadataState;
      _ownersState;
      _registryState;
      _nextTokenIdState;
      _minterState;
      _supplyState;
    } = _Tokens.toStable();

   // Sale
    let { 
      _saleTransactionsState; 
      _salesSettlementsState;
      _failedSalesState; 
      _tokensForSaleState; 
      _whitelistState;
      _soldIcpState;
    } = _Sale.toStable();
  
   // Marketplace
    let { 
      _transactionsState; 
      _tokenSettlementState; 
      _usedPaymentAddressessState; 
      _paymentsState; 
      _tokenListingState; 
    } = _Marketplace.toStable();

   // Assets
    let {
      _assetsState;
    } = _Assets.toStable();
  };

  system func postupgrade() {
   // Tokens
    _tokenMetadataState := [];
    _ownersState := [];
    _registryState := [];
    _nextTokenIdState := 0;
    _minterState := init_minter;
    _supplyState := 0;

   // Sale
    _saleTransactionsState := [];
    _salesSettlementsState := [];
    _failedSalesState := [];
    _tokensForSaleState := [];
    _whitelistState := [];
    _soldIcpState := 0;

   // Marketplace
    _transactionsState := [];
    _tokenSettlementState := [];
    _usedPaymentAddressessState := [];
    _paymentsState := [];
    _tokenListingState := [];

   // Assets
    _assetsState := [];
  };

/*************
* CONSTANTS *
*************/

  let ESCROWDELAY : Time.Time = 10 * 60 * 1_000_000_000;
  let LEDGER_CANISTER = actor "ryjl3-tyaaa-aaaaa-aaaba-cai" : actor { account_balance_dfx : shared query AccountBalanceArgs -> async ICPTs };
  let CREATION_CYCLES: Nat = 1_000_000_000_000;

/***********
* CLASSES *
***********/

 // Cap
  let _Cap = Cap.Cap(null, rootBucketId);

  public shared(msg) func initCap() : async Result.Result<(), Text> {
    assert(msg.caller == _minterState);
    let pid = Principal.fromActor(myCanister);
    let tokenContractId = Principal.toText(pid);

    try {
        rootBucketId := await _Cap.handshake(
            tokenContractId,
            CREATION_CYCLES,
        );

        return #ok();
    } catch e {
        throw e;
    };
  };
  
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

  public shared (msg) func setMinter(minter: Principal) {
    _Tokens.setMinter(msg.caller, minter);
  };
    
  public query func balance(request : TokenTypes.BalanceRequest) : async TokenTypes.BalanceResponse {
    _Tokens.balance(request);
  };

  public query func bearer(token : TokenTypes.TokenIdentifier) : async Result.Result<TokenTypes.AccountIdentifier, TokenTypes.CommonError> {
    _Tokens.bearer(token);
  };


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
  

  

/**********
* ASSETS *
**********/

  let _Assets = Assets.Factory(
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
  
  public query func http_request_streaming_callbackrequest(request : HttpTypes.HttpRequest) : async HttpTypes.HttpResponse {
    _HttpHandler.http_request_streaming_callbackrequest(request);
  };

  public query func http_request_streaming_callback(token : HttpTypes.HttpStreamingCallbackToken) : async HttpTypes.HttpStreamingCallbackResponse {
    _HttpHandler.http_request_streaming_callback(token);
  };

/**********
* CYCLES *
**********/

  public func acceptCycles() : async () {
    let available = Cycles.available();
    let accepted = Cycles.accept(available);
    assert (accepted == available);
  };

  public query func availableCycles() : async Nat {
    return Cycles.balance();
  };
  
}