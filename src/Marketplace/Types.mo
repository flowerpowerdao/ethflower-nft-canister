import Time "mo:base/Time";

import ExtCore "../toniq-labs/Ext/Core";
import TokenTypes "../Tokens/Types";
import Tokens "../Tokens";
import Cap "mo:cap/Cap";

module {
  public type AccountIdentifier = ExtCore.AccountIdentifier;

  public type Time = Time.Time;

  public type TokenIdentifier = TokenTypes.TokenIdentifier;

  public type SubAccount = ExtCore.SubAccount;

  public type CommonError = ExtCore.CommonError;

  public type TokenIndex  = ExtCore.TokenIndex ;
  
  public type ICPTs = { e8s : Nat64 };

  public type Transaction = {
    token : TokenIdentifier;
    seller : Principal;
    price : Nat64;
    buyer : AccountIdentifier;
    time : Time;
  };

  public type Settlement = {
    seller : Principal;
    price : Nat64;
    subaccount : SubAccount;
    buyer : AccountIdentifier;
  };

  public type Listing = {
    seller : Principal;
    price : Nat64;
    locked : ?Time;
  };

  public type ListRequest = {
    token : TokenIdentifier;
    from_subaccount : ?SubAccount;
    price : ?Nat64;
  };

  public type AccountBalanceArgs = { account : AccountIdentifier };

  public type State = {
    _transactionsState : [Transaction];
    _tokenSettlementState : [(TokenIndex, Settlement)];
    _paymentsState : [(Principal, [SubAccount])];
    _tokenListingState : [(TokenIndex, Listing)];
    _Cap : Cap.Cap;
    _Tokens : Tokens.Tokens;
  }

}