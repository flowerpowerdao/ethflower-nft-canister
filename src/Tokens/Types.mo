import Time "mo:base/Time";

import ExtCommon "../toniq-labs/Ext/Common";
import ExtCore "../toniq-labs/Ext/Core";

module {
  public type TokenIdentifier = ExtCore.TokenIdentifier;
  
  public type TokenIndex  = ExtCore.TokenIndex ;
  
  public type Metadata = ExtCommon.Metadata;
  
  public type AccountIdentifier = ExtCore.AccountIdentifier;

  public type Time = Time.Time;

  public type SubAccount = ExtCore.SubAccount;

  public type CommonError = ExtCore.CommonError;
  
  public type ICPTs = { e8s : Nat64 };

  public type State = {
    _tokenMetadataState : [(TokenIndex, Metadata)] ;
    _ownersState : [(AccountIdentifier, [TokenIndex])];
    _registryState : [(TokenIndex, AccountIdentifier)];
  }
}