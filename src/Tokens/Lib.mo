import Types "Types";
import HashMap "mo:base/HashMap";
import ExtCore "../toniq-labs/Ext/Core";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Buffer "../Buffer";
import Utils "../Utils";
import AID "../toniq-labs/util/AccountIdentifier";

module {
  public class Tokens(this : actor { }, state : Types.State) {

    /*********
    * STATE *
    *********/

    private var _tokenMetadata : HashMap.HashMap<Types.TokenIndex, Types.Metadata> = HashMap.fromIter(state._tokenMetadataState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
    private var _owners : HashMap.HashMap<Types.AccountIdentifier, Buffer.Buffer<Types.TokenIndex>> = Utils.BufferHashMapFromIter(state._ownersState.vals(), 0, AID.equal, AID.hash);
    private var _registry : HashMap.HashMap<Types.TokenIndex, Types.AccountIdentifier> = HashMap.fromIter(state._registryState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
    
    public func toStable() : {
      _tokenMetadataState : [(Types.TokenIndex, Types.Metadata)];
      _ownersState : [(Types.AccountIdentifier, [Types.TokenIndex])];
      _registryState : [(Types.TokenIndex, Types.AccountIdentifier)];
      } {
      return {
        _tokenMetadataState = Iter.toArray(_tokenMetadata.entries());
        _ownersState = Iter.toArray(Iter.map<(Types.AccountIdentifier, Buffer.Buffer<Types.TokenIndex>), (Types.AccountIdentifier, [Types.TokenIndex])>(
          _owners.entries(), 
          func (owner) {
            return (owner.0, owner.1.toArray());
        }));
        _registryState = Iter.toArray(_registry.entries());
      }
    };

    public func getTokenDataFromIndex(tokenind: Nat32) : ?Blob {
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

    public func getTokenData(token : Text) : ?Blob {
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
  }
}