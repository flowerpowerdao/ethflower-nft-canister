import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";

import AID "../toniq-labs/util/AccountIdentifier";
import Buffer "../Buffer";
import ExtCore "../toniq-labs/Ext/Core";
import Types "Types";
import Utils "../Utils";

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

    /*******************
    * INTERNAL METHODS *
    *******************/

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

    public func transferTokenToUser(tindex : Types.TokenIndex, receiver : Types.AccountIdentifier) : () {
      let owner : ?Types.AccountIdentifier = _getBearer(tindex); // who owns the token (no one if mint)
      _registry.put(tindex, receiver); // transfer the token to the new owner
      switch(owner){
        case (?o) removeFromUserTokens(tindex, o);
        case (_) {};
      };
      addToUserTokens(tindex, receiver);
    };
    
    public func removeTokenFromUser(tindex : Types.TokenIndex) : () {
      let owner : ?Types.AccountIdentifier = _getBearer(tindex);
      _registry.delete(tindex);
      switch(owner){
        case (?o) removeFromUserTokens(tindex, o);
        case (_) {};
      };
    };

    public func removeFromUserTokens(tindex : Types.TokenIndex, owner : Types.AccountIdentifier) : () {
      switch(_owners.get(owner)) {
        case(?ownersTokens) _owners.put(owner, ownersTokens.filter(func (a : Types.TokenIndex) : Bool { (a != tindex) }));
        case(_) ();
      };
    };

    public func addToUserTokens(tindex : Types.TokenIndex, receiver : Types.AccountIdentifier) : () {
      let ownersTokensNew : Buffer.Buffer<Types.TokenIndex> = switch(_owners.get(receiver)) {
        case(?ownersTokens) {ownersTokens.add(tindex); ownersTokens};
        case(_) Utils.bufferFromArray([tindex]);
      };
      _owners.put(receiver, ownersTokensNew);
    };

    func _getBearer(tindex : Types.TokenIndex) : ?Types.AccountIdentifier {
      _registry.get(tindex);
    };
  }
}