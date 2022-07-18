import Array "mo:base/Array";
import Float "mo:base/Float";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Random "mo:base/Random";

import Buffer "../Buffer";
import Types "Types";
import Utils "../Utils";

module {

  public class Assets (state : Types.State, deps : Types.Dependencies) {
    
    /*********
    * STATE *
    *********/

    private var _assets: Buffer.Buffer<Types.Asset> = Utils.bufferFromArray(state._assetsState);

    public func toStable() : {
      _assetsState : [Types.Asset]; 
    } {
      return {
        _assetsState = _assets.toArray();
      }
    };

    /********************
    * PUBLIC INTERFACE *
    ********************/

    public shared(msg) func streamAsset(id : Nat, isThumb : Bool, payload : Blob) : async () {
      assert(msg.caller == deps._Tokens.getMinter());
      var asset : Types.Asset = _assets.get(id);
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
      _assets.put(id, asset);
    };

    public shared(msg) func updateThumb(name : Text, file : Types.File) : async ?Nat {
      assert(msg.caller == deps._Tokens.getMinter());
      var i : Nat = 0;
      for(a in _assets.vals()){
        if (a.name == name) {
          var asset : Types.Asset = _assets.get(i);
          asset := {
            name = asset.name;
            thumbnail = ?file;
            payload = asset.payload;
            metadata = asset.metadata;
          };
          _assets.put(i, asset);
          return ?i;
        };
        i += 1;
      };
      return null;
    };

    public shared(msg) func addAsset(asset : Types.Asset) : async Nat {
      assert(msg.caller == deps._Tokens.getMinter());
      _assets.add(asset);
      _assets.size() - 1;
    };

    /*******************
    * INTERNAL METHODS *
    *******************/

    public func get(id: Nat) : Types.Asset {
      return _assets.get(id);
    };

    public func put(id: Nat, element: Types.Asset) {
      _assets.put(id, element);
    };

    public func add(element: Types.Asset) {
      _assets.add(element);
    };

    public func size() : Nat {
      _assets.size()
    };

    public func vals() : Iter.Iter<Types.Asset> {
      _assets.vals()
    };

  }
}