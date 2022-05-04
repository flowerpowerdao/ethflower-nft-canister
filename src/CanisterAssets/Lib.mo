import Iter "mo:base/Iter";

import Buffer "../Buffer";
import Types "Types";
import Utils "../Utils";

module {

  public class Assets (state : Types.State) {
    let _assets: Buffer.Buffer<Types.Asset> = Utils.bufferFromArray(state._assetsState);

    public func get(id: Nat) : Types.Asset {
      return _assets.get(id);
    };

    public func put(id: Nat, element: Types.Asset) {
      _assets.put(id, element);
    };

    public func add(element: Types.Asset) {
      _assets.add(element);
    };

    public func toStable() : [Types.Asset] {
      _assets.toArray()
    };

    public func size() : Nat {
      _assets.size()
    };

    public func vals() : Iter.Iter<Types.Asset> {
      _assets.vals()
    };

  }
}