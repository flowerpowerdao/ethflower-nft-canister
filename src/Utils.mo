import Array "mo:base/Array";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";

import Buffer "./Buffer";

module {
  
  /// Create a Buffer from an Array
  public func bufferFromArray<T>(array : [T]) : Buffer.Buffer<T> {
    let buffer = Buffer.Buffer<T>(array.size());
    for (element in Array.vals(array)) {
      buffer.add(element);
    };
    return buffer;
  };

  /// Clone from any iterator of key-value pairs
  public func BufferHashMapFromIter<K, V1>(
    iter : Iter.Iter<(K, [V1])>,
    initCapacity : Nat,
    keyEq : (K, K) -> Bool,
    keyHash : K -> Hash.Hash
  ) : HashMap.HashMap<K, Buffer.Buffer<V1>> {
    let h = HashMap.HashMap<K, Buffer.Buffer<V1>>(initCapacity, keyEq, keyHash);
    for ((k, v) in iter) {
      h.put(k, bufferFromArray<V1>(v));
    };
    h
  };
}