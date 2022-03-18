import Array "mo:base/Array";

import Buffer "./Buffer";

module {
  public func bufferFromArray<T>(array : [T]) : Buffer.Buffer<T> {
    let buffer = Buffer.Buffer<T>(array.size());
    for (element in Array.vals(array)) {
      buffer.add(element);
    };
    return buffer;
  };
}