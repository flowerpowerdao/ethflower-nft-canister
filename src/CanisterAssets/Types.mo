module {
  public type File = {
    ctype : Text;//"image/jpeg"
    data : [Blob];
  };
  public type Asset = {
    name : Text;
    thumbnail : ?File;
    metadata: ?File;
    payload : File;
  };

  public type State = {
    _assetsState : [Asset]; 
  }
}