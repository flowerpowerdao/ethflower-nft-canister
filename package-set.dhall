let upstream = https://github.com/dfinity/vessel-package-set/releases/download/mo-0.6.7-20210818/package-set.dhall sha256:c4bd3b9ffaf6b48d21841545306d9f69b57e79ce3b1ac5e1f63b068ca4f89957
let Package = { name : Text, version : Text, repo : Text, dependencies : List Text }

let additions = [
   { name = "asset-storage"
   , repo = "https://github.com/aviate-labs/asset-storage.mo"
   , version = "asset-storage-0.7.0"
   , dependencies = [ "base" ]
   },
   { name = "sha"
   , repo = "https://github.com/aviate-labs/sha.mo"
   , version = "v0.1.1"
   , dependencies = [ "base", "encoding" ]
   },
   { name = "encoding"
  , repo = "https://github.com/aviate-labs/encoding.mo"
  , version = "v0.2.1"
  , dependencies = ["base"]
  },
  { name = "cap"
  , repo = "https://github.com/Psychedelic/cap-motoko-library"
  , version = "v1.0.4"
  , dependencies = [] : List Text
  },
] : List Package

let overrides = [
  { name = "base"
  , repo = "https://github.com/dfinity/motoko-base"
  , version = "627ecb5a2f54c7b95a5ddd82e32cad3bdb319e6c"
  , dependencies = [] : List Text
  },
] : List Package


in  upstream # additions # overrides
