import Iter "mo:base/Iter";
import Nat32 "mo:base/Nat32";
import Principal "mo:base/Principal";
import Result "mo:base/Result";

import Buffer "../Buffer";
import ExtCore "../toniq-labs/ext/Core";
import MarketplaceTypes "../Marketplace/Types";
import Types "Types";

module {
  public class Factory(this : Principal, deps : Types.Dependencies) {

    private let EXTENSIONS : [Types.Extension] = ["@ext/common", "@ext/nonfungible"];

    public query func getMinter() : async Principal {
      deps._Tokens.getMinter();
    };

    public query func extensions() : async [Types.Extension] {
      EXTENSIONS;
    };

    public query func supply() : async Result.Result<Types.Balance, Types.CommonError> {
      #ok(deps._Tokens.getSupply());
    };

    public query func getRegistry() : async [(Types.TokenIndex, Types.AccountIdentifier)] {
      Iter.toArray(deps._Tokens.getRegistry().entries());
      
    };

    public query func getTokens() : async [(Types.TokenIndex, Text)] {
      var resp : Buffer.Buffer<(Types.TokenIndex, Text)> = Buffer.Buffer(0);
      for(e in deps._Tokens.getTokenMetadata().entries()){
        let assetid = deps._Assets.get(Nat32.toNat(e.0)+1).name;
        resp.add((e.0, assetid));
      };
      resp.toArray();
    };
    public query func tokens(aid : Types.AccountIdentifier) : async Result.Result<[Types.TokenIndex], Types.CommonError> {
      switch(deps._Tokens.getTokensFromOwners(aid)) {
        case(?tokens) return #ok(tokens.toArray());
        case(_) return #err(#Other("No tokens"));
      };
    };
    
    public query func tokens_ext(aid : Types.AccountIdentifier) : async Result.Result<[(Types.TokenIndex, ?MarketplaceTypes.Listing, ?Blob)], Types.CommonError> {
      switch(deps._Tokens.getTokensFromOwners(aid)) {
        case(?tokens) {
          var resp : Buffer.Buffer<(Types.TokenIndex, ?Types.Listing, ?Blob)> = Buffer.Buffer(0);
          for (a in tokens.vals()){
            resp.add((a, deps._Marketplace.getListingFromTokenListing(a), null));
          };
          return #ok(resp.toArray());
        };
        case(_) return #err(#Other("No tokens"));
      };
    };
    public query func metadata(token : Types.TokenIdentifier) : async Result.Result<Types.Metadata, Types.CommonError> {
      if (ExtCore.TokenIdentifier.isPrincipal(token, this) == false) {
        return #err(#InvalidToken(token));
      };
      let tokenind = ExtCore.TokenIdentifier.getIndex(token);
      switch (deps._Tokens.getMetadataFromTokenMetadata(tokenind)) {
        case (?token_metadata) {
          return #ok(token_metadata);
        };
        case (_) {
          return #err(#InvalidToken(token));
        };
      };
    };
  }
}