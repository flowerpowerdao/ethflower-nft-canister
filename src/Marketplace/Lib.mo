import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Time "mo:base/Time";

import Root "mo:cap/Root";

import AID "../toniq-labs/util/AccountIdentifier";
import Buffer "../Buffer";
import ExtCore "../toniq-labs/Ext/Core";
import Types "Types";
import Utils "../Utils";

module {
  public class Factory(this: Principal, state : Types.State, deps : Types.Dependencies, consts : Types.Constants ) {
    
    /*********
    * STATE *
    *********/

    private var _transactions : Buffer.Buffer<Types.Transaction> = Utils.bufferFromArray(state._transactionsState);	
    private var _tokenSettlement : HashMap.HashMap<Types.TokenIndex, Types.Settlement> = HashMap.fromIter(state._tokenSettlementState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
    private var _payments : HashMap.HashMap<Principal, Buffer.Buffer<Types.SubAccount>> = Utils.BufferHashMapFromIter(state._paymentsState.vals(), 0, Principal.equal, Principal.hash);
    private var _tokenListing : HashMap.HashMap<Types.TokenIndex, Types.Listing> = HashMap.fromIter(state._tokenListingState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
    private var _usedPaymentAddressess : Buffer.Buffer<(Types.AccountIdentifier, Principal, Types.SubAccount)> = Utils.bufferFromArray<(Types.AccountIdentifier, Principal, Types.SubAccount)>(state._usedPaymentAddressessState);
    
    public func toStable () : {
      _transactionsState : [Types.Transaction];
      _tokenSettlementState : [(Types.TokenIndex, Types.Settlement)];
      _usedPaymentAddressessState : [(Types.AccountIdentifier, Principal, Types.SubAccount)];
      _paymentsState : [(Principal, [Types.SubAccount])];
      _tokenListingState : [(Types.TokenIndex, Types.Listing)];
    } {
      return {
        _tokenSettlementState = Iter.toArray(_tokenSettlement.entries());
        _transactionsState = _transactions.toArray();
        _paymentsState = Iter.toArray(Iter.map<(Principal, Buffer.Buffer<Types.SubAccount>), (Principal, [Types.SubAccount])>(
          _payments.entries(), 
          func (payment) {
            return (payment.0, payment.1.toArray());
        }));
        _usedPaymentAddressessState = _usedPaymentAddressess.toArray();
        _tokenListingState = Iter.toArray(_tokenListing.entries());
      }
    };
    
    /*************
    * CONSTANTS *
    *************/


    /********************
    * PUBLIC INTERFACE *
    ********************/

    public shared(msg) func lock(tokenid : Types.TokenIdentifier, price : Nat64, address : Types.AccountIdentifier, subaccount : Types.SubAccount) : async Result.Result<Types.AccountIdentifier, Types.CommonError> {
      if (ExtCore.TokenIdentifier.isPrincipal(tokenid, this) == false) {
        return #err(#InvalidToken(tokenid));
      };
      var c : Nat = 0;
      var failed : Bool = true;
      while(c < 29) {
        if (failed) {
          if (subaccount[c] > 0) { 
            failed := false;
          };
        };
        c += 1;
      };
      if (failed) {
        return #err(#Other("Invalid subaccount"));
      };
      if (subaccount.size() != 32) {
        return #err(#Other("Wrong subaccount"));				
      };
      let token = ExtCore.TokenIdentifier.getIndex(tokenid);
      if (_isLocked(token)) {					
        return #err(#Other("Listing is locked"));				
      };
      switch(_tokenListing.get(token)) {
        case (?listing) {
          if (listing.price != price) {
            return #err(#Other("Price has changed!"));
          } else {
            let paymentAddress : Types.AccountIdentifier = AID.fromPrincipal(listing.seller, ?subaccount);
            if (Option.isSome(_usedPaymentAddressess.find(func (a : (Types.AccountIdentifier, Principal, Types.SubAccount)) : Bool { a.0 == paymentAddress}))) {
              return #err(#Other("Payment address has been used"));
            };
            _tokenListing.put(token, {
              seller = listing.seller;
              price = listing.price;
              locked = ?(Time.now() + consts.ESCROWDELAY);
            });
            switch(_tokenSettlement.get(token)) {
              case(?settlement){
                let resp : Result.Result<(), Types.CommonError> = await settle(tokenid);
                switch(resp) {
                  case(#ok) {
                    return #err(#Other("Listing as sold"));
                  };
                  case(#err _) {
                    //If settled outside of here...
                    if (Option.isNull(_tokenListing.get(token))) return #err(#Other("Listing as sold"));
                  };
                };
              };
              case(_){};
            };
            _usedPaymentAddressess.add((paymentAddress, listing.seller, subaccount));
            _tokenSettlement.put(token, {
              seller = listing.seller;
              price = listing.price;
              subaccount = subaccount;
              buyer = address;
            });
            return #ok(paymentAddress);
          };
        };
        case (_) {
          return #err(#Other("No listing!"));				
        };
      };
    };

    public shared(msg) func settle(tokenid : Types.TokenIdentifier) : async Result.Result<(), Types.CommonError> {
      if (ExtCore.TokenIdentifier.isPrincipal(tokenid, this) == false) {
        return #err(#InvalidToken(tokenid));
      };
      let token = ExtCore.TokenIdentifier.getIndex(tokenid);
      switch(_tokenSettlement.get(token)) {
        case(?settlement){
          let response : Types.ICPTs = await consts.LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(settlement.seller, ?settlement.subaccount)});
          switch(_tokenSettlement.get(token)) {
            case(?settlement){
              if (response.e8s >= settlement.price){
                //We can settle!
                _payments.put(settlement.seller, switch(_payments.get(settlement.seller)) {
                  case(?p) {p.add(settlement.subaccount); p};
                  case(_) Utils.bufferFromArray([settlement.subaccount]);
                });
                let event : Root.IndefiniteEvent = {
                        operation = "sale";
                        details = [
                          ("to", #Text(settlement.buyer)),
                          ("from", #Principal(settlement.seller)),
                          ("price_decimals", #U64(8)),
                          ("price_currency", #Text("ICP")),
                          ("price", #U64(settlement.price)),
                          ("token_id", #Text(tokenid))
                        ];
                        caller = msg.caller;
                };
                ignore deps._Cap.insert(event);
                deps._Tokens.transferTokenToUser(token, settlement.buyer);
                _transactions.add({
                  token = tokenid;
                  seller = settlement.seller;
                  price = settlement.price;
                  buyer = settlement.buyer;
                  time = Time.now();
                });
                _tokenListing.delete(token);
                _tokenSettlement.delete(token);
                return #ok();
              } else {
                return #err(#Other("Insufficient funds sent"));
              };
            };
            case(_) return #err(#Other("Nothing to settle"));
          };
        };
        case(_) return #err(#Other("Nothing to settle"));
      };
    };

    public shared(msg) func list(request: Types.ListRequest) : async Result.Result<(), Types.CommonError> {
      if (ExtCore.TokenIdentifier.isPrincipal(request.token, this) == false) {
        return #err(#InvalidToken(request.token));
      };
      let token = ExtCore.TokenIdentifier.getIndex(request.token);
      if (_isLocked(token)) {					
        return #err(#Other("Listing is locked"));				
      };
      switch(_tokenSettlement.get(token)) {
        case(?settlement){
          let resp : Result.Result<(), Types.CommonError> = await settle(request.token);
          switch(resp) {
            case(#ok) return #err(#Other("Listing as sold"));
            case(#err _) {};
          };
        };
        case(_){};
      };
      let owner = AID.fromPrincipal(msg.caller, request.from_subaccount);
      switch (deps._Tokens.getOwnerFromRegistry(token)) {
        case (?token_owner) {
          if(AID.equal(owner, token_owner) == false) {
            return #err(#Other("Not authorized"));
          };
          switch(request.price) {
            case(?price) {
              _tokenListing.put(token, {
                seller = msg.caller;
                price = price;
                locked = null;
              });
            };
            case(_) {
              _tokenListing.delete(token);
            };
          };
          if (Option.isSome(_tokenSettlement.get(token))) {
            _tokenSettlement.delete(token);
          };
          return #ok;
        };
        case (_) {
          return #err(#InvalidToken(request.token));
        };
      };
    };

    public query func details(token : Types.TokenIdentifier) : async Result.Result<(Types.AccountIdentifier, ?Types.Listing), Types.CommonError> {
      if (ExtCore.TokenIdentifier.isPrincipal(token, this) == false) {
        return #err(#InvalidToken(token));
      };
      let tokenind = ExtCore.TokenIdentifier.getIndex(token);
      switch (deps._Tokens.getBearer(tokenind)) {
        case (?token_owner) {
          return #ok((token_owner, _tokenListing.get(tokenind)));
        };
        case (_) {
          return #err(#InvalidToken(token));
        };
      };
    };

    public query func transactions() : async [Types.Transaction] {
      _transactions.toArray();
    };

    public query func settlements() : async [(Types.TokenIndex, Types.AccountIdentifier, Nat64)] {
      //Lock to admin?
      var result : Buffer.Buffer<(Types.TokenIndex, Types.AccountIdentifier, Nat64)> = Buffer.Buffer(0);
      for((token, listing) in _tokenListing.entries()) {
        if(_isLocked(token)){
          switch(_tokenSettlement.get(token)) {
            case(?settlement) {
              result.add((token, AID.fromPrincipal(settlement.seller, ?settlement.subaccount), settlement.price));
            };
            case(_) {};
          };
        };
      };
      result.toArray();
    };

    public query(msg) func payments() : async ?[Types.SubAccount] {
      let buffer = _payments.get(msg.caller);
      switch (buffer) {
        case (?buffer) {?buffer.toArray()};
        case (_) {null};
      }
    };

    public query func listings() : async [(Types.TokenIndex, Types.Listing, Types.Metadata)] {
      var results : Buffer.Buffer<(Types.TokenIndex, Types.Listing, Types.Metadata)> = Buffer.Buffer(0);
      for(a in _tokenListing.entries()) {
        results.add((a.0, a.1, #nonfungible({ metadata = null })));
      };
      results.toArray();
    };

    public query(msg) func allSettlements() : async [(Types.TokenIndex, Types.Settlement)] {
      Iter.toArray(_tokenSettlement.entries())
    };

    public query(msg) func allPayments() : async [(Principal, [Types.SubAccount])] {
      let transformedPayments : Iter.Iter<(Principal, [Types.SubAccount])> = Iter.map<(Principal, Buffer.Buffer<Types.SubAccount>), (Principal, [Types.SubAccount])>(
        _payments.entries(), 
        func (payment) {
          return (payment.0, payment.1.toArray());
      });
      Iter.toArray(transformedPayments)
    };

    public shared(msg) func clearPayments(seller : Principal, payments : [Types.SubAccount]) : async () {
      let removedPayments : Buffer.Buffer<Types.SubAccount> = Buffer.Buffer(0);
      for (p in payments.vals()){
        let response : Types.ICPTs = await consts.LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(seller, ?p)});
        if (response.e8s < 10_000){
          removedPayments.add(p);
        };
      };
      switch(_payments.get(seller)) {
        case(?sellerPayments) {
          var newPayments : Buffer.Buffer<Types.SubAccount> = Buffer.Buffer(0);
          for (p in sellerPayments.vals()){
            if (Option.isNull(removedPayments.find(func(a : Types.SubAccount) : Bool {
              Array.equal(a, p, Nat8.equal);
            }))) {
              newPayments.add(p);
            };
          };
          _payments.put(seller, newPayments)
        };
        case(_){};
      };
    };

    /***********************
    * GETTERS AND SETTERS *
    ***********************/
    
    public func transactionsSize () : Nat {
      _transactions.size();
    };

    public func getTransactions() : Buffer.Buffer<Types.Transaction> {
      return _transactions;
    };

    public func tokenListingSize() : Nat {
      return _tokenListing.size();
    };

    public func putPayments(principal: Principal, payments: Buffer.Buffer<Types.SubAccount>) {
      return _payments.put(principal, payments);
    };

    public func getPayments(principal: Principal) : ?Buffer.Buffer<Types.SubAccount>{
      return _payments.get(principal);
    };

    public func findUsedPaymentAddress(paymentAddress : Types.AccountIdentifier) : ?(Types.AccountIdentifier, Principal, Types.SubAccount) {
      return _usedPaymentAddressess.find(
        func (a : (Types.AccountIdentifier, Principal, Types.SubAccount)) : Bool { 
          a.0 == paymentAddress
        }
      );
    };

    public func addUsedPaymentAddress(paymentAddress : Types.AccountIdentifier, principal: Principal, subaccount: Types.SubAccount) {
      _usedPaymentAddressess.add((paymentAddress, principal, subaccount));
    };

    func _isLocked(token : Types.TokenIndex) : Bool {
      switch(_tokenListing.get(token)) {
        case(?listing){
          switch(listing.locked) {
            case(?time) {
              if (time > Time.now()) {
                return true;
              } else {					
                return false;
              }
            };
            case(_) {
              return false;
            };
          };
        };
        case(_) return false;
      };
    };

  }
}