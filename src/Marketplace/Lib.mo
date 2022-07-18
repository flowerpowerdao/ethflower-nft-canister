import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
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
  public class Marketplace(this: actor { }, state : Types.State) {
    

    /*********
    * STATE *
    *********/

    private var _transactions : Buffer.Buffer<Types.Transaction> = Utils.bufferFromArray(state._transactionsState);	
    private var _tokenSettlement : HashMap.HashMap<Types.TokenIndex, Types.Settlement> = HashMap.fromIter(state._tokenSettlementState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
    private var _payments : HashMap.HashMap<Principal, Buffer.Buffer<Types.SubAccount>> = Utils.BufferHashMapFromIter(state._paymentsState.vals(), 0, Principal.equal, Principal.hash);
    private var _tokenListing : HashMap.HashMap<Types.TokenIndex, Types.Listing> = HashMap.fromIter(state._tokenListingState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
    
    public func toStable () : {
      _transactionsState : [Types.Transaction];
      _tokenSettlementState : [(Types.TokenIndex, Types.Settlement)];
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
        _tokenListingState = Iter.toArray(_tokenListing.entries());
      }
    };
    
    /*************
    * CONSTANTS *
    *************/

    let LEDGER_CANISTER = actor "ryjl3-tyaaa-aaaaa-aaaba-cai" : actor { account_balance_dfx : shared query Types.AccountBalanceArgs -> async Types.ICPTs };

    /***********************
    * GETTERS AND SETTERS *
    ***********************/
    
    public func transactionsSize () : Nat {
      _transactions.size();
    };

    public func getTransactions() : Buffer.Buffer<Types.Transaction> {
      return _transactions;
    };

    /********************
    * PUBLIC INTERFACE *
    ********************/

    public shared(msg) func settle(tokenid : Types.TokenIdentifier) : async Result.Result<(), Types.CommonError> {
      if (ExtCore.TokenIdentifier.isPrincipal(tokenid, Principal.fromActor(this)) == false) {
        return #err(#InvalidToken(tokenid));
      };
      let token = ExtCore.TokenIdentifier.getIndex(tokenid);
      switch(_tokenSettlement.get(token)) {
        case(?settlement){
          let response : Types.ICPTs = await LEDGER_CANISTER.account_balance_dfx({account = AID.fromPrincipal(settlement.seller, ?settlement.subaccount)});
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
                ignore state._Cap.insert(event);
                state._Tokens.transferTokenToUser(token, settlement.buyer);
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
  }
}