import Float "mo:base/Float";
import Int "mo:base/Int";
import Random "mo:base/Random";

import Types "Types";
import Utils "../Utils";

module {
  public class Shuffle (state : Types.State) {
    private var shuffled = state.shuffled;

    public func toStable () : Bool {
        shuffled
    };

    public func isShuffled() : Bool {
        shuffled
    };

    public func shuffleAssets(caller : Principal) :async () {
      assert(caller == state.minter and shuffled == false);
      // get a random seed from the IC
      let seed: Blob = await Random.blob();
      // use that seed to generate a truly random number
      var randomNumber : Nat8 = Random.byteFrom(seed);
      // get the number of available assets
      var currentIndex : Nat = state._Assets.size();

      // shuffle the assets array using the random beacon
      while (currentIndex != 1){
        // create a pseudo random number between 0-99
        randomNumber := Utils.prng(randomNumber);
        // use that number to calculate a random index between 0 and currentIndex
        var randomIndex : Nat = Int.abs(Float.toInt(Float.floor(Float.fromInt(Utils.fromNat8ToInt(randomNumber)* currentIndex/100))));
        assert(randomIndex < currentIndex);
        currentIndex -= 1;
        // we never want to touch the 0 index
        // as it contains the seed video
        if (randomIndex == 0) {
          randomIndex += 1;
        };
        assert((randomIndex != 0) and (currentIndex != 0));
        let temporaryValue = state._Assets.get(currentIndex);
        state._Assets.put(currentIndex, state._Assets.get(randomIndex));
        state._Assets.put(randomIndex,temporaryValue);
      };

      shuffled := true;
    };

  };
}