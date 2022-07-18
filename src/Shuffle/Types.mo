import Assets "../CanisterAssets";
import Tokens "../Tokens";

module {
    public type State = {
        _isShuffledState : Bool;
    };

    public type Dependencies = {
        _Assets : Assets.Assets;
        _Tokens : Tokens.Factory;
    };
}