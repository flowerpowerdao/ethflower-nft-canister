import Assets "../Assets";

module {
    public type State = {
        shuffled : Bool;
        _Assets : Assets.Assets;
        minter : Principal;
    }
}