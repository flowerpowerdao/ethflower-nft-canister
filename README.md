# eth flower nft 🌼

## quick deploy 🏃‍♀️

-   To quickly reinstall (**WIPES ALL STATE**) the NFT staging canister locally run `dfx deploy --argument '(principal "<your_principal>")' staging --mode reinstall`

## caveats 🕳

-   The canister code is written in a way that the seed animation _ALWAYS_ has to be the first asset uploaded to the canister.
-   The `deploy.zsh` adds another oracle to the NFT canister because the script in the source SVG won't be executed the way it's currently structured.

## vessel 🚢

-   Run `vessel verify --version 0.6.21` to verify everything still builds correctly after adding a new depdenceny
-   To use `vessels`s moc version when deploying, use `DFX_MOC_PATH="$(vessel bin)/moc" dfx deploy`

## off-chain backup ⛓

We use the `getRegistry` and `getTokens` canister methods to backup state offchain. Therefore we simply use a script that queries the afore mentioned methods every 60 minutes and saves the responses on a server. You can find the script in `state_backup`. We are also submitting every transaction to `CAP`, which again offers off-chain backups of their data.

Note that the indices of the json outputs represent the indices of the internal storage. E.g. index `0` means it is the first item in the array. In the UI (entrepot or stoic wallet) those indices are incremented by one, so they start with `1` and not with `0`.

To have the same token identifiers for the same tokens, it is important to keep the order of the minting when reinstantiating the canister.

So when executing `mintNFT`, the `to` address is taken from `registry.json` and the `asset` is taken from `tokens.json`. It's important here that the uploading of the assets is on order (start with flower 1, end with flower 2009) and that the `assets` index 0 is used by something other than an NFT asset (before it was the seed animation)! It's also crucial to remove `shuffleAssets` functionality from the canister!

## deploy 🚀

---

**WARNING**

you need to use `moc 0.6.20` and the `master` of `base-library` for this to compile!

---

https://forum.dfinity.org/t/how-to-get-access-to-prim-prinicipalofblob/10290

deploy the canister with

```
dfx deploy --argument '(principal "3hlbk-k7klp-u4z66-ejvtg-cef6i-aurmy-4umv3-cdtno-uihny-ifis6-dqe")' --no-wallet
```

use the following command to upload an asset that fits into a single message

```
dfx canister call btcflower addAsset '(record {name = "privat";payload = record {ctype = "text/html"; data = vec {blob "hello world!"} } })'
```

use the following command to mint a token

```
dfx canister call btcflower mintNFT '(record {to = "75c52c5ee26d10c7c3da77ec7bc2b4c75e1fdc2b92e01d3da6986ba67cfa1703"; asset = 0 : nat32})'
```

run icx-proxy to be able to user query parameters locally

```
$(dfx cache show)/icx-proxy --address 127.0.0.1:8453 -vv
```

add the following line to `/etc/hosts` if on mac

```
127.0.0.1       rrkah-fqaaa-aaaaa-aaaaq-cai.localhost
```

the canister can now be accesed with

```
http://rwlgt-iiaaa-aaaaa-aaaaa-cai.localhost:8453/?tokenid=rwlgt-iiaaa-aaaaa-aaaaa-cai
```

or via command line with

```
curl "rwlgt-iiaaa-aaaaa-aaaaa-cai.localhost:8453/?tokenid=rwlgt-iiaaa-aaaaa-aaaaa-cai"
```

to get the tokenid from the canister and index do the following

1. clone https://github.com/Toniq-Labs/ext-cli and https://github.com/Toniq-Labs/ext-js in the same directory
2. run `npm i -g` from within `ext-cli`
3. run `ext token <canister_id> <index>`
