#!/bin/zsh
# stream asset to the canister using this
# https://gist.github.com/jorgenbuilder/6d32ef665b84457a9be2063224b754fb
file=${1}
filename=$(echo $file | sed -E "s/.+\///")
fileextension=$(echo $file | sed -E "s/.+\.//")
mime="video/$fileextension"
network=${2}
number_of_assets=${3}
mode=${4}
threshold="100000"

# reset the canister state
if [[ "$mode" == "production" ]]
then
echo "production deployment ..."
dfx deploy --network $network --argument '(principal "cbvco-k27pa-dgumq-tjhcq-iqrcx-ayr3z-moywz-jqblc-nvsif-dayv3-4qe")' --no-wallet $mode
else
echo "staging deployment ..."
yes yes| dfx deploy --network $network --argument '(principal "cbvco-k27pa-dgumq-tjhcq-iqrcx-ayr3z-moywz-jqblc-nvsif-dayv3-4qe")' --no-wallet --mode=reinstall $mode
fi


# first create the asset calling addAsset
echo "creating asset..."
asset_id=$(dfx canister --network $network call $mode addAsset "(record { \
    name = \"$filename\"; \
    payload = record {
        ctype = \"$mime\"; \
        data = vec {\
}}})")

asset_id=$(echo $asset_id | tr -d -c 0-9)
echo $asset_id

# then chunk the file and upload it to the asset
# id using streamAsset
i=0
byteSize=${#$(od -An -v -tuC $file)[@]}
echo "$network Uploading asset \"$filename\", size: $byteSize"
while [ $i -le $byteSize ]; do
    echo "chunk #$(($i/$threshold+1))..."
    dfx canister --network $network call $mode streamAsset "($asset_id, \
        false, \
        vec { $(for byte in ${(j:;:)$(od -An -v -tuC $file)[@]:$i:$threshold}; echo "$byte;") }\
    )"
    # dfx canister call staging addAsset "( vec {\
    #     vec { $(for byte in ${(j:;:)$(od -An -v -tuC $file)[@]:$i:$threshold}; echo "$byte;") };\
    # })"
    i=$(($i+$threshold))
done

open "https://$(dfx canister --network $network id $mode).raw.ic0.app/?asset=0"

# add the other 
for i in {1..$number_of_assets}
do
    echo "uploading asset $i"
    j=$i-1;
	dfx canister --network $network call $mode addAsset '(record {
        name = "'$i'";
        payload = record {
            ctype = "image/svg+xml"; 
            data = vec {blob "
                <svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"
                xmlns:ev=\"http://www.w3.org/2001/xml-events\">
                    <script>
                        fetch(\"https://n6au6-3aaaa-aaaae-qaaxq-cai.raw.ic0.app/'$i'.svg\")
                        .then(response =&gt; response.text())
                        .then(text =&gt; {
                            let parser = new DOMParser();
                            let doc = parser.parseFromString( text, \"image/svg+xml\" );
                            document.getElementsByTagName(\"svg\")[0].appendChild( doc.getElementsByTagName(\"svg\")[0] );
                            return fetch(\"https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&amp;vs_currencies=usd&amp;include_24hr_change=true\");
                        })
                        .then(response =&gt; response.json())
                        .then( priceChange =&gt; {
                            let usd24HourChange = priceChange.bitcoin.usd_24h_change.toFixed(2);
                            updateAnimationDuration(usd24HourChange);
                        })
                        .catch(err =&gt; console.log(err))

                        function calculateAnimationDuration(usd24HourChange, initialValue) {
                            let g = initialValue*2;
                            let k = 1/(50*initialValue);
                            let newValue = (g * (1 / (1 + Math.exp(k * g * usd24HourChange) * (g / initialValue - 1)))).toFixed(3);
                            return newValue === 0 ? 0.001 : newValue;
                        }

                        function updateAnimationDuration (usd24HourChange) {
                            let styleSheets = document.styleSheets;
                            for (let i = 2; i &lt; 22; i++){
                                let animationDuration = parseFloat(styleSheets[0].rules[i].style.animationDuration);
                                let updatedDuration = calculateAnimationDuration(usd24HourChange, animationDuration);
                                let updatedDurationString = updatedDuration.toString()+\"s\";
                                styleSheets[0].rules[i].style.animationDuration = updatedDurationString;
                            }
                        }
                    </script>
                </svg>"
            };
        };
        highres = opt record {
            ctype = "image/svg+xml"; 
            data = vec {blob "
                <svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"
                xmlns:ev=\"http://www.w3.org/2001/xml-events\">
                    <script>
                        fetch(\"https://n6au6-3aaaa-aaaae-qaaxq-cai.raw.ic0.app/'$i'_high.svg\")
                        .then(response =&gt; response.text())
                        .then(text =&gt; {
                            let parser = new DOMParser();
                            let doc = parser.parseFromString( text, \"image/svg+xml\" );
                            document.getElementsByTagName(\"svg\")[0].appendChild( doc.getElementsByTagName(\"svg\")[0] );
                            return fetch(\"https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&amp;vs_currencies=usd&amp;include_24hr_change=true\");
                        })
                        .then(response =&gt; response.json())
                        .then( priceChange =&gt; {
                            let usd24HourChange = priceChange.bitcoin.usd_24h_change.toFixed(2);
                            updateAnimationDuration(usd24HourChange);
                        })
                        .catch(err =&gt; console.log(err))

                        function calculateAnimationDuration(usd24HourChange, initialValue) {
                            let g = initialValue*2;
                            let k = 1/(50*initialValue);
                            let newValue = (g * (1 / (1 + Math.exp(k * g * usd24HourChange) * (g / initialValue - 1)))).toFixed(3);
                            return newValue === 0 ? 0.001 : newValue;
                        }

                        function updateAnimationDuration (usd24HourChange) {
                            let styleSheets = document.styleSheets;
                            for (let i = 2; i &lt; 22; i++){
                                let animationDuration = parseFloat(styleSheets[0].rules[i].style.animationDuration);
                                let updatedDuration = calculateAnimationDuration(usd24HourChange, animationDuration);
                                let updatedDurationString = updatedDuration.toString()+\"s\";
                                styleSheets[0].rules[i].style.animationDuration = updatedDurationString;
                            }
                        }
                    </script>
                </svg>"
            };
        };
        thumbnail = opt record {
            ctype = "image/svg+xml"; 
            data = vec {blob "
                <svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"
                xmlns:ev=\"http://www.w3.org/2001/xml-events\">
                    <script>
                        fetch(\"https://n6au6-3aaaa-aaaae-qaaxq-cai.raw.ic0.app/'$i'_low.svg\")
                        .then(response =&gt; response.text())
                        .then(text =&gt; {
                            let parser = new DOMParser();
                            let doc = parser.parseFromString( text, \"image/svg+xml\" );
                            document.getElementsByTagName(\"svg\")[0].appendChild( doc.getElementsByTagName(\"svg\")[0] );
                            return fetch(\"https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&amp;vs_currencies=usd&amp;include_24hr_change=true\");
                        })
                        .then(response =&gt; response.json())
                        .then( priceChange =&gt; {
                            let usd24HourChange = priceChange.bitcoin.usd_24h_change.toFixed(2);
                            updateAnimationDuration(usd24HourChange);
                        })
                        .catch(err =&gt; console.log(err))

                        function calculateAnimationDuration(usd24HourChange, initialValue) {
                            let g = initialValue*2;
                            let k = 1/(50*initialValue);
                            let newValue = (g * (1 / (1 + Math.exp(k * g * usd24HourChange) * (g / initialValue - 1)))).toFixed(3);
                            return newValue === 0 ? 0.001 : newValue;
                        }

                        function updateAnimationDuration (usd24HourChange) {
                            let styleSheets = document.styleSheets;
                            for (let i = 2; i &lt; 22; i++){
                                let animationDuration = parseFloat(styleSheets[0].rules[i].style.animationDuration);
                                let updatedDuration = calculateAnimationDuration(usd24HourChange, animationDuration);
                                let updatedDurationString = updatedDuration.toString()+\"s\";
                                styleSheets[0].rules[i].style.animationDuration = updatedDurationString;
                            }
                        }
                    </script>
                </svg>"
            };
        };
        metadata = opt record {
            ctype = "application/json"; 
            data = vec {blob "'"$(cat assets/btcflower_no_mint_number.json | jq ".[$j]" | sed 's/"/\\"/g')"'"
            };
        };
    })'
done

# init mint
echo "initiating mint ..."
dfx canister --network $network call $mode initMint

# init cap
# first create the asset calling addAsset
echo "initiating cap ..."
dfx canister --network $network call $mode initCap

# check the asset that are linked to the tokens
for i in {0..9}
do
        tokenid=$(ext tokenid $(dfx canister --network $network id $mode) $i | sed -n  2p)
		tokenid=$(echo $tokenid | tr -dc '[:alnum:]-')
		tokenid="${tokenid:3:-2}"
		echo "https://$(dfx canister --network $network id $mode).raw.ic0.app/?tokenid=$tokenid"
done

# now shuffle the assets using the random beacon
# echo "shuffling assets"
# dfx canister --network $network call staging shuffleAssets

# check the assets again to see if we now indeed
# see the correct assets
# for i in {0..9}
# do
#         tokenid=$(ext tokenid $(dfx canister --network ic id staging) $i | sed -n  2p)
# 		tokenid=$(echo $tokenid | tr -dc '[:alnum:]-')
# 		tokenid="${tokenid:3:-2}"
# 		curl "$(dfx canister --network ic id staging).raw.ic0.app/?tokenid=$tokenid"
# 		echo "\n"
# done
