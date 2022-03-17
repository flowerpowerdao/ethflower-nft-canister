#!/bin/zsh

for i in {0..2008}
do
        tokenid=$(ext tokenid pk6rk-6aaaa-aaaae-qaazq-cai $i | sed -n  2p)
		tokenid=$(echo $tokenid | tr -dc '[:alnum:]-')
		tokenid="${tokenid:3:-2}"
        curl "https://pk6rk-6aaaa-aaaae-qaazq-cai.raw.ic0.app/?tokenid=$tokenid&type=metadata" >> output.json
done

jq -s '.' < metadata_raw.json > metadata.json