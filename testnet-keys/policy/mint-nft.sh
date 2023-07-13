#!bin/sh

# $1 txhash $2 txix $3 address $4 output $5 tokenname $6 policyid $7 script $8 slotnumber 


cardano-cli transaction build \
--testnet-magic 1 \
--alonzo-era \
--tx-in $1#$2 \
--tx-out $3+$4+"$5 $6.$5" \
--change-address $3 \
--mint="$5 $6.$6" \
--minting-script-file $7 \
--metadata-json-file metadata.json  \
--invalid-hereafter $8 \
--witness-override 2 \
--out-file matx.raw

cardano-cli transaction sign  \
--signing-key-file policy/payment.skey  \
--signing-key-file policy/policy.skey  \
--mainnet --tx-body-file matx.raw  \
--out-file matx.signed

cardano-cli transaction submit --tx-file matx.signed --testnet-magic 1