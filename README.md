# Escrow Smart Contract v1.0

## Plutus Documentation

### July 2023

#### Escrow Contract Parameters

1. 'treasuryPkh' public key hash of the Bonfire Treasury (can be another smart contract or
   just simply a wallet address)
2. 'betaTesterToken' PolicyID of an NFT minted for first users of Bonfire dApp.
   Allows for free usage of the dApp.

`EscrowParam`

```
treasuryPkh = "3475d4d36f40048a934ddc211dd84fa7c00a910421b566fc7e886080"
betaTesterToken = "481146d15d0c9bacc880254f88f944f6a88dba2e917d35fcbf92aa24"
```

### For Testing on `cardano-cli`:

- See `/testnet-keys/**` to be used as followed:
  - 'benefactor' - the person who locks funds at Escrow address for an exchange
    of 'beneficiary's goods/time
  - 'beneficiary' - person eligible to withdraw the funds
  - 'treasury' - party eligible for withdrawing UTxOs older than 1 year to be re-used

## We need to test the following scenarios:

### BR' (benefactor) BY' (beneficiary) T' (treasury)

1. Successful transaction

   - BR' locks funds into contract
   - BY' unlocks the funds after release window
   - T' receives fees for a successful deal

2. Successful transaction (with a BetaTester NFT)

   - BR' locks funds into contract
   - BY' tries to unlock the funds after a release window with BetaTester NFT
   - T' does not receive any fee

3. On-time cancellation

   - BR' locks funds into contract
   - BR' unlocks the funds before cancellation deadline

4. Failed cancellation after deadline

   - BR' locks funds into contract
   - BR' tries to cancel (return) funds after deadline and fails

5. Failed withdrawal before release window

   - BR' locks funds into contract
   - BY' fails to withdraw funds before release window

6. Failed attempt to collect funds by any outside party

   - BR' locks funds into contract
   - a malicious actor (not BR' or BY') fails to collect funds:
     a) before cancellation deadline
     b) after release window

7. Successful recycle of UTxOs older than 1 year

   - BR' locks funds into contract
   - T' withdraws UTxO after 1 year

8. Failed recycle of UTxOs younger than 1 year

   - BR' locks funds into contract
   - T' fails to withdraw UTxO before 1 year

### Things to Keep In Mind:

1. Datum is created by either a dApp using this contract, or a person crafting a transaction.
   `releaseDate`, `cancelDeadline` & `createdAt` should be adjusted accordingly.
2.

### Global Variables

Set those variables before going through any test scenario.

```
BENEFACTOR=addr_test1qptjnhrdj59a6l8kpfwyz6g3nle2wre63attjwc9dfl2htaez6y4l8t8qfp0mmwmtgz86tjrfdmj6h95qedegpdtap8saejk7e \
BENEFACTOR_KEY=./testnet-keys/benefactor/addr.skey \
BENEFACTOR_PKH=5729dc6d950bdd7cf60a5c4169119ff2a70f3a8f56b93b056a7eabaf \
BENEFICIARY=addr_test1qznk843nwlpygv0wk92ykjnfx0u7syagzg44u9deq3sg26w3hvu80mxsh6rmrfg09x6wxkztrn428swfnjmnsvk4mpcqajg9zd \
BENEFICIARY_KEY=./testnet-keys/beneficiary/addr.skey \
BENEFICIARY_PKH=a763d63377c24431eeb1544b4a6933f9e813a8122b5e15b904608569 \
TREASURY=addr_test1qq68t4xndaqqfz5nfhwzz8wcf7nuqz53qssm2ehu06yxpqxt6cxzpd46l9arna6d75yjnfkkkp62066s3wfdrxetds8qclfqcv \
TREASURY_KEY=./testnet-keys/treasury/addr.skey \
CONTRACT=$(cat ./output/plutus-scripts/escrow.addr)  \
SCRIPT_PATH=./output/plutus-scripts/escrow.plutus
CONTRACT_REFERENCE_TX= \
DATUM_PATH=./output/datums/example-datum.json \
CANCEL_REDEEMER_PATH=./output/redeemers/Cancel.json \
COMPLETE_REDEEMER_PATH=./output/redeemers/Complete.json \
RECYCLE_REDEEMER_PATH=./output/redeemers/Recycle.json \
PAYMENT_TOKEN=3e544e20875172fe302df3afdcdaefeba828299e0f89562449845a4f.5049474759 \
PARAM_PATH=protocol.json \
NETWORK='--testnet-magic 1' 
```

You can also set those aliases to shorten the time needed for entering each command:
```
alias submit="cardano-cli transaction submit --tx-file tx.signed $NETWORK"
alias signBR="cardano-cli transaction sign --signing-key-file $BENEFACTOR_KEY $NETWORK --tx-body-file tx.raw --out-file tx.signed"
alias signBY="cardano-cli transaction sign --signing-key-file $BENEFICIARY_KEY $NETWORK --tx-body-file tx.raw --out-file tx.signed"
alias signTR="cardano-cli transaction sign --signing-key-file $TREASURY_KEY $NETWORK --tx-body-file tx.raw --out-file tx.signed"
alias queryBR="cardano-cli query utxo --address $BENEFACTOR $NETWORK"
alias queryBY="cardano-cli query utxo --address $BENEFICIARY $NETWORK"
alias queryCTR="cardano-cli query utxo --address $CONTRACT $NETWORK"
alias queryTR="cardano-cli query utxo --address $TREASURY $NETWORK"
```


### Test: #0 Preliminary Step

In order for this contract to execute correctly you need to adjust a few inputs inside Datum,
which looks like this:

```
data EscrowDatum = EscrowDatum
  {
    beneficiaryPkh :: PubKeyHash,
    benefactorPkh :: PubKeyHash,
    releaseDate :: POSIXTime,
    cancelDeadline :: POSIXTime,
    createdAt :: POSIXTime
  }
```

Change 'createdAt', 'cancelDeadline' & 'releaseDate' inside ./output/datums/example-datum.json
(json file path: $DATUM_PATH)

Same goes for the redeemer. There are 3 actions present: Cancel, Complete & Recycle.
(json file path: $REDEEMER_PATH)

To check the Haskell implementation, check out this file ./src/Escrow/Types.hs

#### Each test scenario starts with benefactor locking funds at Escrow Contract, so we run:

- Step 1 - Set Variables:

We will use PIGGY tokens as our additional payment asset.
Query beneficiary address by running: 
`cardano-cli query utxo --address $BENEFACTOR $NETWORK`
and copy the TXIN's together with the Idx number.

```
TXIN1=2ca489f3820c4f0689f60ff8d1541bca6e4161de0008768d7b44ba18449c5d00#1
TXIN2=9e6447dcd4609a75c6e206a4c1336772a312d9689f0be767ae68800e95dece35#0 
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=5000
```

- Step 2 - Build + Submit Tx:

````
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACT+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--tx-out-inline-datum-file $DATUM_PATH \
--tx-out $BENEFACTOR+"226779677 + 85000 $PAYMENT_TOKEN" \
--change-address $BENEFACTOR \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

signBR && submit
```

### Test #1: Successful Transaction (without a BetaTester NFT) ❓

Query contract and beneficiary addresses to find out the hashes of UTxOs.

Fee is set here to 1.5 ADA because 1% of the total Lovelace amount is less than
minimum fee amount (1.5 ADA).
```
TXIN1=6a117c86c85b87fcc7c742c5dd3773df70ce052362df0d5de4af50f5c705bef8#0
TXIN2=6a117c86c85b87fcc7c742c5dd3773df70ce052362df0d5de4af50f5c705bef8#1
TXIN3=2ca489f3820c4f0689f60ff8d1541bca6e4161de0008768d7b44ba18449c5d00#0
AMOUNT_FEE=1500000
AMOUNT_LOVELACE=$(expr 25000000 - $AMOUNT_FEE)
AMOUNT_PAYMENT_TOKEN=5000
VALID_FROM=$(cardano-cli query tip $NETWORK | jq .slot)
VALID_TO=$(expr $VALID_FROM + 300)
```
---  !!! should attach plutus script

After running step #0. Run this terminal command to unlock the funds as a beneficiary.
We need to provide TX-valid-range that's after the 'releaseDate' specified in Datum.

cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN3 \
--tx-in-script-file $SCRIPT_PATH \
--tx-in-inline-datum-present \
--tx-in-redeemer-file $COMPLETE_REDEEMER_PATH \
--tx-in-collateral $TXIN2 \
--tx-in $TXIN1 \
--tx-out $BENEFICIARY+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--tx-out $BENEFICIARY+200000000 \
--tx-out $TREASURY+$AMOUNT_FEE \
--invalid-before $VALID_FROM \
--invalid-hereafter $VALID_TO \
--required-signer-hash $BENEFICIARY_PKH \
--change-address $BENEFICIARY \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

signBY && submit
```

### Test #2: Successful transaction (with a BetaTester NFT)

Query contract and beneficiary address to find out the hashes of UTxOs
```
TXIN1=<beneficiary UTxO>
TXIN2=<beneficiary collateral UTxO>
TXIN3=<beneficiary UTxO with betaTesterToken>
TXIN4=<contract UTxO>
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=5000
VALID_FROM=$(cardano-cli query slot-number $NETWORK)
VALID_TO=$(expr $VALID_FROM + 300)
```

After running step #0. Run this terminal command to unlock the funds as a beneficiary.

--- !!! no need to use -tx-out ??

cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN3 \
--tx-in $TXIN4 \
--payment-script-file $CONTRACT \
--tx-in-inline-datum-present \
--tx-in-redeemer-file $REDEEMER_PATH \
--tx-in-collateral $TXIN2 \
--invalid-before $VALID_FROM \
--invalid-hereafter $VALID_TO \
--change-address $BENEFICIARY \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

cardano-cli transaction sign \
--signing-key-file $BENEFICIARY_KEY \
--tx-body-file tx.raw \
--out-file tx.signed \
$NETWORK

cardano-cli transaction submit \
--tx-file tx.signed \
$NETWORK

### Test #3: On-time cancellation


Query contract and benefactor address to find out the hashes of UTxOs
```
TXIN1=<benefactor UTxO>
TXIN2=<benefactor collateral UTxO>
TXIN3=<contract UTxO>
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=5000
```

After running step #0. Run this terminal command to unlock the funds as a beneficiary.

SLOT=61030331
Note: we can query tip for current slot and use that slot.
This is like saying "this tx is invalid before now"

cardano-cli transaction build \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUM_PATH \
--tx-in-redeemer-file $REDEEMER_PATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ORGANIZER+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--tx-out $ORGANIZER+"2000000 + 1 $ORGANIZERTOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENT_TOKEN" \
--change-address $ORGANIZER \
--invalid-before $SLOT \
--required-signer-hash $ORGANIZERPKH \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1

cardano-cli transaction build \
--tx-in $TXIN1 \
--tx-in $TXIN3 \
--tx-in $TXIN4 \
--payment-script-file $CONTRACT \
--tx-in-inline-datum-present \
--tx-in-redeemer-file $REDEEMER_PATH \
--tx-in-collateral $TXIN2 \
--change-address $BENEFACTOR \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

cardano-cli transaction sign \
--signing-key-file $BENEFACTOR_KEY \
--tx-body-file tx.raw \
--out-file tx.signed \
$NETWORK

cardano-cli transaction submit \
--tx-file tx.signed \
$NETWORK

### Test #4: Failed cancellation after deadline ✅

```
TXIN1=2ca489f3820c4f0689f60ff8d1541bca6e4161de0008768d7b44ba18449c5d00#0
TXIN3=e2c706c6ede119256e380aadae08282e3a2bebcfae4b1e7f367ff958bd23a244#0
TXIN2=e2c706c6ede119256e380aadae08282e3a2bebcfae4b1e7f367ff958bd23a244#1
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=5000
VALID_FROM=$(cardano-cli query tip $NETWORK | jq .slot)
VALID_TO=$(expr $VALID_FROM + 300)
```

cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN2 \
--tx-in $TXIN1 \
--tx-in-script-file $SCRIPT_PATH \
--tx-in-inline-datum-present \
--tx-in-redeemer-file $CANCEL_REDEEMER_PATH \
--tx-in-collateral $TXIN3 \
--tx-out $BENEFACTOR+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--invalid-before $VALID_FROM \
--invalid-hereafter $VALID_TO \
--required-signer-hash $BENEFACTOR_PKH \
--change-address $BENEFACTOR \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

signBR && submit
```

### Test: Resolving Disputes --> Pay to Attendee


Note: in this test case, `ADMIN` and `TREASURY` are same addr, but they will not be in Dapp.

cardano-cli transaction build \
--tx-in $TXINADMIN \
--tx-in $TXINFEE \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUM_PATH \
--tx-in-redeemer-file $REDEEMER_PATH \
--tx-in-collateral $COLLATERAL \
--tx-out $BENEFACTOR+"$LOVELACETOATT + $GIMBALSTOATT $PAYMENT_TOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENT_TOKEN" \
--tx-out $ADMIN+"2000000 + 1 $ADMINTOKEN" \
--change-address $ADMIN \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $ADMINKEY \
--testnet-magic 1 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1

### Test: Resolving Disputes --> Pay to Organizer



Note: in this test case, `ADMIN` and `TREASURY` are same addr, but they will not be in Dapp.

cardano-cli transaction build \
--tx-in $TXINADMIN \
--tx-in $TXINFEE \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUM_PATH \
--tx-in-redeemer-file $REDEEMER_PATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ORGANIZER+"$LOVELACETOORG + $GIMBALSTOORG $PAYMENT_TOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENT_TOKEN" \
--tx-out $ADMIN+"2000000 + 1 $ADMINTOKEN" \
--change-address $ADMIN \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $ADMINKEY \
--testnet-magic 1 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1

### Test: Resolving Disputes --> Split

Note 16 June - Need to recompile contracts to fix 45/10 error.

````


Note: in this test case, `ADMIN` and `TREASURY` are same addr, but they will not be in Dapp.

cardano-cli transaction build \
--tx-in $TXINADMIN \
--tx-in $TXINFEE \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUM_PATH \
--tx-in-redeemer-file $REDEEMER_PATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ORGANIZER+"$LOVELACETOORG + $GIMBALSTOORG $PAYMENT_TOKEN" \
--tx-out $BENEFACTOR+"$LOVELACETOATT + $GIMBALSTOATT $PAYMENT_TOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENT_TOKEN" \
--tx-out $ADMIN+"2000000 + 1 $ADMINTOKEN" \
--change-address $ADMIN \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $ADMINKEY \
--testnet-magic 1 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1

```

## Unlocking Event UTXOs

```

CONTRACTADDR=""
BENEFACTOR=""
ORGANIZER=""
CONTRACTTXIN="#1"
ORGANIZERTXIN="#0"
COLLATERAL="#10"

```

```

cardano-cli transaction build \
--tx-in $CONTRACTTXIN \
--tx-in-script-file bonfire-escrow-000.plutus \
--tx-in-datum-file piotr-attendee.json \
--tx-in-redeemer-file OrgCancel.json \
--tx-in $ORGANIZERTXIN \
--tx-in-collateral $COLLATERAL \
--tx-out $BENEFACTOR+"4000000 + 20 cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059.706c6179" \
--required-signer-hash "22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3" \
--change-address $ORGANIZER \
--protocol-params-file protocol.json \
--testnet-magic 1 \
--out-file orgUnlock.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1 \
--tx-body-file orgUnlock.raw \
--out-file orgUnlock.signed

cardano-cli transaction submit \
--tx-file orgUnlock.signed \
--testnet-magic 1

```

### Organizer can Unlock any time

### Attendee can Unlock up to 24 hours before Event start time

## Some V1 Ideas:

- Implement multi-asset payment (V0 works with ADA and gimbals)
- Implement Organizer cancellation policy (V0, everyone works with same cancel policy)
- Implement Update as contract Action (V0 only has Cancel, Complete, Dispute)
- More robust Dispute handling! (Technically, V0 allows Attendee to Dispute too early)

## Things to think about:

- How does the Contract "reference" the records in the backend?
- How can users trust that information on backend is not changing?
- Do we need to change the `geq` in BonfireDispute?

---

## Quick Tx

```

cardano-cli transaction build \
--tx-in $TXIN \
--tx-out $ORGANIZER+20000000 \
--tx-out $ORGANIZER+20000000 \
--tx-out $ORGANIZER+20000000 \
--tx-out $ORGANIZER+20000000 \
--change-address $ORGANIZER \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $ORGANIZERKEY \
--testnet-magic 1 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1

```

## Quick Send bonGimbals TX

```

cardano-cli transaction build \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $BENEFACTOR+"100000000 + 5000000 982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c" \
--change-address $MONDAY \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $MONDAYKEY \
--testnet-magic 1 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1

```

Special thanks to Gimbalabs community for the initial help.
```
