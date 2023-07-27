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
BENEFACTOR=$(cat ./testnet-keys/benefactor/addr) 
BENEFACTOR_KEY=./testnet-keys/benefactor/addr.skey 
BENEFACTOR_PKH=5729dc6d950bdd7cf60a5c4169119ff2a70f3a8f56b93b056a7eabaf 
BENEFICIARY=$(cat ./testnet-keys/beneficiary/addr)
BENEFICIARY_KEY=./testnet-keys/beneficiary/addr.skey 
BENEFICIARY_PKH=a763d63377c24431eeb1544b4a6933f9e813a8122b5e15b904608569 
TREASURY=$(cat ./testnet-keys/treasury/addr) 
TREASURY_KEY=./testnet-keys/treasury/addr.skey 
CONTRACT=$(cat ./output/plutus-scripts/escrow.addr)  
SCRIPT_PATH=./output/plutus-scripts/escrow.plutus 
CONTRACT_REFERENCE_TX=bbe4fc7fde3ff36730a7b83586df2d7c51284b30b42e09d1581ab455833ecf5b#0
DATUM_PATH=./output/datums/example-datum.json 
CANCEL_REDEEMER_PATH=./output/redeemers/Cancel.json 
COMPLETE_REDEEMER_PATH=./output/redeemers/Complete.json 
RECYCLE_REDEEMER_PATH=./output/redeemers/Recycle.json 
PAYMENT_TOKEN=3e544e20875172fe302df3afdcdaefeba828299e0f89562449845a4f.5049474759 
PARAM_PATH=protocol.json 
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
    createdAt :: POSIXTime,
    paymentTokens :: Value
  }
```

Change 'createdAt', 'cancelDeadline' & 'releaseDate' inside ./output/datums/example-datum.json
(json file path: $DATUM_PATH). Payment tokens are set to have 25 ADA + 5000 Piggy tokens. 
You can change them if you want to.

Same goes for the redeemer. There are 3 actions present: Cancel, Complete & Recycle.
(json file path: $REDEEMER_PATH)

To check the Haskell implementation, check out this file ./src/Escrow/Types.hs

#### Each test scenario starts with benefactor locking funds at Escrow Contract, so we run:

- Step 1 - Set Variables:

We will use PIGGY tokens as our additional payment asset.
Query benefactor address by running: 
`cardano-cli query utxo --address $BENEFACTOR $NETWORK` or `queryBR`
and copy the TXIN's together with the Idx number.

```
TXIN1=1bd7602ebf49bd133f31781133d9a7ffeff4dbec0157f9cd3c2babc3ee2bf3bd#1
TXIN2=1bd7602ebf49bd133f31781133d9a7ffeff4dbec0157f9cd3c2babc3ee2bf3bd#2
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=1000
```

- Step 2 - Build + Submit Tx:

````
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACT+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--tx-out-inline-datum-file $DATUM_PATH \
--tx-out $BENEFACTOR+"7000000000 + 44000 $PAYMENT_TOKEN" \
--change-address $BENEFACTOR \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

signBR && submit
```

### Test #1: Successful Transaction (without a BetaTester NFT) ❓❓

Query contract and beneficiary addresses to find out the hashes of UTxOs.

Fee is set here to 1.5 ADA because 1% of the total Lovelace amount is less than
minimum fee amount (1.5 ADA).
```
TXIN1=d17e8441f714be87f0800df99c85b769792641a879f74a236d4a03468da534e3#0
TXIN2=017fe886cea5486210071eb1ca9f90cf2e014caf7f77132778daf87391c77f60#0
TXIN3=c901631c583de4dc5d867889d4763337403c0492b76615ae07b58a3feaf38a9b#0
AMOUNT_FEE=1500000
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=1000
VALID_FROM=$(cardano-cli query tip $NETWORK | jq .slot)
VALID_TO=$(expr $VALID_FROM + 300)
```

After running step #0. Run this terminal command to unlock the funds as a beneficiary.
We need to provide TX-valid-range that's after the 'releaseDate' specified in Datum.

```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-in-script-file $SCRIPT_PATH \
--tx-in-inline-datum-present \
--tx-in-redeemer-file $COMPLETE_REDEEMER_PATH \
--tx-in-collateral $TXIN3 \
--tx-out $BENEFICIARY+"999999999999 + 9999999 $PAYMENT_TOKEN" \
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
VALID_FROM=$(cardano-cli query tip $NETWORK | jq .slot)
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

### Test #3: On-time cancellation ❓❓

Query contract and benefactor address to find out the hashes of UTxOs
```
TXIN1=c086ae13ff2152eb7177218679aa8e27abc2a3a9f31dfc6ef7dad6646b19e90a#0
TXIN2=d31c0669db58f2ebf4c39652f48c3d125c84d4526ca00db37d18eef8d4bdd8ec#0
TXIN3=d1f815c09577a942c2759a4a480725fe741cd6df818e16725652d427e20b3dbd#0
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=5000
VALID_FROM=$(cardano-cli query tip $NETWORK | jq .slot)
VALID_TO=$(expr $VALID_FROM + 300)
```

After running step #0. Run this terminal command to unlock the funds as a beneficiary.


```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--spending-tx-in-reference $CONTRACT_REFERENCE_TX \
--spending-plutus-script-v2 \
--spending-reference-tx-in-datum-file $DATUM_PATH \
--spending-reference-tx-in-redeemer-file $CANCEL_REDEEMER_PATH \
--tx-out $BENEFACTOR+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--tx-out $BENEFACTOR+5000000 \
--tx-in-collateral $TXIN3 \
--invalid-before $VALID_FROM \
--invalid-hereafter $VALID_TO \
--required-signer-hash $BENEFACTOR_PKH \
--change-address $BENEFACTOR \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

signBR && submit
```

### Test #4: Failed cancellation after deadline ✅

```
TXIN1=c086ae13ff2152eb7177218679aa8e27abc2a3a9f31dfc6ef7dad6646b19e90a#0
TXIN2=659efae7c08bc099a89e8632ed76428d16691a90180a39b604eb75251a31ce48#0
TXIN3=d1f815c09577a942c2759a4a480725fe741cd6df818e16725652d427e20b3dbd#0
AMOUNT_LOVELACE=25000000
AMOUNT_PAYMENT_TOKEN=5000
VALID_FROM=$(cardano-cli query tip $NETWORK | jq .slot)
VALID_TO=$(expr $VALID_FROM + 300)
```

```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--spending-tx-in-reference $CONTRACT_REFERENCE_TX \
--spending-plutus-script-v2 \
--spending-reference-tx-in-inline-datum-present \
--spending-reference-tx-in-redeemer-file $CANCEL_REDEEMER_PATH \
--tx-out $BENEFACTOR+"$AMOUNT_LOVELACE + $AMOUNT_PAYMENT_TOKEN $PAYMENT_TOKEN" \
--tx-in-collateral $TXIN3 \
--invalid-before $VALID_FROM \
--invalid-hereafter $VALID_TO \
--required-signer-hash $BENEFACTOR_PKH \
--change-address $BENEFACTOR \
--protocol-params-file protocol.json \
--out-file tx.raw \
$NETWORK

signBR && submit
```

### Test #5: Failed withdraw before release window

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

#### Special thanks to Gimbalabs community for the initial help.

