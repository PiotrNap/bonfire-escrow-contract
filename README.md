# Smart Contract Documentation

## Introduction

This smart contract provides the ability to exchange crypto assets between two parties by introducing
the escrow capabilities. The details about the benefactor-beneficiary transaction are stored on the
blockchain in a transparent manner, by a use of inline Datum.

The contract allows benefactor to cancel his stake in a predefined time window, and prevents the
beneficiary from withdrawing any assets before a release date.

In case of a successful transaction between both parties, a service fee of 1% or 1.5 ADA is applied.
All UTxOs older than 1 year are eligible for withdraw by the service provider.

### Tech Stack

There are two contracts: one written in Plutus (not completed & not tested), and one written in Helios language
(completed & fully tested).

Tests were done by using Vite.js and Helios.js, and can be previed in file "escrow.test.ts".

### Parameters

1. 'treasuryPkh' public key hash of a wallet providing the service (can be another smart contract address)
2. 'betaTesterToken' PolicyID of tokens that give free use of this smart contract (can be removed)

### Data structures

1. Datum

   - benefactorPkh & beneficiaryPkh - public key hashes of both parties who are exchanging crypto assets
   - releaseDate - time after which beneficiary is eligible to collect the payment
   - cancelFee - lovelace fee applied during cancelation by benefactor within allowed time window (expressed in % of total Lovelace assets value)
   - cancelWindowStart & cancelWindowEnd - cancellation window in which benefactor is allowed to back out from agreement
   - createdAt - time at which this agreement was created
   - paymentTokens - Cardano assets which are supposed to be exchanged between both parties

2. Redeemer
   - Cancel - allows to cancel the agreement, should contain the UTxO identifier & UTxO index (so smart contract can read the Datum)
   - Complete - allows to withdraw UTxOs after the release date by beneficiary
   - Recycle - allows to recycle UTxOs older than 1 year by the 'treasuryPkh' owner

### Testing

The following scenarios were tested:

#### A' (benefactor) O' (beneficiary) T' (treasury)

1. Successful transaction

   - A' locks funds into contract
   - O' unlocks the funds after a release window
   - T' receives fees for successful deal

2. Successful transaction (with a BetaTester NFT)

   - A' locks funds into contract
   - O' tries to unlock the funds after a release window with BetaTester NFT
   - T' does not receive any fee

3. On-time cancellation

   - A' locks funds into contract
   - A' unlocks the funds before cancellation deadline

4. Failed cancellation after deadline

   - A' locks funds into contract
   - A' tries to cancel (return) funds after deadline and fails

5. Failed withdrawal before release window

   - A' locks funds into contract
   - O' fails to withdraw funds before release window

6. Failed attempt to collect funds by any outside party

   - A' locks funds into contract
   - a malicious actor (not A' or O') fails to collect funds:
     a) before cancellation deadline
     b) after release window

7. Successful recycle of UTxOs older than 1 year

   - A' locks funds into contract
   - T' withdraws UTxO after 1 year

8. Failed recycle of UTxOs younger than 1 year
   - A' locks funds into contract
   - T' fails to withdraw UTxO before 1 year

#### How to run it.

Whenever new changes were made to the escrow contract (helios) code, build command should be executed: `node ./utils/helios.js`.

After this you can run the `vitest` command which will automatically apply new contract and run all test.

---

#### Special thanks to Catalyst Fund 8 voters for supporting this project.
