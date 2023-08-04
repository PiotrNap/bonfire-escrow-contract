import { Context, beforeEach, describe, expect, it } from "vitest"
import fs from "fs/promises"
import * as L from "lucid-cardano"
import * as fc from "fast-check"

/**
 * Create multiple test scenarios
 *
 *
 * A' (attendee) O' (organizer) T' (treasury)
 *
 *
 * 1. Successful transaction
 *      - A' locks funds into contract
 *      - O' unlocks the funds after release window
 *      - T' receives fees for successful deal
 *
 * 2. Successful transaction (with a BetaTester NFT)
 *      - A' locks funds into contract
 *      - O' tries to unlock the funds after a release window with BetaTester NFT
 *      - T' does not receives any fee
 *
 * 3. On-time cancellation
 *      - A' locks funds into contract
 *      - A' unlocks the funds before cancellation deadline
 *
 * 4. Failed cancellation after deadline
 *      - A' locks funds into contract
 *      - A' tries to cancel (return) funds after deadline and fails
 *
 * 5. Failed withdrawal before release window
 *      - A' locks funds into contract
 *      - O' fails to withdraw funds before release window
 *
 * 6. Failed attempt to collect funds by any outside party
 *      - A' locks funds into contract
 *      - a malicious actor (not A' or O') fails to collect funds:
 *          a) before cancellation deadline
 *          b) after release window
 *
 * 7. Successful recycle of UTxOs older than 1 year
 *      - A' locks funds into contract
 *      - T' withdraws UTxO after 1 year
 *
 * 8. Failed recycle of UTxOs younger than 1 year
 *      - A' locks funds into contract
 *      - T' fails to withdraw UTxO before 1 year
 */

const BETA_TESTER_NFT_POLICYID =
  "283eb7c920155ba340c772eacdf466ccb45382202540ad734ded2fb5"
const BETA_TESTER_NFT_NAME = "BETA#1"

type CustomContext = Context & {
  // organizer: WalletEmulator
  // attendee: WalletEmulator
  // network: NetworkEmulator
  user1: L.PrivateKey
  user2: L.PrivateKey
  address1: string
  address2: string
  lucid: L.Lucid
  datum: any
}

describe("Escrow contract with 3 actions: Cancel, Complete, Recycle", async () => {
  const escrowValidator = await fs.readFile(
    "./output/plutus-scripts/escrow.plutus",
    { encoding: "utf8" }
  )
  const networkParamsFile = await fs.readFile("./protocol.json", "utf8")
  const validatorAddr: L.Address = (
    await L.Lucid.new(undefined, "Custom")
  ).utils.validatorToAddress(JSON.parse(escrowValidator))

// the function that given the context of lucid, the wallet,the datum and sends 10 ada to the script address.
async function sendToScript(
  lucid: L.Lucid,
  userPrivKey: L.PrivateKey,
  dtm: EscrowDatum
): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey)
  const tx = await lucid
    .newTx()
    .payToContract(
      validatorAddr,
      { inline: L.Data.to<EscrowDatum>(dtm) },
      { lovelace: 10000000n }
    )
    .complete()
  const signedTx = await tx.sign().complete()
  const txHash = await signedTx.submit()
  return txHash
}


  beforeEach(async (context: CustomContext) => {
  const user1: L.PrivateKey = L.generatePrivateKey()
  const address1: string = await (await L.Lucid.new(undefined, "Custom"))
    .selectWalletFromPrivateKey(user1)
    .wallet.address()

  const user2: L.PrivateKey = L.generatePrivateKey()
  const address2: string = await (await L.Lucid.new(undefined, "Custom"))
    .selectWalletFromPrivateKey(user2)
    .wallet.address()

  // Setup the emulator and give our testing wallet 10000 ada. These funds get added to the genesis block.
  const emulator = new L.Emulator([
    { address: address1, assets: { lovelace: 10000000000n, PIGGY: 50000n } },
    { address: address2, assets: { lovelace: 10000000000n } },
  ])

  context.lucid = await L.Lucid.new(emulator)

  })

  /** Test #1 **/
  it("Successful transaction", async ({}) => {
  // gets added to the first block in the emulator
  await sendToScript(lucid, user1, dtm)
  })

  /** Test #2 **/
  it("Successful transaction (with a BetaTester NFT)", async ({}) => {})

})

//////////////

// the typed datum and redeemer that we are going to use.
const EscrowDatum = L.Data.Object({
  deadline: L.Data.Integer(),
})
type EscrowDatum = L.Data.Static<typeof EscrowDatum>

const NegativeRTimedRedeemer = L.Data.Integer()
type NegativeRTimedRedeemer = L.Data.Static<typeof NegativeRTimedRedeemer>

// the function that given the context of lucid and a negative redeemer, grabs funds from the script address.
async function grabFunds(
  lucid: L.Lucid,
  emulator: L.Emulator,
  userPrivKey: L.PrivateKey,
  dtm: EscrowDatum,
  r: NegativeRTimedRedeemer
): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey)
  const rdm: L.Redeemer = L.Data.to<NegativeRTimedRedeemer>(
    r,
    NegativeRTimedRedeemer
  )
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(negativeRTimedAddr)
  const ourUTxO: L.UTxO[] = utxoAtScript.filter(
    (utxo) =>
      utxo.datum == L.Data.to<EscrowDatum>(dtm, EscrowDatum)
  )

  if (ourUTxO && ourUTxO.length > 0) {
    const tx = await lucid
      .newTx()
      .collectFrom(ourUTxO, rdm)
      .attachSpendingValidator(negativeRTimedValidator)
      .validFrom(emulator.now())
      .complete()

    const signedTx = await tx.sign().complete()
    const txHash = await signedTx.submit()
    return txHash
  } else throw new Error("UTxO's Expected!")
}

async function runTest(
  dtm: EscrowDatum,
  r: NegativeRTimedRedeemer,
  n: number
) {
  // setup a new privateKey that we can use for testing.
  const user1: L.PrivateKey = L.generatePrivateKey()
  const address1: string = await (await L.Lucid.new(undefined, "Custom"))
    .selectWalletFromPrivateKey(user1)
    .wallet.address()

  const user2: L.PrivateKey = L.generatePrivateKey()
  const address2: string = await (await L.Lucid.new(undefined, "Custom"))
    .selectWalletFromPrivateKey(user2)
    .wallet.address()

  // Setup the emulator and give our testing wallet 10000 ada. These funds get added to the genesis block.
  const emulator = new L.Emulator([
    { address: address1, assets: { lovelace: 10000000000n } },
    { address: address2, assets: { lovelace: 10000000000n } },
  ])
  const lucid = await L.Lucid.new(emulator)

  // gets added to the first block in the emulator
  await sendToScript(lucid, user1, dtm)

  // wait n slots
  emulator.awaitSlot(n)

  // gets added to the (n+1)th block
  await grabFunds(lucid, emulator, user2, dtm, r)

  emulator.awaitBlock(10)

  //console.log(await emulator.getUtxos(address2));
}
//await runTest({deadline:BigInt(Date.now()+20000*5+1000)},-42n,5*20+1);

// UNIT tests

function testSucceed(
  str: string, // the string to display of the test
  r: bigint, // the redeemer number
  d: bigint, // the deadline in seconds from now
  n: number // the number of slots user 2 waits
) {
  Deno.test(str, async () => {
    await runTest({ deadline: BigInt(Date.now()) + d }, r, n)
  })
}

async function testFails(
  str: string, // the string to display of the test
  r: bigint, // the redeemer number
  d: bigint, // the deadline in seconds from now
  n: number // the number of slots user 2 waits
) {
  Deno.test(str, async () => {
    let errorThrown = false
    try {
      await runTest({ deadline: BigInt(Date.now()) + d }, r, n)
    } catch (error) {
      errorThrown = true
    }
    assert(
      errorThrown,
      "Expected to throw an error, but it completed successfully"
    )
  })
}

// deadline is slot 100 and user 2 claims at slot 120
testSucceed(
  "UT: User 1 locks and user 2 takes with R = -42 after dealine; succeeds",
  -42n,
  BigInt(1000 * 100),
  120
)
// deadline is slot 100 and user 2 claims at slot 120
testSucceed(
  "UT: User 1 locks and user 2 takes with R = 0 after dealine; succeeds",
  0n,
  BigInt(1000 * 100),
  120
)
// deadline is slot 100 and user 2 claims at slot 120
testFails(
  "UT: User 1 locks and user 2 takes with R = 42 after dealine; fails",
  42n,
  BigInt(1000 * 100),
  120
)
// deadline is slot 100 and user 2 claims at slot 80
testFails(
  "UT: User 1 locks and user 2 takes with R = -42 before dealine; fails",
  -42n,
  BigInt(1000 * 100),
  80
)
// deadline is slot 100 and user 2 claims at slot 80
testFails(
  "UT: User 1 locks and user 2 takes with R = 0 before dealine; fails",
  -0n,
  BigInt(1000 * 100),
  80
)
// deadline is slot 100 and user 2 claims at slot 80
testFails(
  "UT: User 1 locks and user 2 takes with R = 42 before dealine; fails",
  42n,
  BigInt(1000 * 100),
  80
)

// Property test
// set up a fixed deadline at slot 100
const dl: number = 100 * 1000
// create only random 256 bit negative big integers for r.
const negativeBigIntArbitrary = fc.bigIntN(256).filter((n: bigint) => n <= 0n)
// create only random 256 bit positive big integers for r.
const positiveBigIntArbitrary = fc.bigIntN(256).filter((n: bigint) => n > 0n)
// create only random integers that represent claiming after the deadline
const afterDeadlineWaits = fc.integer().filter((n: number) => n >= dl)
// create only random integers that represent claiming before the deadline
const beforeDeadlineWaits = fc.integer().filter((n: number) => n < dl)

Deno.test("PT: Negative redeemer after deadline always succeeds", () => {
  fc.assert(
    fc.asyncProperty(
      negativeBigIntArbitrary,
      afterDeadlineWaits,
      async (r: bigint, n: number) => {
        try {
          await runTest({ deadline: BigInt(Date.now() + dl) }, r, n)
        } catch (error) {
          console.error(
            "Test failed for r= " + r + " with error: " + error.message
          )
          throw error
        }
      }
    ),
    { numRuns: 100 }
  )
})

Deno.test("PT: Positive redeemer after deadline always fails", () => {
  fc.assert(
    fc.asyncProperty(
      positiveBigIntArbitrary,
      afterDeadlineWaits,
      async (r: bigint, n: number) => {
        let errorThrown = false
        try {
          await runTest({ deadline: BigInt(Date.now() + dl) }, r, n)
        } catch (error) {
          errorThrown = true
        }
        assert(errorThrown, "Test failed for r= " + r + " and n= " + n)
      }
    ),
    { numRuns: 100 }
  )
})

Deno.test("PT: Anything before the deadline always fails", () => {
  fc.assert(
    fc.asyncProperty(
      fc.bigIntN(256),
      beforeDeadlineWaits,
      async (r: bigint, n: number) => {
        let errorThrown = false
        try {
          await runTest({ deadline: BigInt(Date.now() + dl) }, r, n)
        } catch (error) {
          errorThrown = true
        }
        assert(errorThrown, "Test failed for r= " + r + " and n= " + n)
      }
    ),
    { numRuns: 100 }
  )
})
