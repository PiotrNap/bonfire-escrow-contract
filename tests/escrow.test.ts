import { Context, beforeEach, describe, expect, it } from "vitest"
import {
  Address,
  Assets,
  ByteArrayData,
  ConstrData,
  Datum,
  IntData,
  ListData,
  NetworkEmulator,
  NetworkParams,
  Tx,
  TxOutput,
  UplcProgram,
  Value,
  WalletEmulator,
  hexToBytes,
} from "@hyperionbt/helios"
const fs = require("fs/promises")

type CustomContext = Context & {
  organizer: WalletEmulator
  attendee: WalletEmulator
  network: NetworkEmulator
  datum: any
}

/**

   // if it is a minting policy
   const mph = uplcProgram.mintingPolicyHash;
   tx.mintTokens(mph, [[<tokenName>, <quantity>], ...], <redeemer>);
   tx.attachScript(uplcProgram);
   
   // if you want to send to a spending validator
   const vh = uplcProgram.validatorHash;
   const scriptAddress = Address.fromValidatorHash(vh);
   
   tx.addOutput(new TxOutput(
     scriptAddress,
     new Value(1000000)
   );
   
   // if you want to spend from a validator
   tx.addInput(await network.getUtxos(scriptAddress)[0], <redeemer>);
   tx.attachScript(uplcProgram);
**/

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

// const OPTIMIZE = false
const BETA_TESTER_NFT_POLICYID =
  "283eb7c920155ba340c772eacdf466ccb45382202540ad734ded2fb5"
const BETA_TESTER_NFT_NAME = "BETA#1"

describe("Escrow contract with 3 actions: Cancel, Complete, Recycle", async () => {
  const scriptData: string = await fs.readFile(
    "./output/plutus-scripts/escrow.plutus",
    { encoding: "utf8" }
  )
  const plutusCBORHex = JSON.parse(scriptData)["cborHex"]
  const uplcProgram = UplcProgram.fromCbor(plutusCBORHex)
  const vh = uplcProgram.validatorHash
  const scriptAddress = Address.fromValidatorHash(vh)

  const networkParamsFile = await fs.readFile("./protocol-params.json", "utf8")
  const networkParams = new NetworkParams(JSON.parse(networkParamsFile))
  const emulatorDate = Number(networkParams.slotToTime(BigInt(0)))

  beforeEach(async (context: CustomContext) => {
    const network = new NetworkEmulator()
    const attendee = network.createWallet(BigInt(0))
    const organizer = network.createWallet(BigInt(0))

    /** Locking Datum **/
    const oneHour = 3_600_000
    const oneMinute = 600
    const createdAt = new Date(emulatorDate + oneMinute)
    const cancelDeadline = new Date(emulatorDate + oneHour / 2)
    const releaseDate = new Date(emulatorDate + oneHour)
    const datum = new ConstrData(0, [
      new ByteArrayData(organizer.pubKeyHash.bytes),
      new ByteArrayData(attendee.pubKeyHash.bytes),
      new IntData(BigInt(Math.floor(releaseDate.getTime() / 1000))),
      new IntData(BigInt(Math.floor(cancelDeadline.getTime() / 1000))),
      new IntData(BigInt(Math.floor(createdAt.getTime() / 1000))),
    ])
    const inlineDatum = Datum.inline(datum)
    debugger
    console.log(organizer.pubKeyHash.hex)
    console.log(inlineDatum.toData())

    network.createUtxo(attendee, BigInt(100_000_000))
    network.createUtxo(organizer, BigInt(5_000_000))
    network.tick(BigInt(1))

    context.datum = inlineDatum
    context.network = network
    context.attendee = attendee
    context.organizer = organizer
  })

  /** Test #1 **/
  it("Successful transaction", async ({
    organizer,
    attendee,
    network,
    datum,
  }: CustomContext) => {
    let attendeeUtxos = await attendee.utxos
    let organizerUtxos = await organizer.utxos
    const redeemer = new ConstrData(1, [])

    // lock funds
    const lockingTx = new Tx()
      .addInputs(attendeeUtxos)
      .addOutput(
        new TxOutput(scriptAddress, new Value(BigInt(5_000_000)), datum)
      )
    await lockingTx.finalize(networkParams, attendee.address)
    await network.submitTx(lockingTx)
    network.tick(BigInt(100))

    // unlock funds
    let escrowContratUtxos = await network.getUtxos(scriptAddress)
    attendeeUtxos = await attendee.utxos
    organizerUtxos = await organizer.utxos
    let params = await network.getParameters()

    // what needs to be sent to unlock a UTxO
    const unlockingTx = new Tx()
      .attachScript(uplcProgram)
      .addInputs(organizerUtxos)
      .addInput(escrowContratUtxos[0], redeemer)
      .validFrom(params.liveSlot)
      .validTo(params.liveSlot + BigInt(10))
      .addSigner(organizer.pubKeyHash)
      .addOutput(
        new TxOutput(organizer.address, escrowContratUtxos[0].value, datum)
      )
    debugger
    await unlockingTx.finalize(networkParams, organizer.address)
    await network.submitTx(unlockingTx)
    network.tick(BigInt(1))

    escrowContratUtxos = await network.getUtxos(scriptAddress)
    attendeeUtxos = await attendee.utxos
    organizerUtxos = await organizer.utxos

    // expect:
    // - contract to have no UTxOs
    // - organizer have more than 5 ADA in a wallet
    // - treasury to have ... in a wallet
  })

  /** Test #2 **/
  it("Successful transaction (with a BetaTester NFT)", async ({
    attendee,
    network,
    organizer,
  }: CustomContext) => {
    const nftTokenName = ByteArrayData.fromString(BETA_TESTER_NFT_NAME).toHex()
    const tokens: [number[], bigint][] = [[hexToBytes(nftTokenName), BigInt(1)]]

    organizer = network.createWallet(
      BigInt(0),
      new Assets([[BETA_TESTER_NFT_POLICYID, tokens]])
    )
  })
})
