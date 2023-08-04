import { Context, beforeEach, describe, it } from "vitest"

import {
  Address,
  Assets,
  ByteArrayData,
  ConstrData,
  Datum,
  NetworkEmulator,
  NetworkParams,
  Program,
  PubKeyHash,
  RootPrivateKey,
  Tx,
  TxOutput,
  UplcProgram,
  Value,
  WalletEmulator,
  hexToBytes,
  textToBytes,
} from "@hyperionbt/helios"
import fs from "fs/promises"

type EscrowDatum = {
  beneficiaryPkh: string
  benefactorPkh: string
  releaseDate: number
  cancelDeadline: number
  createdAt: number
  paymentTokens: Value
}
type CustomContext = Context & {
  beneficiary: WalletEmulator
  benefactor: WalletEmulator
  // treasury: WalletEmulator
  treasury: any
  network: NetworkEmulator
  datum: any
  datum2: any
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
 * A' (benefactor) O' (beneficiary) T' (treasury)
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

const PAYMENT_TOKEN_POLICYID =
  "3e544e20875172fe302df3afdcdaefeba828299e0f89562449845a4f"
const BETA_TESTER_NFT_POLICYID =
  "283eb7c920155ba340c772eacdf466ccb45382202540ad734ded2fb5"
const BETA_TESTER_NFT_NAME = "BETA#1"
const TREASURY_TEST_SEED =
  "raise glove tail friend resource helmet spatial similar maid minimum shock talent cigar rice suspect"

/**
 * Helper functions
 */
function calculateServiceFee(
  totalValue: Value,
  withBetaTesterToken: boolean = false,
): Value {
  const lovelace = totalValue.lovelace

  if (withBetaTesterToken) {
    return new Value(0)
  } else if (lovelace / 100n > 1_500_00n) {
    return new Value(lovelace / 100n)
  } else return new Value(1_500_000n)
}

describe("Escrow contract with 3 actions: Cancel, Complete, Recycle", async () => {
  const scriptData: string = await fs.readFile(
    "./output/plutus-scripts/escrow.plutus",
    { encoding: "utf8" },
  )
  const plutusCBORHex = JSON.parse(scriptData)["cborHex"]
  const uplcProgram = UplcProgram.fromCbor(plutusCBORHex)
  const vh = uplcProgram.validatorHash
  const scriptAddress = Address.fromValidatorHash(vh)

  const escrowScript = await fs.readFile(
    "./src/Escrow/EscrowContract.hl",
    "utf8",
  )
  const escrowProgram = Program.new(escrowScript)
  const escrowProgramCompiled = escrowProgram.compile(false)

  const networkParamsFile = await fs.readFile("./protocol.json", "utf8")
  const networkParams = new NetworkParams(JSON.parse(networkParamsFile))

  beforeEach(async (context: CustomContext) => {
    const network = new NetworkEmulator()
    const treasuryRootKey = RootPrivateKey.fromPhrase(
      TREASURY_TEST_SEED.split(" "),
    )
    const treasuryPubKey = treasuryRootKey.derivePubKey()
    const benefactor = network.createWallet(BigInt(0))
    const beneficiary = network.createWallet(BigInt(0))
    const treasury = {
      address: Address.fromPubKeyHash(
        new PubKeyHash(treasuryPubKey.pubKeyHash.hex),
      ),
      rootKey: treasuryRootKey,
    }

    /** Locking Datum **/
    const oneHour = 3_600_000
    const oneMinute = 60000
    const scriptTime = 1655683301000
    const createdAt = new Date(scriptTime - oneHour)
    const cancelDeadline = new Date(scriptTime - oneHour / 2)
    const releaseDate = new Date(scriptTime - oneMinute)
    const paymentToken: [number[], bigint][] = [
      [textToBytes("PIGGY"), BigInt(5000)],
    ]
    const paymentTokenAssets = new Assets([
      [PAYMENT_TOKEN_POLICYID, paymentToken],
    ])
    const paymentTokens = new Value(25_000_000, paymentTokenAssets)

    const escrowDatum = new escrowProgram.types.Datum(
      beneficiary.pubKeyHash.hex, // beneficiaryPkh
      benefactor.pubKeyHash.hex, // benefactorPkh
      BigInt(Math.floor(releaseDate.getTime())), // releaseDate
      20, // cancelFee %
      BigInt(Math.floor(cancelDeadline.getTime())), // cancelWindowStart
      BigInt(Math.floor(cancelDeadline.getTime())), // cancelWindowEnd
      BigInt(Math.floor(createdAt.getTime())), // createdAt
      paymentTokens,
    )
    const inlineDatum = Datum.inline(escrowDatum)

    network.createUtxo(benefactor, BigInt(100_000_000))
    network.createUtxo(beneficiary, BigInt(5_000_000))
    // network.createUtxo(treasury, BigInt(5_000_000))
    network.tick(BigInt(1))

    context.datum = inlineDatum
    context.network = network
    context.benefactor = benefactor
    context.beneficiary = beneficiary
    context.treasury = treasury
  })

  /** Test #1A **/
  it("Successful transaction", async ({
    beneficiary,
    benefactor,
    treasury,
    network,
    datum,
  }: CustomContext) => {
    let benefactorUtxos = await benefactor.utxos
    let beneficiaryUtxos = await beneficiary.utxos

    // lock funds
    const lockingTx = new Tx()
      .addInputs(benefactorUtxos)
      .addOutputs([
        new TxOutput(scriptAddress, new Value(BigInt(5_000_000)), datum),
      ])
    await lockingTx.finalize(networkParams, benefactor.address)
    await network.submitTx(lockingTx)
    network.tick(BigInt(100))

    // unlock funds
    benefactorUtxos = await benefactor.utxos
    beneficiaryUtxos = await beneficiary.utxos
    const params = await network.getParameters()

    let escrowUtxos = await network.getUtxos(scriptAddress)
    const escrowTxIds = escrowUtxos.map((utxo) => utxo.outputId)[0]

    const escrowScript = await fs.readFile(
      "./src/Escrow/EscrowContract.hl",
      "utf8",
    )
    const escrowProgram = Program.new(escrowScript)
    const redeemer = new escrowProgram.types.Redeemer.Cancel(escrowTxIds)

    // what needs to be sent to unlock a UTxO
    const unlockingTx = new Tx()
      .attachScript(escrowProgramCompiled)
      .addInputs(beneficiaryUtxos)
      .addInputs([escrowUtxos[0], escrowUtxos[1]], redeemer)
      .validFrom(params.liveSlot)
      .validTo(params.liveSlot + BigInt(10))
      .addSigner(beneficiary.pubKeyHash)
      .addOutputs([
        new TxOutput(beneficiary.address, new Value(5_000_000), datum),
        new TxOutput(
          treasury.address,
          calculateServiceFee(new Value(15_000_000)),
        ),
      ])

    await unlockingTx.finalize(networkParams, beneficiary.address)
    await network.submitTx(unlockingTx)
    network.tick(BigInt(1))

    escrowUtxos = await network.getUtxos(scriptAddress)
    benefactorUtxos = await benefactor.utxos
    beneficiaryUtxos = await beneficiary.utxos

    debugger

    // expect:
    // - contract to have no UTxOs
    // - beneficiary have more than 5 ADA in a wallet
    // - treasury to have ... in a wallet
  })

  /** Test #2 **/
  it("Successful transaction (with a BetaTester NFT)", async ({
    benefactor,
    network,
    beneficiary,
  }: CustomContext) => {
    const nftTokenName = ByteArrayData.fromString(BETA_TESTER_NFT_NAME).toHex()
    const tokens: [number[], bigint][] = [[hexToBytes(nftTokenName), BigInt(1)]]
    const redeemer = new ConstrData(1, [])

    network.createUtxo(
      beneficiary,
      2_000_000n,
      new Assets([[BETA_TESTER_NFT_POLICYID, tokens]]),
    )
    network.tick(1n)
  })
})
