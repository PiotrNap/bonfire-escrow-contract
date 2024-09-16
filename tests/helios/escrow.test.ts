import { Context, beforeEach, describe, expect, it } from "vitest"

import {
  Address,
  Assets,
  ByteArrayData,
  Datum,
  NetworkEmulator,
  Program,
  Tx,
  TxBuilder,
  TxOutput,
  ValidatorHash,
  Value,
  hexToBytes,
  textToBytes,
} from "@helios-lang/compat"
import { Emulator, RootPrivateKey, SimpleWallet } from "@helios-lang/tx-utils"
import { DEFAULT_NETWORK_PARAMS } from "@helios-lang/ledger-conway"
import { escrow_contract } from "../../dist"

import { ContractContextBuilder } from "@helios-lang/contract-utils"

type CustomContext = Context & {
  beneficiary: SimpleWallet
  benefactor: SimpleWallet
  treasury: SimpleWallet
  network: NetworkEmulator
  datum: any
  datum2: any
}

/**
 * Create multiple test scenarios
 *
 *
 * A' (benefactor) O' (beneficiary) T' (treasury)
 *
 *
 * 1. Successful transaction
 *      - A' locks funds into contract
 *      - O' unlocks the funds after a release window
 *      - T' receives fees for successful deal
 *
 * 2. Successful transaction (with a BetaTester NFT)
 *      - A' locks funds into contract
 *      - O' tries to unlock the funds after a release window with BetaTester NFT
 *      - T' does not receive any fee
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
  "481146d15d0c9bacc880254f88f944f6a88dba2e917d35fcbf92aa24"
const BETA_TESTER_NFT_NAME = "BETA#1"
const TREASURY_TEST_SEED =
  "expand good gun morning wall assault heart punch access magic spoon tag"

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
  } else if (lovelace / 100n < 1_500_00n) {
    return new Value(lovelace / 100n)
  } else return new Value(1_500_000n)
}

function calculateCancellationFee(
  paymentLovelace: number,
  cancellationRate: number,
): Value {
  return new Value(paymentLovelace * (cancellationRate / 100))
}

describe("Escrow contract with 3 actions: Cancel, Complete, Recycle", async () => {
  try {
    const contractContext = ContractContextBuilder.new()
      .with(escrow_contract)
      .build({
        isMainnet: false,
      })

    const escrowProgram = new Program(escrow_contract.$sourceCode).compile()
    const escrowValidatorHash = Buffer.from(escrowProgram.hash()).toString(
      "hex",
    )
    const scriptAddress = Address.fromHash(
      false,
      new ValidatorHash(escrowValidatorHash),
    )

    const networkParams = DEFAULT_NETWORK_PARAMS()
    const oneYearMilSec = 1000 * 60 * 60 * 24 * 365
    const oneHourMilSec = 3_600_000
    const oneMinuteMilSec = 60_000
    const scriptTime = 1655683301000 // default time inside validator (networkEmulator?)

    const paymentTokenName = textToBytes("PIGGY")
    const paymentTokenAmt = BigInt(5000)
    const paymentToken: [number[], bigint][] = [
      [paymentTokenName, paymentTokenAmt],
    ]
    const paymentTokenAssets = new Assets([
      [PAYMENT_TOKEN_POLICYID, paymentToken],
    ])
    const paymentLovelace = 25_000_000
    const paymentTokensValue = new Value(paymentLovelace, paymentTokenAssets)

    const nftTokenName = ByteArrayData.fromString(BETA_TESTER_NFT_NAME).toHex()
    const tokens: [number[], bigint][] = [[hexToBytes(nftTokenName), BigInt(1)]]
    const betaTesterToken = new Assets([[BETA_TESTER_NFT_POLICYID, tokens]])

    const serviceFee = calculateServiceFee(paymentTokensValue)

    beforeEach(async (context: CustomContext) => {
      const network = new Emulator()
      const treasuryRootKey = RootPrivateKey.fromPhrase(
        TREASURY_TEST_SEED.split(" "),
      )
      const treasurySpendingKey = treasuryRootKey.deriveSpendingRootKey(0)
      const treasuryStakingKey = treasuryRootKey.deriveStakingRootKey(0)
      const benefactor = network.createWallet(BigInt(0))
      const beneficiary = network.createWallet(BigInt(0))
      const treasury = new SimpleWallet(
        treasurySpendingKey,
        treasuryStakingKey,
        network,
      )

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
    }: CustomContext) => {
      /** Locking Datum **/
      const createdAt = new Date(scriptTime - oneHourMilSec)
      const cancelWindowStart = new Date(scriptTime - oneHourMilSec / 2)
      const cancelWindowEnd = new Date(scriptTime + oneHourMilSec)
      const releaseDate = new Date(
        scriptTime + oneHourMilSec + oneHourMilSec / 2,
      )

      const escrowDatum = contractContext.escrow_contract.Datum.toUplcData({
        beneficiaryPkh: beneficiary.spendingPubKeyHash,
        benefactorPkh: benefactor.spendingPubKeyHash,
        releaseDate: BigInt(Math.floor(releaseDate.getTime())),
        cancelFee: 20,
        cancelWindowStart: BigInt(Math.floor(cancelWindowStart.getTime())),
        cancelWindowEnd: BigInt(Math.floor(cancelWindowEnd.getTime())),
        createdAt: BigInt(Math.floor(createdAt.getTime())),
        paymentTokens: paymentTokensValue.toUplcData().toSchemaJson(),
      })
      const inlineDatum = Datum.Inline(escrowDatum)
      const serviceFee = calculateServiceFee(
        paymentTokensValue.add(paymentTokensValue),
      )
      /** create some utxos **/
      network.createUtxo(benefactor, BigInt(25_000_000), paymentTokenAssets)
      network.createUtxo(benefactor, BigInt(25_000_000), paymentTokenAssets)
      network.createUtxo(benefactor, BigInt(50_000_000))
      network.createUtxo(beneficiary, BigInt(5_000_000)) // collateral utxo
      network.createUtxo(beneficiary, BigInt(25_000_000))
      network.tick(BigInt(releaseDate.getTime() + oneMinuteMilSec))

      let benefactorUtxos = await benefactor.utxos
      let beneficiaryUtxos = await beneficiary.utxos

      // lock funds
      const lockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .spend(benefactorUtxos)
        .payUnsafe(scriptAddress, paymentTokensValue, inlineDatum)
        .payUnsafe(scriptAddress, paymentTokensValue, inlineDatum)

      const readyLockingTx = await lockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
        spareUtxos: benefactor.utxos,
      })
      let signature = await benefactor.signTx(readyLockingTx)
      readyLockingTx.addSignatures(signature)
      await network.submitTx(readyLockingTx)

      network.tick(BigInt(100))

      // unlock funds
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos
      const params = await network.parameters

      let escrowUtxos = await network.getUtxos(scriptAddress)
      const escrowTxIds = escrowUtxos.map((utxo) => utxo.id)

      const frstTxOutId = {
        txId: escrowTxIds[0].txId.toHex(),
        utxoIdx: escrowTxIds[0].utxoIdx,
      }

      const sndTxOutId = {
        txId: escrowTxIds[1].txId.toHex(),
        utxoIdx: escrowTxIds[1].utxoIdx,
      }
      const redeemer = contractContext.escrow_contract.Redeemer.toUplcData({
        Complete: {
          txOutIds: [frstTxOutId, sndTxOutId],
        },
      })

      const unlockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(beneficiaryUtxos)
        .spendUnsafe(escrowUtxos, redeemer)
        .payUnsafe(beneficiary.address, paymentTokensValue, inlineDatum)
        .payUnsafe(beneficiary.address, paymentTokensValue, inlineDatum)
        .pay(treasury.address, serviceFee.multiply(2))
        .validFromSlot(params.refTipSlot)
        .validToSlot(params.refTipSlot + 10)
        .addSigners(beneficiary.spendingPubKeyHash)

      const readyUnlockingTx = await unlockingTx.build({
        networkParams,
        changeAddress: beneficiary.address,
        spareUtxos: beneficiary.utxos,
      })

      signature = await beneficiary.signTx(readyUnlockingTx)
      readyUnlockingTx.addSignatures(signature)

      await network.submitTx(readyUnlockingTx)
      network.tick(BigInt(1))

      escrowUtxos = await network.getUtxos(scriptAddress)
      benefactorUtxos = await network.getUtxos(benefactor.address)
      beneficiaryUtxos = await network.getUtxos(beneficiary.address)
      let treasuryUtxos = await network.getUtxos(treasury.address)

      // console.log(
      //   1,
      //   benefactorUtxos.map((u) => JSON.stringify(u.dump())),
      // )
      // console.log(
      //   2,
      //   beneficiaryUtxos.map((u) => JSON.stringify(u.dump())),
      // )
      // console.log(
      //   3,
      //   treasuryUtxos.map((u) => JSON.stringify(u.dump())),
      // )

      // contract to have no UTxOs
      expect(escrowUtxos.length).toBe(0)

      // beneficiary have the UTxO with payment tokens
      expect(
        beneficiaryUtxos.filter((txInput) =>
          txInput.output.value.isEqual(paymentTokensValue),
        ),
      ).toHaveLength(2)

      // - treasury to have <serviceFee * 2> sitting at its address
      expect(
        treasuryUtxos.length == 1 &&
          treasuryUtxos[0].output.value.isEqual(serviceFee.multiply(2)),
      ).toBeTruthy()
    })

    /** Test #2 **/
    it("Successful transaction (with a BetaTester NFT)", async ({
      benefactor,
      network,
      beneficiary,
      treasury,
    }: CustomContext) => {
      /** Locking Datum **/
      const createdAt = new Date(scriptTime - oneHourMilSec)
      const cancelWindowStart = new Date(scriptTime - oneHourMilSec / 2)
      const cancelWindowEnd = new Date(scriptTime + oneHourMilSec)
      const releaseDate = new Date(
        scriptTime + oneHourMilSec + oneHourMilSec / 2,
      )

      const escrowDatum = contractContext.escrow_contract.Datum.toUplcData({
        beneficiaryPkh: beneficiary.spendingPubKeyHash,
        benefactorPkh: benefactor.spendingPubKeyHash,
        releaseDate: BigInt(Math.floor(releaseDate.getTime())),
        cancelFee: 20,
        cancelWindowStart: BigInt(Math.floor(cancelWindowStart.getTime())),
        cancelWindowEnd: BigInt(Math.floor(cancelWindowEnd.getTime())),
        createdAt: BigInt(Math.floor(createdAt.getTime())),
        paymentTokens: paymentTokensValue.toUplcData().toSchemaJson(),
      })

      network.createUtxo(benefactor, BigInt(100_000_000), paymentTokenAssets)
      network.createUtxo(beneficiary, 5_000_000n, betaTesterToken)
      network.createUtxo(beneficiary, 5_000_000n)
      network.tick(1n)

      let benefactorUtxos = await benefactor.utxos
      let beneficiaryUtxos = await beneficiary.utxos

      const inlineDatum = Datum.Inline(escrowDatum)

      // lock funds
      const lockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .spend(benefactorUtxos)
        .payUnsafe(scriptAddress, paymentTokensValue, inlineDatum)

      const readyLockingTx = await lockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
        spareUtxos: benefactor.utxos,
      })

      let sign = await benefactor.signTx(readyLockingTx)
      readyLockingTx.addSignatures(sign)

      await network.submitTx(readyLockingTx)
      network.tick(BigInt(releaseDate.getTime() + oneMinuteMilSec))

      // unlock funds
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos

      let escrowUtxos = await network.getUtxos(scriptAddress)
      const escrowTxIds = escrowUtxos.map((utxo) => utxo.id)

      const txOutId = {
        txId: escrowTxIds[0].txId.toHex(),
        utxoIdx: escrowTxIds[0].utxoIdx,
      }
      const redeemer = contractContext.escrow_contract.Redeemer.toUplcData({
        Complete: {
          txOutIds: [txOutId],
        },
      })
      const params = await network.parameters

      const unlockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(beneficiaryUtxos)
        .spendUnsafe([escrowUtxos[0]], redeemer)
        .validFromSlot(params.refTipSlot)
        .validToSlot(params.refTipSlot + 10)
        .addSigners(beneficiary.spendingPubKeyHash)
        .payUnsafe(beneficiary.address, paymentTokensValue, inlineDatum)

      const readyUnlockingTx = await unlockingTx.build({
        networkParams,
        changeAddress: beneficiary.address,
        spareUtxos: beneficiary.utxos,
      })

      sign = await beneficiary.signTx(readyUnlockingTx)
      readyUnlockingTx.addSignatures(sign)
      await network.submitTx(readyUnlockingTx)
      network.tick(BigInt(1))

      escrowUtxos = await network.getUtxos(scriptAddress)
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos
      let treasuryUtxos = await network.getUtxos(treasury.address)

      // contract to have no UTxOs
      expect(escrowUtxos.length).toBe(0)

      // - beneficiary to have the UTxO with the right amount of payment tokens
      expect(
        beneficiaryUtxos.find((txInput) =>
          txInput.value.isEqual(paymentTokensValue),
        ),
      ).toBeTruthy()

      // - treasury to have none funds
      expect(treasuryUtxos.length).toBe(0)
    })

    /** Test #3 **/
    it("On-time cancellation", async ({
      benefactor,
      network,
      beneficiary,
      treasury,
    }: CustomContext) => {
      /** Locking Datum **/
      const createdAt = new Date(scriptTime - oneHourMilSec)
      const cancelWindowStart = new Date(scriptTime - oneHourMilSec / 2)
      const cancelWindowEnd = new Date(scriptTime + oneHourMilSec)
      const releaseDate = new Date(
        scriptTime + oneHourMilSec + oneHourMilSec / 2,
      )

      const escrowDatum = contractContext.escrow_contract.Datum.toUplcData({
        beneficiaryPkh: beneficiary.spendingPubKeyHash,
        benefactorPkh: benefactor.spendingPubKeyHash,
        releaseDate: BigInt(Math.floor(releaseDate.getTime())),
        cancelFee: 20,
        cancelWindowStart: BigInt(Math.floor(cancelWindowStart.getTime())),
        cancelWindowEnd: BigInt(Math.floor(cancelWindowEnd.getTime())),
        createdAt: BigInt(Math.floor(createdAt.getTime())),
        paymentTokens: paymentTokensValue.toUplcData().toSchemaJson(),
      })

      network.createUtxo(benefactor, BigInt(100_000_000), paymentTokenAssets)
      network.tick(1n)

      let benefactorUtxos = await benefactor.utxos
      let beneficiaryUtxos = await beneficiary.utxos

      const inlineDatum = Datum.Inline(escrowDatum)
      const cancellationFee = calculateCancellationFee(paymentLovelace, 20)

      // lock funds
      const lockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .spend(benefactorUtxos)
        .payUnsafe(scriptAddress, paymentTokensValue, inlineDatum)
      const readyTx = await lockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
        spareUtxos: benefactor.utxos,
      })
      let sign = await benefactor.signTx(readyTx)
      readyTx.addSignatures(sign)

      await network.submitTx(readyTx)
      network.tick(BigInt(100))

      // unlock funds
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos
      const params = await network.parameters

      let escrowUtxos = await network.getUtxos(scriptAddress)

      const txId = escrowUtxos[0].id.txId
      const utxoIdx = escrowUtxos[0].id.utxoIdx
      const redeemer = contractContext.escrow_contract.Redeemer.toUplcData({
        Cancel: {
          txId,
          utxoIdx,
        },
      })

      const unlockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(benefactorUtxos)
        .spendUnsafe([escrowUtxos[0]], redeemer)
        .validFromSlot(params.refTipSlot)
        .validToSlot(params.refTipSlot + 10)
        .addSigners(benefactor.spendingPubKeyHash)
        .payUnsafe([
          new TxOutput(benefactor.address, paymentTokensValue, inlineDatum),
          new TxOutput(beneficiary.address, cancellationFee),
        ])

      const readyUnlockingTx = await unlockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
        spareUtxos: benefactor.utxos,
      })
      sign = await benefactor.signTx(readyUnlockingTx)
      readyUnlockingTx.addSignatures(sign)
      await network.submitTx(readyUnlockingTx)

      network.tick(BigInt(1))

      escrowUtxos = await network.getUtxos(scriptAddress)
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos
      let treasuryUtxos = await network.getUtxos(treasury.address)

      // contract to have no UTxOs
      expect(escrowUtxos.length).toBe(0)

      // beneficiary to have the UTxO with the right amount of payment tokens
      expect(
        benefactorUtxos.find((txInput) =>
          txInput.value.isEqual(paymentTokensValue),
        ),
      ).toBeTruthy()

      // benefactor to have a utxo with the cancellation fee
      expect(beneficiaryUtxos[0].value.isEqual(cancellationFee)).toBeTruthy()

      // treasury to have none funds
      expect(treasuryUtxos.length).toBe(0)
    })

    /** Test #4 + #5 **/
    it("Failed cancellation after deadline + failed withdrawal before release window", async ({
      benefactor,
      network,
      beneficiary,
      treasury,
    }: CustomContext) => {
      /** Locking Datum **/
      const createdAt = new Date(scriptTime - oneHourMilSec)
      const cancelWindowStart = new Date(scriptTime - oneHourMilSec / 2)
      const cancelWindowEnd = new Date(scriptTime + oneHourMilSec)
      const releaseDate = new Date(
        scriptTime + oneHourMilSec + oneHourMilSec / 2,
      )

      const escrowDatum = contractContext.escrow_contract.Datum.toUplcData({
        beneficiaryPkh: beneficiary.spendingPubKeyHash,
        benefactorPkh: benefactor.spendingPubKeyHash,
        releaseDate: BigInt(Math.floor(releaseDate.getTime())),
        cancelFee: 20,
        cancelWindowStart: BigInt(Math.floor(cancelWindowStart.getTime())),
        cancelWindowEnd: BigInt(Math.floor(cancelWindowEnd.getTime())),
        createdAt: BigInt(Math.floor(createdAt.getTime())),
        paymentTokens: paymentTokensValue.toUplcData().toSchemaJson(),
      })
      network.createUtxo(benefactor, BigInt(100_000_000), paymentTokenAssets)
      network.tick(1n)

      let benefactorUtxos = await benefactor.utxos
      let beneficiaryUtxos = await beneficiary.utxos

      const inlineDatum = Datum.Inline(escrowDatum)
      const cancellationFee = calculateCancellationFee(paymentLovelace, 20)

      // lock funds
      const lockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .spend(benefactorUtxos)
        .payUnsafe([
          new TxOutput(scriptAddress, paymentTokensValue, inlineDatum),
        ])
      const readyLockingTx = await lockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
        spareUtxos: benefactor.utxos,
      })
      let sign = await benefactor.signTx(readyLockingTx)
      readyLockingTx.addSignatures(sign)

      await network.submitTx(readyLockingTx)
      network.tick(BigInt(cancelWindowEnd.getTime() + oneMinuteMilSec))

      // unlock funds
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos
      const params = await network.parameters

      const escrowUtxos = await network.getUtxos(scriptAddress)

      const txId = escrowUtxos[0].id.txId
      const utxoIdx = escrowUtxos[0].id.utxoIdx
      const redeemer = contractContext.escrow_contract.Redeemer.toUplcData({
        Cancel: {
          txId,
          utxoIdx,
        },
      })

      const cancellingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(benefactorUtxos)
        .spendUnsafe([escrowUtxos[0]], redeemer)
        .validFromSlot(params.refTipSlot)
        .validToSlot(params.refTipSlot + 10)
        .addSigners(benefactor.spendingPubKeyHash)
        .payUnsafe([
          new TxOutput(benefactor.address, paymentTokensValue, inlineDatum),
          new TxOutput(beneficiary.address, cancellationFee),
        ])

      // should fail on off-chain evaluation due to cancellation date being passed
      await expect(() =>
        cancellingTx.build({
          networkParams,
          changeAddress: benefactor.address,
          spareUtxos: benefactor.utxos,
        }),
      ).rejects.toThrowError()

      network.tick(1n)

      const txOutId = {
        txId: escrowUtxos[0].id.txId.toHex(),
        utxoIdx: escrowUtxos[0].id.utxoIdx,
      }
      const withdrawRedeemer =
        contractContext.escrow_contract.Redeemer.toUplcData({
          Complete: {
            txOutIds: [txOutId],
          },
        })

      const withdrawingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(beneficiaryUtxos)
        .spendUnsafe([escrowUtxos[0]], withdrawRedeemer)
        .validFromSlot(params.refTipSlot)
        .validToSlot(params.refTipSlot + 10)
        .addSigners(benefactor.spendingPubKeyHash)
        .payUnsafe([
          new TxOutput(benefactor.address, paymentTokensValue, inlineDatum),
          new TxOutput(beneficiary.address, cancellationFee),
        ])

      // should fail on off-chain evaluation due to too early date for a release
      await expect(() =>
        withdrawingTx.build({
          networkParams,
          changeAddress: benefactor.address,
        }),
      ).rejects.toThrowError()
    })

    /** Test #6 **/
    it("Failed attempt to collect funds by any outside party", async ({
      benefactor,
      network,
      beneficiary,
    }: CustomContext) => {
      /** Locking Datum **/
      const createdAt = new Date(scriptTime - oneHourMilSec)
      const cancelWindowStart = new Date(scriptTime - oneHourMilSec / 2)
      const cancelWindowEnd = new Date(scriptTime + oneHourMilSec)
      const releaseDate = new Date(
        scriptTime + oneHourMilSec + oneHourMilSec / 2,
      )

      const outsideParty = network.createWallet(5_000_000n)

      const escrowDatum = contractContext.escrow_contract.Datum.toUplcData({
        beneficiaryPkh: beneficiary.spendingPubKeyHash,
        benefactorPkh: benefactor.spendingPubKeyHash,
        releaseDate: BigInt(Math.floor(releaseDate.getTime())),
        cancelFee: 20,
        cancelWindowStart: BigInt(Math.floor(cancelWindowStart.getTime())),
        cancelWindowEnd: BigInt(Math.floor(cancelWindowEnd.getTime())),
        createdAt: BigInt(Math.floor(createdAt.getTime())),
        paymentTokens: paymentTokensValue.toUplcData().toSchemaJson(),
      })

      network.createUtxo(benefactor, BigInt(100_000_000), paymentTokenAssets)
      network.tick(1n)

      let benefactorUtxos = await benefactor.utxos
      let beneficiaryUtxos = await beneficiary.utxos

      const inlineDatum = Datum.Inline(escrowDatum)

      // lock funds
      const lockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .spend(benefactorUtxos)
        .payUnsafe([
          new TxOutput(scriptAddress, paymentTokensValue, inlineDatum),
        ])
      const readyLockingTx = await lockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
      })
      let sign = await benefactor.signTx(readyLockingTx)
      readyLockingTx.addSignatures(sign)

      await network.submitTx(readyLockingTx)
      network.tick(BigInt(100))

      // unlock funds
      benefactorUtxos = await benefactor.utxos
      beneficiaryUtxos = await beneficiary.utxos
      const params = await network.parameters

      let escrowUtxos = await network.getUtxos(scriptAddress)

      const txOutId = {
        txId: escrowUtxos[0].id.txId.toHex(),
        utxoIdx: escrowUtxos[0].id.utxoIdx,
      }
      const withdrawRedeemer =
        contractContext.escrow_contract.Redeemer.toUplcData({
          Complete: {
            txOutIds: [txOutId],
          },
        })
      const withdrawingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(beneficiaryUtxos)
        .spendUnsafe([escrowUtxos[0]], withdrawRedeemer)
        .validFromSlot(params.refTipSlot)
        .validToSlot(params.refTipSlot + 10)
        .addSigners(outsideParty.spendingPubKeyHash)
        .payUnsafe([
          new TxOutput(outsideParty.address, paymentTokensValue, inlineDatum),
        ])

      // should fail on off-chain evaluation due to tx being signed by an outside party
      await expect(() =>
        withdrawingTx.build({
          networkParams,
          changeAddress: outsideParty.address,
        }),
      ).rejects.toThrowError()
    })

    /** Test #8 & #7 **/
    it("Failed recycle of UTxOs younger than 1 year & successful recycle of UTxOs older than 1 year", async ({
      benefactor,
      network,
      beneficiary,
      treasury,
    }: CustomContext) => {
      let params = await network.parameters
      let currTime = params.refTipTime
      /** Locking Datum **/
      const createdAt = new Date(currTime - oneHourMilSec)
      const cancelWindowStart = new Date(currTime - oneHourMilSec / 2)
      const cancelWindowEnd = new Date(currTime + oneHourMilSec)
      const releaseDate = new Date(currTime + oneHourMilSec + oneHourMilSec / 2)

      const escrowDatum = contractContext.escrow_contract.Datum.toUplcData({
        beneficiaryPkh: beneficiary.spendingPubKeyHash,
        benefactorPkh: benefactor.spendingPubKeyHash,
        releaseDate: BigInt(Math.floor(releaseDate.getTime())),
        cancelFee: 20,
        cancelWindowStart: BigInt(Math.floor(cancelWindowStart.getTime())),
        cancelWindowEnd: BigInt(Math.floor(cancelWindowEnd.getTime())),
        createdAt: BigInt(Math.floor(createdAt.getTime())),
        paymentTokens: paymentTokensValue.toUplcData().toSchemaJson(),
      })

      network.createUtxo(benefactor, BigInt(100_000_000), paymentTokenAssets)
      network.createUtxo(treasury, BigInt(5_000_000))
      network.tick(1n)

      let benefactorUtxos = await benefactor.utxos

      const inlineDatum = Datum.Inline(escrowDatum)

      // lock funds
      const lockingTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .spend(benefactorUtxos)
        .payUnsafe([
          new TxOutput(scriptAddress, paymentTokensValue, inlineDatum),
        ])
      const readyLockingTx = await lockingTx.build({
        networkParams,
        changeAddress: benefactor.address,
      })
      let sign = await benefactor.signTx(readyLockingTx)
      readyLockingTx.addSignatures(sign)
      await network.submitTx(readyLockingTx)

      // 6 months have passed...
      const sixMonths = oneYearMilSec / 2
      network.tick(BigInt(sixMonths / 1000)) // converting to seconds...

      const newParams = await network.parameters
      currTime = newParams.refTipTime

      // unlock funds

      let treasuryUtxos = await network.getUtxos(treasury.address)
      const escrowUtxos = await network.getUtxos(scriptAddress)

      const recycleRedeemer =
        contractContext.escrow_contract.Redeemer.toUplcData({ Recycle: {} })

      let recycleTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(treasuryUtxos)
        .spendUnsafe([escrowUtxos[0]], recycleRedeemer)
        .validFromTime(currTime)
        .validToTime(currTime + 60 * 1000)
        .addSigners(treasury.spendingPubKeyHash)

      // should throw an error, if it doesn't something is wrong.
      await expect(() =>
        recycleTx.build({ networkParams, changeAddress: treasury.address }),
      ).rejects.toThrowError()

      // wait another 6 months... and 1 sec :)

      network.tick(BigInt(sixMonths / 1000))
      network.tick(BigInt(1))

      params = await network.parameters
      currTime = params.refTipTime

      recycleTx = new TxBuilder({ isMainnet: network.isMainnet() })
        .attachUplcProgram(escrowProgram)
        .spend(treasuryUtxos)
        .spendUnsafe([escrowUtxos[0]], recycleRedeemer)
        .validFromTime(currTime)
        .validToTime(currTime + 60 * 1000)
        .payUnsafe([new TxOutput(treasury.address, paymentTokensValue)])
        .addSigners(treasury.spendingPubKeyHash)

      expect(() =>
        recycleTx.build({ networkParams, changeAddress: treasury.address }),
      ).not.rejects
    })
  } catch (e) {
    console.error(e)
  }
})
