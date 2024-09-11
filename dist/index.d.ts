import { Cast, UserFunc } from "@helios-lang/contract-utils";
import { Address, AssetClass, DatumHash, MintingPolicyHash, PubKey, PubKeyHash, SpendingCredential, StakingCredential, StakingHash, StakingValidatorHash, TimeRange, TxId, TxInput, TxOutput, TxOutputDatum, ValidatorHash, Value } from "@helios-lang/ledger";
import type { IntLike } from "@helios-lang/codec-utils";
import type { CastConfig } from "@helios-lang/contract-utils";
import type { ConfigurableCast } from "@helios-lang/contract-utils";
import type { TimeLike } from "@helios-lang/ledger";
import type { UplcData } from "@helios-lang/ledger";
import type { UplcProgram } from "@helios-lang/uplc";
export const escrow_contract: {
    $name: "escrow_contract"
    $purpose: "spending"
    $currentScriptIndex: 0
    $sourceCode: string
    $dependencies: []
    $hashDependencies: []
    $dependsOnOwnHash: boolean
    $Redeemer: ConfigurableCast<{Cancel: {txId: TxId, utxoIdx: bigint}} | {Complete: {txOutIds: ({txId: TxId, utxoIdx: bigint})[]}} | {Recycle: {}}, {Cancel: {txId: TxId | string | number[], utxoIdx: IntLike}} | {Complete: {txOutIds: ({txId: TxId | string | number[], utxoIdx: IntLike})[]}} | {Recycle: {}}>
    $Datum: ConfigurableCast<{beneficiaryPkh: PubKeyHash, benefactorPkh: PubKeyHash, releaseDate: number, cancelFee: bigint, cancelWindowStart: number, cancelWindowEnd: number, createdAt: number, paymentTokens: string}, {beneficiaryPkh: PubKeyHash | string | number[], benefactorPkh: PubKeyHash | string | number[], releaseDate: TimeLike, cancelFee: IntLike, cancelWindowStart: TimeLike, cancelWindowEnd: TimeLike, createdAt: TimeLike, paymentTokens: string}>,
    $types: {
        Datum: ConfigurableCast<{beneficiaryPkh: PubKeyHash, benefactorPkh: PubKeyHash, releaseDate: number, cancelFee: bigint, cancelWindowStart: number, cancelWindowEnd: number, createdAt: number, paymentTokens: string}, {beneficiaryPkh: PubKeyHash | string | number[], benefactorPkh: PubKeyHash | string | number[], releaseDate: TimeLike, cancelFee: IntLike, cancelWindowStart: TimeLike, cancelWindowEnd: TimeLike, createdAt: TimeLike, paymentTokens: string}>,
        TxOutId: ConfigurableCast<{txId: TxId, utxoIdx: bigint}, {txId: TxId | string | number[], utxoIdx: IntLike}>,
        Redeemer: ConfigurableCast<{Cancel: {txId: TxId, utxoIdx: bigint}} | {Complete: {txOutIds: ({txId: TxId, utxoIdx: bigint})[]}} | {Recycle: {}}, {Cancel: {txId: TxId | string | number[], utxoIdx: IntLike}} | {Complete: {txOutIds: ({txId: TxId | string | number[], utxoIdx: IntLike})[]}} | {Recycle: {}}>,
    },
    $functions: {
        "TREASURY_PKH": (uplc: UplcProgram, config: CastConfig) => UserFunc<{}, number[]>,
        "treasuryPkh": (uplc: UplcProgram, config: CastConfig) => UserFunc<{}, PubKeyHash>,
        "BETA_TESTER_MPH": (uplc: UplcProgram, config: CastConfig) => UserFunc<{}, number[]>,
        "betaTesterMph": (uplc: UplcProgram, config: CastConfig) => UserFunc<{}, MintingPolicyHash>,
        "minServiceFeePercent": (uplc: UplcProgram, config: CastConfig) => UserFunc<{}, bigint>,
        "minServiceFeeLovelace": (uplc: UplcProgram, config: CastConfig) => UserFunc<{}, bigint>,
        "getServiceFeeAmnt": (uplc: UplcProgram, config: CastConfig) => UserFunc<{lovelacePayout: IntLike, withBetaTesterToken: boolean}, Value>,
        "checkInputForBetaTesterToken": (uplc: UplcProgram, config: CastConfig) => UserFunc<{txIn: TxInput}, boolean>,
    },
}
