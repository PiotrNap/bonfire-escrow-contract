import { Cast, UserFunc } from "@helios-lang/contract-utils";
import { Address, AssetClass, DatumHash, MintingPolicyHash, PubKey, PubKeyHash, SpendingCredential, StakingCredential, StakingHash, StakingValidatorHash, TimeRange, TxId, TxInput, TxOutput, TxOutputDatum, ValidatorHash, Value } from "@helios-lang/ledger";
/**
 * @typedef {import("@helios-lang/codec-utils").IntLike} IntLike
 */
/**
 * @typedef {import("@helios-lang/contract-utils").CastConfig} CastConfig
 */
/**
 * @template TStrict
 * @template TPermissive
 * @typedef {import("@helios-lang/contract-utils").ConfigurableCast<TStrict, TPermissive>} ConfigurableCast
 */
/**
 * @typedef {import("@helios-lang/ledger").TimeLike} TimeLike
 */
/**
 * @typedef {import("@helios-lang/ledger").UplcData} UplcData
 */
/**
 * @typedef {import("@helios-lang/uplc").UplcProgram} UplcProgram
 */
export const escrow_contract = {
    $name: /** @type {const} */ ("escrow_contract"),
    $purpose: /** @type {const} */ ("spending"),
    $currentScriptIndex: /** @type {const} */ (0),
    $sourceCode: "spending escrow_contract\n\nimport { tx } from ScriptContext\n\nstruct Datum {\n    beneficiaryPkh : PubKeyHash\n    benefactorPkh : PubKeyHash\n    releaseDate : Time\n    cancelFee: Int // % of ADA paymentTokens\n    cancelWindowStart: Time\n    cancelWindowEnd: Time\n    createdAt : Time\n    paymentTokens : String // JSON schema of event cost \n}\n\nstruct TxOutId {\n    txId: TxId // hash of transaction\n    utxoIdx: Int\n}\n\nenum Redeemer {\n    Cancel {\n        txId: TxId\n        utxoIdx: Int\n    }\n    Complete {\n        txOutIds: []TxOutId\n    }\n    Recycle\n}\n\nconst TREASURY_PKH: ByteArray = #acae15d3719c18dc69683f3b4ffe7d83df06303a30ea3ebfaca434f5\nconst treasuryPkh: PubKeyHash = PubKeyHash::new(TREASURY_PKH)\n\nconst BETA_TESTER_MPH: ByteArray = #481146d15d0c9bacc880254f88f944f6a88dba2e917d35fcbf92aa24\nconst betaTesterMph: MintingPolicyHash = MintingPolicyHash::new(BETA_TESTER_MPH)\n\nconst minServiceFeePercent: Int = 3\nconst minServiceFeeLovelace: Int = 3000000\n\nfunc getServiceFeeAmnt(lovelacePayout: Int, withBetaTesterToken: Bool) -> Value {\n    if (withBetaTesterToken) {\n        Value::lovelace(0)\n    } else if ( lovelacePayout / 100 < minServiceFeeLovelace ) {\n        Value::lovelace(minServiceFeeLovelace)\n    } else {\n        Value::lovelace(lovelacePayout * ( minServiceFeePercent / 100))\n    }\n}\n\nfunc checkInputForBetaTesterToken(txIn: TxInput) -> Bool {\n    txIn.value.contains_policy(betaTesterMph)\n}\n\n/**\n* This validator should meet the following requirements:\n* \n*  1. Allow to cancel a transaction by one of two parties transacting with each other\n*  2. Allow to cancel only before the cancellation deadline\n*  3. Ensure all the funds are returned to benefactor during on-time cancellation \n*  4. Prevent from collecting funds before release date\n*  5. Allow to collect funds after release date by beneficiary\n*  6. Check whether sufficient fee was sent to the treasury (if any)\n*  7. Allow to recycle UTxOs older than 1 year by the treasury\n*/\n\nfunc main(datum: Datum, redeemer: Redeemer) -> Bool {\n    benefactorPkh: PubKeyHash = datum.benefactorPkh;\n    beneficiaryPkh: PubKeyHash = datum.beneficiaryPkh;\n    now: Time = tx.time_range.start;\n    betaTesterTokenPresent: Bool = tx.inputs.any(checkInputForBetaTesterToken);\n    oneYearTime: Duration = Duration::new(1000 * 60 * 60 * 24 * 365);\n\n    // How to handle 'txOutIds' with different beneficiary's or benefactor's ?\n\n    redeemer.switch {\n        cancelR: Cancel => {\n            cancelWindow: TimeRange = TimeRange::new(datum.cancelWindowStart, datum.cancelWindowEnd);\n            redeemerTxOutValue: Value = tx.inputs.find((in: TxInput) -> {\n                in.output_id.index == cancelR.utxoIdx &&\n                in.output_id.tx_id == cancelR.txId\n            }).value;\n\n            (datum.cancelWindowEnd > now).trace(\"1\") &&\n\n            if(cancelWindow.contains(now)) {\n                if(tx.is_signed_by(benefactorPkh)) {\n                    cFeeLove: Int = (redeemerTxOutValue.get_lovelace() * datum.cancelFee) / 100;\n\n                    // must return all payment tokens\n                     (tx.value_sent_to(benefactorPkh) >= redeemerTxOutValue).trace(\"2\") &&\n\n                    if (cFeeLove < 2000000  ) {\n                        (tx.value_sent_to(beneficiaryPkh).get_lovelace() >= 2000000  ).trace(\"3\")\n                    } else {\n                        (tx.value_sent_to(beneficiaryPkh).get_lovelace() >= cFeeLove).trace(\"4\")\n                    }\n                } else if (tx.is_signed_by(beneficiaryPkh)){\n                    (tx.value_sent_to(benefactorPkh) >= redeemerTxOutValue).trace(\"5\")\n                } else {\n                    false\n                }\n            } else {\n                (tx.value_sent_to(benefactorPkh) >= redeemerTxOutValue).trace(\"6\")\n            }\n        },\n        completeR: Complete => {\n            redeemerTxOutsValue: Value = tx.inputs.filter((txIn: TxInput) -> {\n                                 completeR.txOutIds.any((txOutId: TxOutId) -> {\n                                    txOutId.utxoIdx == txIn.output_id.index &&\n                                    txOutId.txId == txIn.output_id.tx_id\n                                    })\n                                }).fold((acc: Value, txIn: TxInput) -> Value {\n                                    acc + txIn.value\n                                },Value::ZERO);\n            serviceFee: Value = getServiceFeeAmnt(redeemerTxOutsValue.get_lovelace(), betaTesterTokenPresent);\n\n           (tx.is_signed_by(beneficiaryPkh)).trace(\"7\") &&\n\n           (now > datum.releaseDate).trace(\"8\") &&\n\n           (tx.value_sent_to(beneficiaryPkh) >= redeemerTxOutsValue)\n           .trace(\"9\") &&\n\n           (tx.value_sent_to(treasuryPkh) == serviceFee)\n           .trace(\"10 \" + serviceFee.show())\n        },\n        Recycle => {\n           (tx.is_signed_by(treasuryPkh)).trace(\"11\") &&\n\n           (now > datum.createdAt + oneYearTime).trace(\"12\")\n        }\n    }\n}",
    $dependencies: /** @type {const} */ ([]),
    $hashDependencies: [],
    $dependsOnOwnHash: false,
    $Redeemer: (config) => /** @type {Cast<{Cancel: {txId: TxId, utxoIdx: bigint}} | {Complete: {txOutIds: ({txId: TxId, utxoIdx: bigint})[]}} | {Recycle: {}}, {Cancel: {txId: TxId | string | number[], utxoIdx: IntLike}} | {Complete: {txOutIds: ({txId: TxId | string | number[], utxoIdx: IntLike})[]}} | {Recycle: {}}>} */ (new Cast({"kind":"enum","name":"Redeemer","id":"__module__escrow_contract__Redeemer[]","variantTypes":[{"kind":"variant","tag":0,"id":"__module__escrow_contract__Redeemer[]__Cancel","name":"Cancel","fieldTypes":[{"name":"txId","type":{"kind":"internal","name":"TxId"}},{"name":"utxoIdx","type":{"kind":"internal","name":"Int"}}]},{"kind":"variant","tag":1,"id":"__module__escrow_contract__Redeemer[]__Complete","name":"Complete","fieldTypes":[{"name":"txOutIds","type":{"kind":"list","itemType":{"kind":"struct","format":"list","id":"__module__escrow_contract__TxOutId[]","name":"TxOutId","fieldTypes":[{"name":"txId","type":{"kind":"internal","name":"TxId"}},{"name":"utxoIdx","type":{"kind":"internal","name":"Int"}}]}}}]},{"kind":"variant","tag":2,"id":"__module__escrow_contract__Redeemer[]__Recycle","name":"Recycle","fieldTypes":[]}]}, config)),
    $Datum: (config) => /** @type {Cast<{beneficiaryPkh: PubKeyHash, benefactorPkh: PubKeyHash, releaseDate: number, cancelFee: bigint, cancelWindowStart: number, cancelWindowEnd: number, createdAt: number, paymentTokens: string}, {beneficiaryPkh: PubKeyHash | string | number[], benefactorPkh: PubKeyHash | string | number[], releaseDate: TimeLike, cancelFee: IntLike, cancelWindowStart: TimeLike, cancelWindowEnd: TimeLike, createdAt: TimeLike, paymentTokens: string}>} */ (new Cast({"kind":"struct","format":"list","id":"__module__escrow_contract__Datum[]","name":"Datum","fieldTypes":[{"name":"beneficiaryPkh","type":{"kind":"internal","name":"PubKeyHash"}},{"name":"benefactorPkh","type":{"kind":"internal","name":"PubKeyHash"}},{"name":"releaseDate","type":{"kind":"internal","name":"Time"}},{"name":"cancelFee","type":{"kind":"internal","name":"Int"}},{"name":"cancelWindowStart","type":{"kind":"internal","name":"Time"}},{"name":"cancelWindowEnd","type":{"kind":"internal","name":"Time"}},{"name":"createdAt","type":{"kind":"internal","name":"Time"}},{"name":"paymentTokens","type":{"kind":"internal","name":"String"}}]}, config)),
    $types: {
        Datum: (config) => /** @type {Cast<{beneficiaryPkh: PubKeyHash, benefactorPkh: PubKeyHash, releaseDate: number, cancelFee: bigint, cancelWindowStart: number, cancelWindowEnd: number, createdAt: number, paymentTokens: string}, {beneficiaryPkh: PubKeyHash | string | number[], benefactorPkh: PubKeyHash | string | number[], releaseDate: TimeLike, cancelFee: IntLike, cancelWindowStart: TimeLike, cancelWindowEnd: TimeLike, createdAt: TimeLike, paymentTokens: string}>} */ (new Cast({"kind":"struct","format":"list","id":"__module__escrow_contract__Datum[]","name":"Datum","fieldTypes":[{"name":"beneficiaryPkh","type":{"kind":"internal","name":"PubKeyHash"}},{"name":"benefactorPkh","type":{"kind":"internal","name":"PubKeyHash"}},{"name":"releaseDate","type":{"kind":"internal","name":"Time"}},{"name":"cancelFee","type":{"kind":"internal","name":"Int"}},{"name":"cancelWindowStart","type":{"kind":"internal","name":"Time"}},{"name":"cancelWindowEnd","type":{"kind":"internal","name":"Time"}},{"name":"createdAt","type":{"kind":"internal","name":"Time"}},{"name":"paymentTokens","type":{"kind":"internal","name":"String"}}]}, config)),
        TxOutId: (config) => /** @type {Cast<{txId: TxId, utxoIdx: bigint}, {txId: TxId | string | number[], utxoIdx: IntLike}>} */ (new Cast({"kind":"struct","format":"list","id":"__module__escrow_contract__TxOutId[]","name":"TxOutId","fieldTypes":[{"name":"txId","type":{"kind":"internal","name":"TxId"}},{"name":"utxoIdx","type":{"kind":"internal","name":"Int"}}]}, config)),
        Redeemer: (config) => /** @type {Cast<{Cancel: {txId: TxId, utxoIdx: bigint}} | {Complete: {txOutIds: ({txId: TxId, utxoIdx: bigint})[]}} | {Recycle: {}}, {Cancel: {txId: TxId | string | number[], utxoIdx: IntLike}} | {Complete: {txOutIds: ({txId: TxId | string | number[], utxoIdx: IntLike})[]}} | {Recycle: {}}>} */ (new Cast({"kind":"enum","name":"Redeemer","id":"__module__escrow_contract__Redeemer[]","variantTypes":[{"kind":"variant","tag":0,"id":"__module__escrow_contract__Redeemer[]__Cancel","name":"Cancel","fieldTypes":[{"name":"txId","type":{"kind":"internal","name":"TxId"}},{"name":"utxoIdx","type":{"kind":"internal","name":"Int"}}]},{"kind":"variant","tag":1,"id":"__module__escrow_contract__Redeemer[]__Complete","name":"Complete","fieldTypes":[{"name":"txOutIds","type":{"kind":"list","itemType":{"kind":"struct","format":"list","id":"__module__escrow_contract__TxOutId[]","name":"TxOutId","fieldTypes":[{"name":"txId","type":{"kind":"internal","name":"TxId"}},{"name":"utxoIdx","type":{"kind":"internal","name":"Int"}}]}}}]},{"kind":"variant","tag":2,"id":"__module__escrow_contract__Redeemer[]__Recycle","name":"Recycle","fieldTypes":[]}]}, config)),
    },
    $functions: {
        "TREASURY_PKH": (uplc, config) => /** @type {UserFunc<{}, number[]>} */ (new UserFunc(uplc, {...({"name":"TREASURY_PKH","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[],"returns":{"kind":"internal","name":"ByteArray"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "treasuryPkh": (uplc, config) => /** @type {UserFunc<{}, PubKeyHash>} */ (new UserFunc(uplc, {...({"name":"treasuryPkh","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[],"returns":{"kind":"internal","name":"PubKeyHash"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "BETA_TESTER_MPH": (uplc, config) => /** @type {UserFunc<{}, number[]>} */ (new UserFunc(uplc, {...({"name":"BETA_TESTER_MPH","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[],"returns":{"kind":"internal","name":"ByteArray"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "betaTesterMph": (uplc, config) => /** @type {UserFunc<{}, MintingPolicyHash>} */ (new UserFunc(uplc, {...({"name":"betaTesterMph","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[],"returns":{"kind":"internal","name":"MintingPolicyHash"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "minServiceFeePercent": (uplc, config) => /** @type {UserFunc<{}, bigint>} */ (new UserFunc(uplc, {...({"name":"minServiceFeePercent","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[],"returns":{"kind":"internal","name":"Int"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "minServiceFeeLovelace": (uplc, config) => /** @type {UserFunc<{}, bigint>} */ (new UserFunc(uplc, {...({"name":"minServiceFeeLovelace","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[],"returns":{"kind":"internal","name":"Int"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "getServiceFeeAmnt": (uplc, config) => /** @type {UserFunc<{lovelacePayout: IntLike, withBetaTesterToken: boolean}, Value>} */ (new UserFunc(uplc, {...({"name":"getServiceFeeAmnt","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[{"name":"lovelacePayout","isOptional":false,"type":{"kind":"internal","name":"Int"}},{"name":"withBetaTesterToken","isOptional":false,"type":{"kind":"internal","name":"Bool"}}],"returns":{"kind":"internal","name":"Value"}}), castConfig: config, validatorIndices: __validatorIndices})),
        "checkInputForBetaTesterToken": (uplc, config) => /** @type {UserFunc<{txIn: TxInput}, boolean>} */ (new UserFunc(uplc, {...({"name":"checkInputForBetaTesterToken","requiresCurrentScript":false,"requiresScriptContext":false,"arguments":[{"name":"txIn","isOptional":false,"type":{"kind":"internal","name":"TxInput"}}],"returns":{"kind":"internal","name":"Bool"}}), castConfig: config, validatorIndices: __validatorIndices})),
    },
}
const __validatorIndices = {"escrow_contract":0};