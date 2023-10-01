import * as helios from "@hyperionbt/helios"
import fs from "fs/promises"
import { cwd } from "process"

const dir = cwd()
const donationSrc = (
  await fs.readFile(`${dir}/src/Escrow/helios/EscrowContract.hl`)
).toString()
const program = helios.Program.new(donationSrc)
const simplify = false
const uplcProgram = program.compile(simplify)

const vHash = uplcProgram.validatorHash
console.log("escrow contract hash: ", vHash.hex)
console.log(
  "escrow contract address: ",
  helios.Address.fromValidatorHash(vHash).toBech32(),
)

await fs.writeFile(
  `${dir}/output/plutus-scripts/escrow.plutus`,
  uplcProgram.serialize(),
)
await fs.writeFile(`${dir}/output/plutus-scripts/escrow.hash`, vHash.hex)
await fs.writeFile(
  `${dir}/output/plutus-scripts/escrow.addr`,
  helios.Address.fromValidatorHash(vHash).toBech32(),
)
