import { BindingSettings, generate_binding, Language, Target } from "@completium/archetype-binder-ts";
import { RawContractInterface } from "@completium/archetype-binder-ts/build/src/utils";

const fs = require('fs')
const path = require('path')

const path_contracts = '../tests/passed/'

const write_binding = (filename: string) => {
  const json = fs.readFileSync(filename);
  let rci: RawContractInterface = JSON.parse(json);
  const settings: BindingSettings = {
    language: Language.Archetype,
    target: Target.Experiment,
    path: path_contracts
  }
  const output = generate_binding(rci, settings);
  const out_ts = './bindings/passed/' + path.basename(filename.replace('.json', '.ts'));
  fs.writeFileSync(out_ts, output)
}

describe('Generate binding', async () => {
  describe('Passed', async () => {
    const p = './json/passed'
    const dir = fs.opendirSync(p)
    let dirent;
    while ((dirent = dir.readSync()) !== null) {
      const filename = dirent.name as string;
      if (filename.endsWith(".json")) {
        it(filename, async () => {
          write_binding(p + '/' + filename)
        });
      }
    }
    dir.closeSync()
  })
})
