import { BindingSettings, generate_binding, Language, Target } from "@completium/archetype-binder-ts";
import { RawContractInterface } from "@completium/archetype-binder-ts/build/src/utils";

const fs = require('fs')
const path = require('path')

const path_contracts = '../tests/passed/'

const write_binding = (input: string) => {
  const filename = input + '.json';
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

const skip : Array<string> = []

const generate_spec_template = (input: Array<string>) => {
  let imports: Array<string> = []
  let items: Array<string> = []
  for (const id of input) {
    imports.push(`import * as ${id} from '../bindings/passed/${id}'\n`)
    items.push(`

    // TODO
    it('${id}', async () => {
      await ${id}.${id}.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await ${id}.${id}.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await ${id}.${id}.exec({ as: alice })
      //      const res_after = await ${id}.${id}.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })`);
  }
  const output = `/* DO NOT EDIT, GENERATED FILE */
import { get_account, set_mockup, set_quiet } from '@completium/experiment-ts';
import assert from 'assert'
import * as att from '@completium/archetype-ts-types';

${(imports.map(x => x)).join('')}

const alice = get_account('alice')

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Tests-------------------------------------------------------------------- */

describe('Tests', async () => {
  describe('Passed', async () => {
${(items.map(x => x)).join('')}
  })
})
  `
  fs.writeFileSync('./tests/template.spec.ts', output)
}

describe('Generate binding', async () => {
  describe('Passed', async () => {
    const p = './json/passed'
    const dir = fs.opendirSync(p)
    let dirent;
    let filenames: Array<string> = []
    while ((dirent = dir.readSync()) !== null) {
      const filename = dirent.name as string;
      if (filename.endsWith(".json")) {
        if (!skip.includes(filename)) {
          const f : string = filename.substring(0, (filename.length -5));
          filenames.push(f)
        }
      }
    }
    dir.closeSync()
    filenames.sort((x, y) => (x > y ? 1 : -1));
    for (const filename of filenames) {
      it(filename, () => {
        write_binding(p + '/' + filename)
      });
    }
    // generate_spec_template(filenames)
  })
})
