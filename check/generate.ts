import { BindingSettings, generate_binding, Language, Target } from "@completium/archetype-binder-ts";
import { RawContractInterface } from "@completium/archetype-binder-ts/build/src/utils";

const fs = require('fs')
const path = require('path')
const spawn = require('cross-spawn');

const skip: Array<string> = []

const compile = (args: string[]) => {
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, args, {});
  return res
}

const create_folder = (dir: string) => {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir);
  }
}

const generation_interface_contract = (i: string, p : string): string => {
  const arl_path = i + '.arl'
  const res = compile(['--show-contract-interface', arl_path])
  if (res.status != 0) {
    throw new Error(res.stderr.toString())
  }
  const content = JSON.parse(res.stdout.toString())
  const output = JSON.stringify(content, null, 2);
  const json_path = p + path.basename(arl_path.replace('.arl', '.json'))
  fs.writeFileSync(json_path, output)
  return json_path
}

const generation_tz_file = (i: string, p: string): string => {
  const arl_path = i + '.arl'
  const res = compile([arl_path])
  if (res.status != 0) {
    throw new Error(res.stderr.toString())
  }
  const output_path = p + path.basename(arl_path.replace('.arl', '.tz'))
  fs.writeFileSync(output_path, res.stdout.toString())
  return output_path
}

const write_binding = (json_path: string, p : string, op : string) => {
  const json = fs.readFileSync(json_path);
  let rci: RawContractInterface = JSON.parse(json);
  const settings: BindingSettings = {
    language: Language.Archetype,
    target: Target.Experiment,
    path: p
  }
  const output = generate_binding(rci, settings);
  const out_ts = op + path.basename(json_path.replace('.json', '.ts'));
  fs.writeFileSync(out_ts, output)
}

const generate_spec_passed = (input: Array<string>) => {
  const extract_it_body = (): Map<string, string> => {
    const res = new Map<string, string>();
    const input = fs.readFileSync('./tests/passed.spec.ts');

    const frx = /describe\('passed', async \(\) => {(.|\n)+/g
    const fr = frx.exec(input)
    if (fr == null) {
      throw new Error("error");
    }
    const aaa: string = fr[0]
    const aa = aaa.split('\n  it')
    for (let idx = 0; idx < aa.length; ++idx) {
      const b = aa[idx]
      const ii = b.trim()

      const y = ii.split('\n');
      const z = y[0].split('\'')
      const id = z[1]

      const tmp_content = ii.split('\n');
      const content = tmp_content.slice(1, tmp_content.length - (idx == aa.length - 1 ? 3 : 1)).join('\n')

      res.set(id, '\n' + content + '\n  ')
    }

    return res
  }

  const it_bodies = extract_it_body();

  let imports: Array<string> = []
  let items: Array<string> = []
  for (const id of input) {
    imports.push(`import * as ${id} from '../bindings/passed/${id}'\n`)

    const default_body = `
    await ${id}.${id}.deploy({ as: alice })
    `
    const body = it_bodies.get(id) ?? default_body
    items.push(`
  it('${id}', async () => {${body}${it_bodies.has(id) ? "" : "// TODO\n  "}})
`);
  }

  const output = `/* DO NOT EDIT, GENERATED FILE */
import { expect_to_fail, get_account, get_chain_id, get_mockup_level, get_mockup_now, pack, register_global_constant, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts';
import { Address, Bytes, Chain_id, Duration, Int, Key_hash, Micheline, MichelineType, Nat, Option, Or, Rational, Sapling_state, Sapling_transaction, Tez, Ticket, Unit } from '@completium/archetype-ts-types';

import assert from 'assert'
import { BigNumber } from 'bignumber.js'

${(imports.map(x => x)).join('')}

const alice = get_account('alice')
const bob = get_account('bob')
const carl = get_account('carl')

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Tests-------------------------------------------------------------------- */

describe('passed', async () => {
${(items.map(x => x)).join('')}
})`
  fs.writeFileSync('./tests/passed.spec.ts', output)
}

const generate_spec_error = (input: Array<string>, name: string, path: string, code: number, ext: string, args: string) => {
  let items: Array<string> = []
  for (const id of input) {
    items.push(`
  it('${id}', async () => {
    const stat = compile("${path}/${id}${ext}")
    assert(stat.status == ${code}, "Invalid status code, actual: " + stat.status + ", expected: ${code}")
  })`);
  }
  const output = `/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, [${args}p], { });
  return res
}

/* Tests ------------------------------------------------------------------- */

describe('${name}', async () => {${(items.map(x => x)).join('')}
})
  `
  fs.writeFileSync(`./tests/${name}.spec.ts`, output)
}

const extract_file_dir = (path: string, ext: string): Array<string> => {
  const dir = fs.opendirSync(path)
  let dirent;
  let filenames: Array<string> = []
  while ((dirent = dir.readSync()) !== null) {
    const filename = dirent.name as string;
    if (filename.endsWith(ext)) {
      if (!skip.includes(filename)) {
        const f: string = filename.substring(0, (filename.length - ext.length));
        filenames.push(f)
      }
    }
  }
  dir.closeSync()
  filenames.sort((x, y) => (x > y ? 1 : -1));
  return filenames
}

describe('Generate binding', async () => {
  describe('contracts', async () => {
    const p = '../tests/contracts'
    const ids = ['a2', 'fa1.2', 'fa2', 'fa2.1', 'harbinger', 'multisig', 'poll']
    for (const id of ids) {
      it(id, () => {
        const pa = p + '/' + id;
        const out_michelson_path = `./michelson/contracts/${id}/`;
        create_folder(out_michelson_path)
        const out_json_path = `./json/contracts/${id}/`;
        create_folder(out_json_path)
        const out_ts_path = `./bindings/contracts/${id}/`;
        create_folder(out_ts_path)
        const filenames = extract_file_dir(pa, '.arl')
        for (const filename of filenames) {
          const filepath = pa + '/' + filename
          generation_tz_file(filepath, out_michelson_path)
          const json_path = generation_interface_contract(filepath, out_json_path)
          write_binding(json_path, pa + '/', out_ts_path)
        }
      })
    }
  })
  describe('passed', async () => {
    const p = '../tests/passed'
    const filenames = extract_file_dir(p, '.arl')
    for (const filename of filenames) {
      it(filename, () => {
        const filepath = p + '/' + filename
        const json_path = generation_interface_contract(filepath, './json/passed/')
        generation_tz_file(filepath, './michelson/passed/')
        write_binding(json_path, '../tests/passed/', './bindings/passed/')
      });
    }
    it('Generate passed.spec.ts', async () => {
      generate_spec_passed(filenames)
    })
  })

  describe('Generate spec.ts files', async () => {
    const items: Array<[string, string, string, string, number]> = [
      ['.arl', 'passed-errors', '../tests/passed', '', 0],
      ['.arl', 'syntax-errors', '../tests/syntax-errors', '', 1],
      ['.arl', 'type-errors', '../tests/type-errors', '', 3],
      ['.arl', 'model-errors', '../tests/model-errors', '', 5],
      ['.arl', 'proposal-type-errors', '../tests/proposal-type-errors', '', 3],
      ['.arl', 'proposal-model-errors', '../tests/proposal-model-errors', '', 5],
      ['.tz', 'decomp-mic-passed', 'michelson/passed', "'-d', '-mi', ", 0],
      ['.tz', 'decomp-mit-passed', 'michelson/passed', "'-d', '-mit', ", 0],
      ['.tz', 'decomp-dir-passed', 'michelson/passed', "'-d', '-dir', ", 0],
      ['.arl', 'decomp-ama-passed', '../tests/passed', "'-d', '-ama', ", 0],
      ['.tz', 'decomp-mic-mainnet', '../mainnet/mainnet_contracts_2023-07-05/tz', "'-d', '-mi', ", 0],
      ['.tz', 'decomp-mit-mainnet', '../mainnet/mainnet_contracts_2023-07-05/tz', "'-d', '-mit', ", 0],
      ['.tz', 'decomp-dir-mainnet', '../mainnet/mainnet_contracts_2023-07-05/tz', "'-d', '-dir', ", 0]
    ]
    for (const item of items) {
      const ext  = item[0]
      const name = item[1]
      const path = item[2]
      const args = item[3]
      const code = item[4]

      it(name, () => {
        const filenames = extract_file_dir(path, ext)
        generate_spec_error(filenames, name, path, code, ext, args)
      })
    }
  })
})
