import { BindingSettings, generate_binding, Language, Target } from "@completium/archetype-binder-ts";
import { RawContractInterface } from "@completium/archetype-binder-ts/build/src/utils";

const fs = require('fs')
const path = require('path')

const skip: Array<string> = []

const write_binding = (input: string) => {
  const filename = input + '.json';
  const json = fs.readFileSync(filename);
  let rci: RawContractInterface = JSON.parse(json);
  const settings: BindingSettings = {
    language: Language.Archetype,
    target: Target.Experiment,
    path: '../tests/passed/'
  }
  const output = generate_binding(rci, settings);
  const out_ts = './bindings/passed/' + path.basename(filename.replace('.json', '.ts'));
  fs.writeFileSync(out_ts, output)
}

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


const generate_spec_error = (input: Array<string>, name: string, path : string, code: number) => {
  let items: Array<string> = []
  for (const id of input) {
    items.push(`
  it('${id}', async () => {
    const stat = compile("${path}/${id}.arl")
    assert(stat.status == ${code}, "Invalid status code, actual: " + stat.status + ", expected: ${code}")
  })`);
  }
  const output = `/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, [p], { });
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
  describe('Passed', async () => {
    const p = './json/passed'
    const filenames = extract_file_dir(p, '.json')
    for (const filename of filenames) {
      it(filename, () => {
        write_binding(p + '/' + filename)
      });
    }
    // generate_spec_template(filenames)
  })

  describe('Generate spec.ts files', async () => {
    const items: Array<[string, string, number]> = [
      ['syntax-errors', '../tests/syntax-errors', 1],
      ['type-errors', '../tests/type-errors', 3],
      ['model-errors', '../tests/model-errors', 5],
      ['proposal-type-errors', '../tests/proposal-type-errors', 3],
      ['proposal-model-errors', '../tests/proposal-model-errors', 5]
    ]
    for (const item of items) {
      const name = item[0]
      const path = item[1]
      const code = item[2]

      it(name, () => {
        const filenames = extract_file_dir(path, ".arl")
        generate_spec_error(filenames, name, path, code)
      })
    }
  })
})
