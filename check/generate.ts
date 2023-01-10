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

const skip = [
  'asset_iterable_big_map_effect_add.json',
  'asset_iterable_big_map_effect_addupdate.json',
  'asset_iterable_big_map_effect_remove.json',
  'asset_iterable_big_map_effect_removeall.json',
  'asset_iterable_big_map_effect_removeif.json',
  'asset_iterable_big_map_effect_update.json',
  'asset_iterable_big_map_expression_contains.json',
  'asset_iterable_big_map_expression_count.json',
  'asset_iterable_big_map_expression_get.json',
  'asset_iterable_big_map_expression_head.json',
  'asset_iterable_big_map_expression_nth.json',
  'asset_iterable_big_map_expression_select.json',
  'asset_iterable_big_map_expression_sort.json',
  'asset_iterable_big_map_expression_sum.json',
  'asset_iterable_big_map_expression_tail.json',
  'asset_iterable_big_map_instruction_for.json',
  'asset_iterable_big_map_multi_effect_add.json',
  'asset_iterable_big_map_multi_effect_addupdate.json',
  'asset_iterable_big_map_multi_effect_remove.json',
  'asset_iterable_big_map_multi_effect_removeall.json',
  'asset_iterable_big_map_multi_effect_removeif.json',
  'asset_iterable_big_map_multi_effect_update.json',
  'asset_iterable_big_map_multi_expression_contains.json',
  'asset_iterable_big_map_multi_expression_count.json',
  'asset_iterable_big_map_multi_expression_get.json',
  'asset_iterable_big_map_multi_expression_head.json',
  'asset_iterable_big_map_multi_expression_nth.json',
  'asset_iterable_big_map_multi_expression_select.json',
  'asset_iterable_big_map_multi_expression_sort.json',
  'asset_iterable_big_map_multi_expression_sum.json',
  'asset_iterable_big_map_multi_expression_tail.json',
  'asset_iterable_big_map_multi_instruction_for.json',
  'asset_iterable_big_map_multi_storage.json',
  'asset_iterable_big_map_storage.json',
  'asset_iterable_big_map_unit_effect_add.json',
  'asset_iterable_big_map_unit_effect_addupdate.json',
  'asset_iterable_big_map_unit_effect_remove.json',
  'asset_iterable_big_map_unit_effect_removeall.json',
  'asset_iterable_big_map_unit_effect_removeif.json',
  'asset_iterable_big_map_unit_effect_update.json',
  'asset_iterable_big_map_unit_expression_contains.json',
  'asset_iterable_big_map_unit_expression_count.json',
  'asset_iterable_big_map_unit_expression_head.json',
  'asset_iterable_big_map_unit_expression_nth.json',
  'asset_iterable_big_map_unit_expression_select.json',
  'asset_iterable_big_map_unit_expression_sort.json',
  'asset_iterable_big_map_unit_expression_sum.json',
  'asset_iterable_big_map_unit_expression_tail.json',
  'asset_iterable_big_map_unit_instruction_for.json',
  'asset_iterable_big_map_unit_storage.json',
  'asset_iterable_big_map_unit.json',
  'asset_iterable_big_map.json',
  'asset_simple_to_iterable_big_map.json',
  'effect_fail_complex.json',
  'fail_with_tuple_lit.json',
  'lang_enum.json',
  'rf_failif_with.json',
  'spec_fails.json',
  'test_asset.json',
  'type_never.json',
  'unused_variable_opt.json',
  'verif_fail.json',
]

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
