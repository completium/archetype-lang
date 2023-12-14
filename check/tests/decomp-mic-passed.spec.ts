/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, ['-d', '-mi', p], { });
  return res
}

/* Tests ------------------------------------------------------------------- */

describe('decomp-mic-passed', async () => {
  it('add_update_record', async () => {
    const stat = compile("michelson/passed/add_update_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_partition', async () => {
    const stat = compile("michelson/passed/addupdate_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_partition2', async () => {
    const stat = compile("michelson/passed/addupdate_partition2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_partition_with_no_effect_on_default_value', async () => {
    const stat = compile("michelson/passed/addupdate_partition_with_no_effect_on_default_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_with_no_effect_on_default_value', async () => {
    const stat = compile("michelson/passed/addupdate_with_no_effect_on_default_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('annot_enum', async () => {
    const stat = compile("michelson/passed/annot_enum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('apply_lambda', async () => {
    const stat = compile("michelson/passed/apply_lambda.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('arg_fun_constant', async () => {
    const stat = compile("michelson/passed/arg_fun_constant.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('arith_bls', async () => {
    const stat = compile("michelson/passed/arith_bls.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('arith_tez', async () => {
    const stat = compile("michelson/passed/arith_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ascii_string', async () => {
    const stat = compile("michelson/passed/ascii_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access', async () => {
    const stat = compile("michelson/passed/asset_access.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_basic', async () => {
    const stat = compile("michelson/passed/asset_access_basic.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_option_found', async () => {
    const stat = compile("michelson/passed/asset_access_option_found.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_option_not_found', async () => {
    const stat = compile("michelson/passed/asset_access_option_not_found.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_value', async () => {
    const stat = compile("michelson/passed/asset_access_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_addupdate', async () => {
    const stat = compile("michelson/passed/asset_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map', async () => {
    const stat = compile("michelson/passed/asset_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_add', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_effect_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_addupdate', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_effect_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_remove', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_effect_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_removeall', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_effect_removeall.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_update', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_effect_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_expression_contains', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_expression_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_storage', async () => {
    const stat = compile("michelson/passed/asset_big_map_unit_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_for', async () => {
    const stat = compile("michelson/passed/asset_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_init_by_const_key', async () => {
    const stat = compile("michelson/passed/asset_init_by_const_key.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_init_by_const_key_parameter', async () => {
    const stat = compile("michelson/passed/asset_init_by_const_key_parameter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_initializedby_aggregate_empty', async () => {
    const stat = compile("michelson/passed/asset_initializedby_aggregate_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_initializedby_aggregate_filled', async () => {
    const stat = compile("michelson/passed/asset_initializedby_aggregate_filled.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_instructions', async () => {
    const stat = compile("michelson/passed/asset_instructions.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_add', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_effect_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_addupdate', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_effect_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_remove', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_effect_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_removeall', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_effect_removeall.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_removeif', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_effect_removeif.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_update', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_effect_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_contains', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_count', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_count.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_get', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_head', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_nth', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_select', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_select.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_sort', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_sort.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_sum', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_sum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_tail', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_expression_tail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_instruction_for', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_instruction_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_add', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_effect_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_addupdate', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_effect_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_remove', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_effect_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_removeall', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_effect_removeall.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_removeif', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_effect_removeif.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_update', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_effect_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_contains', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_count', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_count.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_get', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_head', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_nth', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_select', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_select.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_sort', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_sort.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_sum', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_sum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_tail', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_expression_tail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_instruction_for', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_instruction_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_storage', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_multi_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_storage', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_add', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_effect_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_addupdate', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_effect_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_remove', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_effect_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_removeall', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_effect_removeall.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_removeif', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_effect_removeif.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_update', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_effect_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_contains', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_count', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_count.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_head', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_nth', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_select', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_select.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_sort', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_sort.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_sum', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_sum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_tail', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_expression_tail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_instruction_for', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_instruction_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_storage', async () => {
    const stat = compile("michelson/passed/asset_iterable_big_map_unit_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_key_in_record', async () => {
    const stat = compile("michelson/passed/asset_key_in_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_key_tuple', async () => {
    const stat = compile("michelson/passed/asset_key_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_not_found', async () => {
    const stat = compile("michelson/passed/asset_not_found.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_nth', async () => {
    const stat = compile("michelson/passed/asset_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_put_single', async () => {
    const stat = compile("michelson/passed/asset_put_single.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_simple', async () => {
    const stat = compile("michelson/passed/asset_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_simple_to_big_map', async () => {
    const stat = compile("michelson/passed/asset_simple_to_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_simple_to_iterable_big_map', async () => {
    const stat = compile("michelson/passed/asset_simple_to_iterable_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_tern_opt', async () => {
    const stat = compile("michelson/passed/asset_tern_opt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_ternary_expr_found', async () => {
    const stat = compile("michelson/passed/asset_ternary_expr_found.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_ternary_expr_notfound', async () => {
    const stat = compile("michelson/passed/asset_ternary_expr_notfound.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_types_get', async () => {
    const stat = compile("michelson/passed/asset_types_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_lit_add', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_map_lit_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_lit_remove', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_map_lit_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_list_add', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_map_var_list_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_list_remove', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_map_var_list_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_set_add', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_map_var_set_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_set_remove', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_map_var_set_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_lit_add', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_set_lit_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_lit_remove', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_set_lit_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_list_add', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_set_var_list_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_list_remove', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_set_var_list_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_set_add', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_set_var_set_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_set_remove', async () => {
    const stat = compile("michelson/passed/asset_update_with_basic_container_set_var_set_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_add_record', async () => {
    const stat = compile("michelson/passed/assign_add_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_add_tuple', async () => {
    const stat = compile("michelson/passed/assign_add_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_field', async () => {
    const stat = compile("michelson/passed/assign_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_minus_nat', async () => {
    const stat = compile("michelson/passed/assign_minus_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_opt', async () => {
    const stat = compile("michelson/passed/assign_opt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_var_rat_int', async () => {
    const stat = compile("michelson/passed/assign_var_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_vardecl_rat_int', async () => {
    const stat = compile("michelson/passed/assign_vardecl_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_vardecl_rat_nat', async () => {
    const stat = compile("michelson/passed/assign_vardecl_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('before_asset_api', async () => {
    const stat = compile("michelson/passed/before_asset_api.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('before_var', async () => {
    const stat = compile("michelson/passed/before_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('bls_lit', async () => {
    const stat = compile("michelson/passed/bls_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('builtin_in_function', async () => {
    const stat = compile("michelson/passed/builtin_in_function.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('called_by_an_asset', async () => {
    const stat = compile("michelson/passed/called_by_an_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast', async () => {
    const stat = compile("michelson/passed/cast.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_dur_int', async () => {
    const stat = compile("michelson/passed/cast_dur_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_nat_int', async () => {
    const stat = compile("michelson/passed/cast_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_nat_int_lit', async () => {
    const stat = compile("michelson/passed/cast_nat_int_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_return', async () => {
    const stat = compile("michelson/passed/cast_return.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_view_pklist', async () => {
    const stat = compile("michelson/passed/cast_view_pklist.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('col_iter_direct_storage', async () => {
    const stat = compile("michelson/passed/col_iter_direct_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('col_iter_filter_storage', async () => {
    const stat = compile("michelson/passed/col_iter_filter_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('compare_enum', async () => {
    const stat = compile("michelson/passed/compare_enum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('const_decl', async () => {
    const stat = compile("michelson/passed/const_decl.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('containers_of_tuple', async () => {
    const stat = compile("michelson/passed/containers_of_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_called', async () => {
    const stat = compile("michelson/passed/contract_called.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_caller', async () => {
    const stat = compile("michelson/passed/contract_caller.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_empty', async () => {
    const stat = compile("michelson/passed/contract_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_to_address', async () => {
    const stat = compile("michelson/passed/contract_to_address.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_transition', async () => {
    const stat = compile("michelson/passed/contract_transition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_transition_on_asset', async () => {
    const stat = compile("michelson/passed/contract_transition_on_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('counter', async () => {
    const stat = compile("michelson/passed/counter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('counter_proxy', async () => {
    const stat = compile("michelson/passed/counter_proxy.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_args_with_record', async () => {
    const stat = compile("michelson/passed/custom_args_with_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage', async () => {
    const stat = compile("michelson/passed/custom_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage10', async () => {
    const stat = compile("michelson/passed/custom_storage10.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage2', async () => {
    const stat = compile("michelson/passed/custom_storage2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage3', async () => {
    const stat = compile("michelson/passed/custom_storage3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage4', async () => {
    const stat = compile("michelson/passed/custom_storage4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage5', async () => {
    const stat = compile("michelson/passed/custom_storage5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage6', async () => {
    const stat = compile("michelson/passed/custom_storage6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage7', async () => {
    const stat = compile("michelson/passed/custom_storage7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage8', async () => {
    const stat = compile("michelson/passed/custom_storage8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage9', async () => {
    const stat = compile("michelson/passed/custom_storage9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('dangling_else', async () => {
    const stat = compile("michelson/passed/dangling_else.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('debug_assign', async () => {
    const stat = compile("michelson/passed/debug_assign.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('dec_lit', async () => {
    const stat = compile("michelson/passed/dec_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decl_var_opt', async () => {
    const stat = compile("michelson/passed/decl_var_opt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decl_var_opt_default', async () => {
    const stat = compile("michelson/passed/decl_var_opt_default.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if', async () => {
    const stat = compile("michelson/passed/decomp_if.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if2', async () => {
    const stat = compile("michelson/passed/decomp_if2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if3', async () => {
    const stat = compile("michelson/passed/decomp_if3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if4', async () => {
    const stat = compile("michelson/passed/decomp_if4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_ifexpr', async () => {
    const stat = compile("michelson/passed/decomp_ifexpr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_map', async () => {
    const stat = compile("michelson/passed/decomp_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_test', async () => {
    const stat = compile("michelson/passed/decomp_test.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_test2', async () => {
    const stat = compile("michelson/passed/decomp_test2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_while', async () => {
    const stat = compile("michelson/passed/decomp_while.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_while1', async () => {
    const stat = compile("michelson/passed/decomp_while1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_while2', async () => {
    const stat = compile("michelson/passed/decomp_while2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_big_map_string', async () => {
    const stat = compile("michelson/passed/detach_big_map_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_big_map_unit', async () => {
    const stat = compile("michelson/passed/detach_big_map_unit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_map_string', async () => {
    const stat = compile("michelson/passed/detach_map_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_option_string', async () => {
    const stat = compile("michelson/passed/detach_option_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('duration_to_int', async () => {
    const stat = compile("michelson/passed/duration_to_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_add_asset_with_complex_partition', async () => {
    const stat = compile("michelson/passed/effect_add_asset_with_complex_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_aggregate', async () => {
    const stat = compile("michelson/passed/effect_control_for_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_collection', async () => {
    const stat = compile("michelson/passed/effect_control_for_collection.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_collection_one_field', async () => {
    const stat = compile("michelson/passed/effect_control_for_collection_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_list', async () => {
    const stat = compile("michelson/passed/effect_control_for_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_map', async () => {
    const stat = compile("michelson/passed/effect_control_for_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_partition', async () => {
    const stat = compile("michelson/passed/effect_control_for_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_set', async () => {
    const stat = compile("michelson/passed/effect_control_for_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_view', async () => {
    const stat = compile("michelson/passed/effect_control_for_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_if', async () => {
    const stat = compile("michelson/passed/effect_control_if.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_if_else', async () => {
    const stat = compile("michelson/passed/effect_control_if_else.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_iter', async () => {
    const stat = compile("michelson/passed/effect_control_iter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_iter_init', async () => {
    const stat = compile("michelson/passed/effect_control_iter_init.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_enum', async () => {
    const stat = compile("michelson/passed/effect_control_match_enum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_list', async () => {
    const stat = compile("michelson/passed/effect_control_match_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_option', async () => {
    const stat = compile("michelson/passed/effect_control_match_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_or', async () => {
    const stat = compile("michelson/passed/effect_control_match_or.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_matchwith', async () => {
    const stat = compile("michelson/passed/effect_control_matchwith.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_sequence', async () => {
    const stat = compile("michelson/passed/effect_control_sequence.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_while', async () => {
    const stat = compile("michelson/passed/effect_control_while.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_dofailif', async () => {
    const stat = compile("michelson/passed/effect_dofailif.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_dorequire', async () => {
    const stat = compile("michelson/passed/effect_dorequire.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_dorequire_not', async () => {
    const stat = compile("michelson/passed/effect_dorequire_not.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_fail', async () => {
    const stat = compile("michelson/passed/effect_fail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_fail_complex', async () => {
    const stat = compile("michelson/passed/effect_fail_complex.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_instruction_put_in_asset', async () => {
    const stat = compile("michelson/passed/effect_instruction_put_in_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset2', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_asset2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_one_field', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_asset_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_with_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_asset_with_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_with_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_asset_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_with_partition_2', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_asset_with_partition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_partition_one_field', async () => {
    const stat = compile("michelson/passed/effect_method_asset_add_partition_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_add_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_map', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_add_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_map_var', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_add_map_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_add_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_set', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_add_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_remove_map', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_remove_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_remove_set', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_remove_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_replace_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_replace_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_replace_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_addupdate_with_replace_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_0_put_remove_put', async () => {
    const stat = compile("michelson/passed/effect_method_asset_big_map_0_put_remove_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_0_put_remove_remove', async () => {
    const stat = compile("michelson/passed/effect_method_asset_big_map_0_put_remove_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_1_put_remove_put', async () => {
    const stat = compile("michelson/passed/effect_method_asset_big_map_1_put_remove_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_1_put_remove_remove', async () => {
    const stat = compile("michelson/passed/effect_method_asset_big_map_1_put_remove_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_2_put_remove_put', async () => {
    const stat = compile("michelson/passed/effect_method_asset_big_map_2_put_remove_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_2_put_remove_remove', async () => {
    const stat = compile("michelson/passed/effect_method_asset_big_map_2_put_remove_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_clear_view', async () => {
    const stat = compile("michelson/passed/effect_method_asset_clear_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_clear_view_with_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_clear_view_with_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_clear_view_with_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_clear_view_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_0_put_remove_put', async () => {
    const stat = compile("michelson/passed/effect_method_asset_map_0_put_remove_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_0_put_remove_remove', async () => {
    const stat = compile("michelson/passed/effect_method_asset_map_0_put_remove_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_1_put_remove_put', async () => {
    const stat = compile("michelson/passed/effect_method_asset_map_1_put_remove_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_1_put_remove_remove', async () => {
    const stat = compile("michelson/passed/effect_method_asset_map_1_put_remove_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_2_put_remove_put', async () => {
    const stat = compile("michelson/passed/effect_method_asset_map_2_put_remove_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_2_put_remove_remove', async () => {
    const stat = compile("michelson/passed/effect_method_asset_map_2_put_remove_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_all_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_asset_one_field', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_all_asset_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_asset_with_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_all_asset_with_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_asset_with_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_all_asset_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_collection', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_all_collection.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset2', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_asset2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_one_field', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_asset_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_with_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_asset_with_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_with_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_asset_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_with_partition_2', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_asset_with_partition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_remove_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeall_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeall_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeall_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeall_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeif_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_collection', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeif_collection.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_collection_with_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeif_collection_with_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_collection_with_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeif_collection_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_removeif_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_coll_1', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_all_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_coll_2', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_all_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_view_1', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_all_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_view_2', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_all_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_add_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_map', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_add_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_add_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_set', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_add_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_map', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_remove_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_map', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_remove_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_remove_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_set', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_remove_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_replace_aggregate', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_replace_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_replace_partition', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_replace_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_set', async () => {
    const stat = compile("michelson/passed/effect_method_asset_update_with_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_transfer_contract', async () => {
    const stat = compile("michelson/passed/effect_transfer_contract.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_transfer_simple', async () => {
    const stat = compile("michelson/passed/effect_transfer_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_inspector', async () => {
    const stat = compile("michelson/passed/entry_inspector.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_called_by_otherwise', async () => {
    const stat = compile("michelson/passed/entry_section_called_by_otherwise.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_no_transfer_otherwise', async () => {
    const stat = compile("michelson/passed/entry_section_no_transfer_otherwise.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_sourced_by_otherwise', async () => {
    const stat = compile("michelson/passed/entry_section_sourced_by_otherwise.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_state_is_otherwise', async () => {
    const stat = compile("michelson/passed/entry_section_state_is_otherwise.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_token', async () => {
    const stat = compile("michelson/passed/entry_token.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_without_effect', async () => {
    const stat = compile("michelson/passed/entry_without_effect.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_all', async () => {
    const stat = compile("michelson/passed/enum_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_key', async () => {
    const stat = compile("michelson/passed/enum_key.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_simple', async () => {
    const stat = compile("michelson/passed/enum_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_with_args', async () => {
    const stat = compile("michelson/passed/enum_with_args.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_with_args_multi', async () => {
    const stat = compile("michelson/passed/enum_with_args_multi.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_without_args', async () => {
    const stat = compile("michelson/passed/enum_without_args.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_all', async () => {
    const stat = compile("michelson/passed/event_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_dup', async () => {
    const stat = compile("michelson/passed/event_dup.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_multi', async () => {
    const stat = compile("michelson/passed/event_multi.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_simple', async () => {
    const stat = compile("michelson/passed/event_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_single', async () => {
    const stat = compile("michelson/passed/event_single.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('exec_letin', async () => {
    const stat = compile("michelson/passed/exec_letin.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_access_asset_field', async () => {
    const stat = compile("michelson/passed/expr_access_asset_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_3wc_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_3wc_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_bool_bool', async () => {
    const stat = compile("michelson/passed/expr_arith_and_bool_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_bytes_bytes', async () => {
    const stat = compile("michelson/passed/expr_arith_and_bytes_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_and_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_and_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_div_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_div_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_div_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_int_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_div_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_div_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_div_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_div_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_rat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_div_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_div_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_div_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_div_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_divmod_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_divmod_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_divmod_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_divmod_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_tez_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_divmod_tez_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_divmod_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_dur_int', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_dur_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_dur_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_dur_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_tez_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_tez_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_ediv_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsl_bytes_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_lsl_bytes_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsl_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_lsl_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsr_bytes_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_lsr_bytes_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsr_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_lsr_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_date_date', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_date_date_neg', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_date_date_neg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_date_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_date_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_int_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_rat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_minus_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_mod_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_mod_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_mod_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_mod_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_mod_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_int_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_int_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_nat_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_nat_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_rat_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_rat_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_tez_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_mult_tez_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_bool', async () => {
    const stat = compile("michelson/passed/expr_arith_not_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_bytes', async () => {
    const stat = compile("michelson/passed/expr_arith_not_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_int', async () => {
    const stat = compile("michelson/passed/expr_arith_not_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_not_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_or_bool_bool', async () => {
    const stat = compile("michelson/passed/expr_arith_or_bool_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_or_bytes_bytes', async () => {
    const stat = compile("michelson/passed/expr_arith_or_bytes_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_or_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_or_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_date_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_date_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_dur_date', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_dur_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_int_int', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_int_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_int_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_nat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_rat_int', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_str_str', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_arith_plus_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_uminus_int', async () => {
    const stat = compile("michelson/passed/expr_arith_uminus_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_uminus_rat', async () => {
    const stat = compile("michelson/passed/expr_arith_uminus_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_xor_bool_bool', async () => {
    const stat = compile("michelson/passed/expr_arith_xor_bool_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_xor_bytes_bytes', async () => {
    const stat = compile("michelson/passed/expr_arith_xor_bytes_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_xor_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_arith_xor_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_addr_addr', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_addr_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_bool_bool', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_bool_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_date_date', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_int_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_int_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_int_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_nat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_rat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_str_str', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_cmp_eq_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_addr_addr', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_addr_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_date_date', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_int_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_int_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_int_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_nat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_rat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_str_str', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_cmp_ge_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_addr_addr', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_addr_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_date_date', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_int_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_int_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_int_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_nat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_rat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_str_str', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_cmp_gt_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_addr_addr', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_addr_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_date_date', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_int_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_int_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_int_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_nat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_rat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_str_str', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_cmp_le_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_addr_addr', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_addr_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_date_date', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_int_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_int_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_int_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_nat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_rat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_str_str', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_cmp_lt_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_addr_addr', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_addr_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_bool_bool', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_bool_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_date_date', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_date_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_dur_dur', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_dur_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_int_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_int_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_int_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_nat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_rat_int', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_str_str', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_str_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_tez_tez', async () => {
    const stat = compile("michelson/passed/expr_cmp_ne_tez_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_fold', async () => {
    const stat = compile("michelson/passed/expr_control_fold.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_int_int', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_int_nat', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_int_rat', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_nat_int', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_rat_int', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_control_if_else_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_match_list', async () => {
    const stat = compile("michelson/passed/expr_control_match_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_match_option', async () => {
    const stat = compile("michelson/passed/expr_control_match_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_match_or', async () => {
    const stat = compile("michelson/passed/expr_control_match_or.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith', async () => {
    const stat = compile("michelson/passed/expr_control_matchwith.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith_with_int_rat', async () => {
    const stat = compile("michelson/passed/expr_control_matchwith_with_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith_with_nat_int', async () => {
    const stat = compile("michelson/passed/expr_control_matchwith_with_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith_with_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_control_matchwith_with_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_balance', async () => {
    const stat = compile("michelson/passed/expr_cst_balance.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_caller', async () => {
    const stat = compile("michelson/passed/expr_cst_caller.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_level', async () => {
    const stat = compile("michelson/passed/expr_cst_level.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_min_block_time', async () => {
    const stat = compile("michelson/passed/expr_cst_min_block_time.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_now', async () => {
    const stat = compile("michelson/passed/expr_cst_now.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_self_address', async () => {
    const stat = compile("michelson/passed/expr_cst_self_address.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_self_chain_id', async () => {
    const stat = compile("michelson/passed/expr_cst_self_chain_id.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_source', async () => {
    const stat = compile("michelson/passed/expr_cst_source.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_total_voting_power', async () => {
    const stat = compile("michelson/passed/expr_cst_total_voting_power.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_transferred', async () => {
    const stat = compile("michelson/passed/expr_cst_transferred.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fail_some_none', async () => {
    const stat = compile("michelson/passed/expr_fail_some_none.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fail_some_some', async () => {
    const stat = compile("michelson/passed/expr_fail_some_some.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_contains', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_count', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_count.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_diff_view', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_diff_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_empty', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_get', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_head', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_inter_view', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_inter_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_isempty', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_isempty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_nth', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_select', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_select.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_singleton', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_singleton.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_subsetof_aggregate', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_subsetof_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_subsetof_collection', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_subsetof_collection.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_subsetof_partition', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_subsetof_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_subsetof_view', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_subsetof_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_sum', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_sum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_tail', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_tail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_asset_method_union_view', async () => {
    const stat = compile("michelson/passed/expr_formula_asset_method_union_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_at', async () => {
    const stat = compile("michelson/passed/expr_formula_at.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_before', async () => {
    const stat = compile("michelson/passed/expr_formula_before.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_eq_list', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_eq_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_eq_map', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_eq_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_eq_option', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_eq_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_eq_set', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_eq_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_ne_list', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_ne_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_ne_map', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_ne_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_ne_option', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_ne_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_cmp_ne_set', async () => {
    const stat = compile("michelson/passed/expr_formula_cmp_ne_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_equiv', async () => {
    const stat = compile("michelson/passed/expr_formula_equiv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_exists_asset', async () => {
    const stat = compile("michelson/passed/expr_formula_exists_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_exists_builtin', async () => {
    const stat = compile("michelson/passed/expr_formula_exists_builtin.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_forall_asset', async () => {
    const stat = compile("michelson/passed/expr_formula_forall_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_forall_builtin', async () => {
    const stat = compile("michelson/passed/expr_formula_forall_builtin.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_implication', async () => {
    const stat = compile("michelson/passed/expr_formula_implication.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_iterated_aggregate', async () => {
    const stat = compile("michelson/passed/expr_formula_iterated_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_iterated_collection', async () => {
    const stat = compile("michelson/passed/expr_formula_iterated_collection.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_iterated_partition', async () => {
    const stat = compile("michelson/passed/expr_formula_iterated_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_iterated_view', async () => {
    const stat = compile("michelson/passed/expr_formula_iterated_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_toiterate_aggregate', async () => {
    const stat = compile("michelson/passed/expr_formula_toiterate_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_toiterate_collection', async () => {
    const stat = compile("michelson/passed/expr_formula_toiterate_collection.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_toiterate_partition', async () => {
    const stat = compile("michelson/passed/expr_formula_toiterate_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_formula_toiterate_view', async () => {
    const stat = compile("michelson/passed/expr_formula_toiterate_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_abs_int', async () => {
    const stat = compile("michelson/passed/expr_fun_abs_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_abs_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_abs_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_abs_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_abs_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_address_to_contract', async () => {
    const stat = compile("michelson/passed/expr_fun_address_to_contract.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_address_to_contract_unit', async () => {
    const stat = compile("michelson/passed/expr_fun_address_to_contract_unit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_bytes_to_int', async () => {
    const stat = compile("michelson/passed/expr_fun_bytes_to_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_bytes_to_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_bytes_to_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_ceil', async () => {
    const stat = compile("michelson/passed/expr_fun_ceil.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_byt', async () => {
    const stat = compile("michelson/passed/expr_fun_concat_byt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_list_byt', async () => {
    const stat = compile("michelson/passed/expr_fun_concat_list_byt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_list_str', async () => {
    const stat = compile("michelson/passed/expr_fun_concat_list_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_str', async () => {
    const stat = compile("michelson/passed/expr_fun_concat_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_exp_horner', async () => {
    const stat = compile("michelson/passed/expr_fun_exp_horner.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_floor', async () => {
    const stat = compile("michelson/passed/expr_fun_floor.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_get_denominator', async () => {
    const stat = compile("michelson/passed/expr_fun_get_denominator.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_get_numerator', async () => {
    const stat = compile("michelson/passed/expr_fun_get_numerator.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_int_to_bytes', async () => {
    const stat = compile("michelson/passed/expr_fun_int_to_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_int_to_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_int_to_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_key_hash_to_contract', async () => {
    const stat = compile("michelson/passed/expr_fun_key_hash_to_contract.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_length_bytes', async () => {
    const stat = compile("michelson/passed/expr_fun_length_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_length_str', async () => {
    const stat = compile("michelson/passed/expr_fun_length_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_make_event', async () => {
    const stat = compile("michelson/passed/expr_fun_make_event.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_make_operation', async () => {
    const stat = compile("michelson/passed/expr_fun_make_operation.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_date', async () => {
    const stat = compile("michelson/passed/expr_fun_max_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_dur', async () => {
    const stat = compile("michelson/passed/expr_fun_max_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_int_int', async () => {
    const stat = compile("michelson/passed/expr_fun_max_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_int_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_max_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_int_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_max_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_nat_int', async () => {
    const stat = compile("michelson/passed/expr_fun_max_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_max_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_max_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_rat_int', async () => {
    const stat = compile("michelson/passed/expr_fun_max_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_max_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_max_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_tez', async () => {
    const stat = compile("michelson/passed/expr_fun_max_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_date', async () => {
    const stat = compile("michelson/passed/expr_fun_min_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_dur', async () => {
    const stat = compile("michelson/passed/expr_fun_min_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_int_int', async () => {
    const stat = compile("michelson/passed/expr_fun_min_int_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_int_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_min_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_int_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_min_int_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_nat_int', async () => {
    const stat = compile("michelson/passed/expr_fun_min_nat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_nat_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_min_nat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_nat_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_min_nat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_rat_int', async () => {
    const stat = compile("michelson/passed/expr_fun_min_rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_rat_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_min_rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_rat_rat', async () => {
    const stat = compile("michelson/passed/expr_fun_min_rat_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_tez', async () => {
    const stat = compile("michelson/passed/expr_fun_min_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_nat_to_bytes', async () => {
    const stat = compile("michelson/passed/expr_fun_nat_to_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_nat_to_string', async () => {
    const stat = compile("michelson/passed/expr_fun_nat_to_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_get_some', async () => {
    const stat = compile("michelson/passed/expr_fun_opt_get_some.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_is_none', async () => {
    const stat = compile("michelson/passed/expr_fun_opt_is_none.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_is_some', async () => {
    const stat = compile("michelson/passed/expr_fun_opt_is_some.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_require_some', async () => {
    const stat = compile("michelson/passed/expr_fun_opt_require_some.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_pack_complex', async () => {
    const stat = compile("michelson/passed/expr_fun_pack_complex.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_pack_lit_tuple', async () => {
    const stat = compile("michelson/passed/expr_fun_pack_lit_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_pack_string', async () => {
    const stat = compile("michelson/passed/expr_fun_pack_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_setdelegate', async () => {
    const stat = compile("michelson/passed/expr_fun_setdelegate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_simplify_rational', async () => {
    const stat = compile("michelson/passed/expr_fun_simplify_rational.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_slice_byt', async () => {
    const stat = compile("michelson/passed/expr_fun_slice_byt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_slice_str', async () => {
    const stat = compile("michelson/passed/expr_fun_slice_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_sub_mutez', async () => {
    const stat = compile("michelson/passed/expr_fun_sub_mutez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_sub_nat', async () => {
    const stat = compile("michelson/passed/expr_fun_sub_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_sub_nat_zero', async () => {
    const stat = compile("michelson/passed/expr_fun_sub_nat_zero.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_unpack_bool', async () => {
    const stat = compile("michelson/passed/expr_fun_unpack_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_unpack_complex', async () => {
    const stat = compile("michelson/passed/expr_fun_unpack_complex.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_unpack_string', async () => {
    const stat = compile("michelson/passed/expr_fun_unpack_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_1_0', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_1_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_2_0', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_2_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_2_1', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_2_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_3_0', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_3_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_3_1', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_3_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_3_2', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_3_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_0', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_4_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_1', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_4_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_2', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_4_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_3', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_4_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_rollback', async () => {
    const stat = compile("michelson/passed/expr_instr_rec_rollback.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lambda', async () => {
    const stat = compile("michelson/passed/expr_lambda.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lambda2', async () => {
    const stat = compile("michelson/passed/expr_lambda2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_concat', async () => {
    const stat = compile("michelson/passed/expr_list_concat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_contains', async () => {
    const stat = compile("michelson/passed/expr_list_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_head', async () => {
    const stat = compile("michelson/passed/expr_list_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_length', async () => {
    const stat = compile("michelson/passed/expr_list_length.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_lit', async () => {
    const stat = compile("michelson/passed/expr_list_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_map', async () => {
    const stat = compile("michelson/passed/expr_list_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_map_string_nat', async () => {
    const stat = compile("michelson/passed/expr_list_map_string_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_nth', async () => {
    const stat = compile("michelson/passed/expr_list_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_prepend', async () => {
    const stat = compile("michelson/passed/expr_list_prepend.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_reverse', async () => {
    const stat = compile("michelson/passed/expr_list_reverse.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_tail', async () => {
    const stat = compile("michelson/passed/expr_list_tail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_addr', async () => {
    const stat = compile("michelson/passed/expr_lit_addr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_bytes', async () => {
    const stat = compile("michelson/passed/expr_lit_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_cur_mtz', async () => {
    const stat = compile("michelson/passed/expr_lit_cur_mtz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_cur_tz', async () => {
    const stat = compile("michelson/passed/expr_lit_cur_tz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_cur_utz', async () => {
    const stat = compile("michelson/passed/expr_lit_cur_utz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_0', async () => {
    const stat = compile("michelson/passed/expr_lit_date_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_1', async () => {
    const stat = compile("michelson/passed/expr_lit_date_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_2', async () => {
    const stat = compile("michelson/passed/expr_lit_date_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_3', async () => {
    const stat = compile("michelson/passed/expr_lit_date_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_4', async () => {
    const stat = compile("michelson/passed/expr_lit_date_4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_dur', async () => {
    const stat = compile("michelson/passed/expr_lit_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_int', async () => {
    const stat = compile("michelson/passed/expr_lit_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_int_neg', async () => {
    const stat = compile("michelson/passed/expr_lit_int_neg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_nat', async () => {
    const stat = compile("michelson/passed/expr_lit_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_opt_none', async () => {
    const stat = compile("michelson/passed/expr_lit_opt_none.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_opt_some', async () => {
    const stat = compile("michelson/passed/expr_lit_opt_some.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_or_left', async () => {
    const stat = compile("michelson/passed/expr_lit_or_left.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_or_right', async () => {
    const stat = compile("michelson/passed/expr_lit_or_right.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_dec', async () => {
    const stat = compile("michelson/passed/expr_lit_rat_dec.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_dec_neg', async () => {
    const stat = compile("michelson/passed/expr_lit_rat_dec_neg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_div', async () => {
    const stat = compile("michelson/passed/expr_lit_rat_div.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_div_neg', async () => {
    const stat = compile("michelson/passed/expr_lit_rat_div_neg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_str', async () => {
    const stat = compile("michelson/passed/expr_lit_str.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_tuple', async () => {
    const stat = compile("michelson/passed/expr_lit_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_unit', async () => {
    const stat = compile("michelson/passed/expr_lit_unit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_big_map', async () => {
    const stat = compile("michelson/passed/expr_make_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_big_map_empty', async () => {
    const stat = compile("michelson/passed/expr_make_big_map_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_list', async () => {
    const stat = compile("michelson/passed/expr_make_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_list_empty', async () => {
    const stat = compile("michelson/passed/expr_make_list_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_map', async () => {
    const stat = compile("michelson/passed/expr_make_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_map_empty', async () => {
    const stat = compile("michelson/passed/expr_make_map_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_set', async () => {
    const stat = compile("michelson/passed/expr_make_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_set_empty', async () => {
    const stat = compile("michelson/passed/expr_make_set_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_contains', async () => {
    const stat = compile("michelson/passed/expr_map_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_get', async () => {
    const stat = compile("michelson/passed/expr_map_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_length', async () => {
    const stat = compile("michelson/passed/expr_map_length.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_lit', async () => {
    const stat = compile("michelson/passed/expr_map_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_map', async () => {
    const stat = compile("michelson/passed/expr_map_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_put', async () => {
    const stat = compile("michelson/passed/expr_map_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_remove', async () => {
    const stat = compile("michelson/passed/expr_map_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_update', async () => {
    const stat = compile("michelson/passed/expr_map_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains', async () => {
    const stat = compile("michelson/passed/expr_method_asset_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_contains_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_contains_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_contains_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_contains_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count', async () => {
    const stat = compile("michelson/passed/expr_method_asset_count.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_count_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_count_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_count_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_count_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_get', async () => {
    const stat = compile("michelson/passed/expr_method_asset_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head', async () => {
    const stat = compile("michelson/passed/expr_method_asset_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_head_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_head_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_head_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_head_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth', async () => {
    const stat = compile("michelson/passed/expr_method_asset_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_nth_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_nth_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_nth_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_nth_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select', async () => {
    const stat = compile("michelson/passed/expr_method_asset_select.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_select_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_select_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_select_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_select_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sort.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sort_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sort_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sort_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sort_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sum_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sum_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sum_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_rational', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sum_rational.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_sum_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail', async () => {
    const stat = compile("michelson/passed/expr_method_asset_tail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_aggregate', async () => {
    const stat = compile("michelson/passed/expr_method_asset_tail_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_one_field', async () => {
    const stat = compile("michelson/passed/expr_method_asset_tail_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_partition', async () => {
    const stat = compile("michelson/passed/expr_method_asset_tail_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_view', async () => {
    const stat = compile("michelson/passed/expr_method_asset_tail_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_multicmp', async () => {
    const stat = compile("michelson/passed/expr_multicmp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_option_map', async () => {
    const stat = compile("michelson/passed/expr_option_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_record_lit', async () => {
    const stat = compile("michelson/passed/expr_record_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_record_update_asset_in_formula', async () => {
    const stat = compile("michelson/passed/expr_record_update_asset_in_formula.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_record_update_record_in_exec', async () => {
    const stat = compile("michelson/passed/expr_record_update_record_in_exec.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_record_update_record_in_formula', async () => {
    const stat = compile("michelson/passed/expr_record_update_record_in_formula.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_add', async () => {
    const stat = compile("michelson/passed/expr_set_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_contains', async () => {
    const stat = compile("michelson/passed/expr_set_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_length', async () => {
    const stat = compile("michelson/passed/expr_set_length.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_lit', async () => {
    const stat = compile("michelson/passed/expr_set_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_remove', async () => {
    const stat = compile("michelson/passed/expr_set_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_update', async () => {
    const stat = compile("michelson/passed/expr_set_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_tuple_access', async () => {
    const stat = compile("michelson/passed/expr_tuple_access.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_tuple_access_simple', async () => {
    const stat = compile("michelson/passed/expr_tuple_access_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_list_empty', async () => {
    const stat = compile("michelson/passed/expr_var_match_list_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_list_head', async () => {
    const stat = compile("michelson/passed/expr_var_match_list_head.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_option_none', async () => {
    const stat = compile("michelson/passed/expr_var_match_option_none.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_option_some', async () => {
    const stat = compile("michelson/passed/expr_var_match_option_some.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_or_left', async () => {
    const stat = compile("michelson/passed/expr_var_match_or_left.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_or_right', async () => {
    const stat = compile("michelson/passed/expr_var_match_or_right.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fa12_false', async () => {
    const stat = compile("michelson/passed/fa12_false.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fa12_simple', async () => {
    const stat = compile("michelson/passed/fa12_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_', async () => {
    const stat = compile("michelson/passed/fail_.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_for', async () => {
    const stat = compile("michelson/passed/fail_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_if', async () => {
    const stat = compile("michelson/passed/fail_if.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_match_list', async () => {
    const stat = compile("michelson/passed/fail_match_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_match_option', async () => {
    const stat = compile("michelson/passed/fail_match_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_while', async () => {
    const stat = compile("michelson/passed/fail_while.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_with_tuple_lit', async () => {
    const stat = compile("michelson/passed/fail_with_tuple_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fold_reverse', async () => {
    const stat = compile("michelson/passed/fold_reverse.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('formula_added_asset', async () => {
    const stat = compile("michelson/passed/formula_added_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun', async () => {
    const stat = compile("michelson/passed/fun.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_pure', async () => {
    const stat = compile("michelson/passed/fun_entry_pure.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_read', async () => {
    const stat = compile("michelson/passed/fun_entry_read.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_write', async () => {
    const stat = compile("michelson/passed/fun_entry_write.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_write_with_transfer', async () => {
    const stat = compile("michelson/passed/fun_entry_write_with_transfer.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_instr_unit', async () => {
    const stat = compile("michelson/passed/fun_instr_unit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_instr_unit_arg', async () => {
    const stat = compile("michelson/passed/fun_instr_unit_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_instr_unit_arg_side_effect', async () => {
    const stat = compile("michelson/passed/fun_instr_unit_arg_side_effect.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_unit', async () => {
    const stat = compile("michelson/passed/fun_unit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_instr_pure', async () => {
    const stat = compile("michelson/passed/fun_view_instr_pure.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_pure', async () => {
    const stat = compile("michelson/passed/fun_view_pure.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_read', async () => {
    const stat = compile("michelson/passed/fun_view_read.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_read_asset', async () => {
    const stat = compile("michelson/passed/fun_view_read_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('function_with_nat_to_string', async () => {
    const stat = compile("michelson/passed/function_with_nat_to_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('function_with_simplify_rational', async () => {
    const stat = compile("michelson/passed/function_with_simplify_rational.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('get_in_require_failif', async () => {
    const stat = compile("michelson/passed/get_in_require_failif.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('get_some_with_msg', async () => {
    const stat = compile("michelson/passed/get_some_with_msg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('getter_called_by', async () => {
    const stat = compile("michelson/passed/getter_called_by.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('greedy_and', async () => {
    const stat = compile("michelson/passed/greedy_and.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('greedy_or', async () => {
    const stat = compile("michelson/passed/greedy_or.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('implicit_cast_to_view', async () => {
    const stat = compile("michelson/passed/implicit_cast_to_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('implicit_the', async () => {
    const stat = compile("michelson/passed/implicit_the.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_all_def', async () => {
    const stat = compile("michelson/passed/import_arl_all_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_container_use_arg', async () => {
    const stat = compile("michelson/passed/import_arl_asset_container_use_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_container_use_arg_collide', async () => {
    const stat = compile("michelson/passed/import_arl_asset_container_use_arg_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_def', async () => {
    const stat = compile("michelson/passed/import_arl_asset_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_key_use_arg', async () => {
    const stat = compile("michelson/passed/import_arl_asset_key_use_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_key_use_arg_collide', async () => {
    const stat = compile("michelson/passed/import_arl_asset_key_use_arg_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_use_all', async () => {
    const stat = compile("michelson/passed/import_arl_asset_use_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_value_use_arg', async () => {
    const stat = compile("michelson/passed/import_arl_asset_value_use_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_value_use_arg_collide', async () => {
    const stat = compile("michelson/passed/import_arl_asset_value_use_arg_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_view_use_arg', async () => {
    const stat = compile("michelson/passed/import_arl_asset_view_use_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_view_use_arg_collide', async () => {
    const stat = compile("michelson/passed/import_arl_asset_view_use_arg_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_def', async () => {
    const stat = compile("michelson/passed/import_arl_constant_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_use', async () => {
    const stat = compile("michelson/passed/import_arl_constant_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_use_all', async () => {
    const stat = compile("michelson/passed/import_arl_constant_use_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_constant_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entry_record_use_all', async () => {
    const stat = compile("michelson/passed/import_arl_entry_record_use_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entrypoint_def', async () => {
    const stat = compile("michelson/passed/import_arl_entrypoint_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entrypoint_use', async () => {
    const stat = compile("michelson/passed/import_arl_entrypoint_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entrypoint_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_entrypoint_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_def', async () => {
    const stat = compile("michelson/passed/import_arl_enum_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use', async () => {
    const stat = compile("michelson/passed/import_arl_enum_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use_all', async () => {
    const stat = compile("michelson/passed/import_arl_enum_use_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_enum_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use_complete', async () => {
    const stat = compile("michelson/passed/import_arl_enum_use_complete.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_def', async () => {
    const stat = compile("michelson/passed/import_arl_enum_with_args_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_use', async () => {
    const stat = compile("michelson/passed/import_arl_enum_with_args_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_enum_with_args_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_use_complete', async () => {
    const stat = compile("michelson/passed/import_arl_enum_with_args_use_complete.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_event_def', async () => {
    const stat = compile("michelson/passed/import_arl_event_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_event_use', async () => {
    const stat = compile("michelson/passed/import_arl_event_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_event_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_event_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_2_pure_use', async () => {
    const stat = compile("michelson/passed/import_arl_fun_2_pure_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_def', async () => {
    const stat = compile("michelson/passed/import_arl_fun_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_def_pure', async () => {
    const stat = compile("michelson/passed/import_arl_fun_def_pure.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_instr_pure_use', async () => {
    const stat = compile("michelson/passed/import_arl_fun_instr_pure_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_pure_use', async () => {
    const stat = compile("michelson/passed/import_arl_fun_pure_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_complex_0', async () => {
    const stat = compile("michelson/passed/import_arl_record_complex_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_complex_1', async () => {
    const stat = compile("michelson/passed/import_arl_record_complex_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_complex_2', async () => {
    const stat = compile("michelson/passed/import_arl_record_complex_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_def', async () => {
    const stat = compile("michelson/passed/import_arl_record_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use', async () => {
    const stat = compile("michelson/passed/import_arl_record_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use_all', async () => {
    const stat = compile("michelson/passed/import_arl_record_use_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_record_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use_complete', async () => {
    const stat = compile("michelson/passed/import_arl_record_use_complete.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_transfer_use', async () => {
    const stat = compile("michelson/passed/import_arl_transfer_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_def', async () => {
    const stat = compile("michelson/passed/import_arl_view_def.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_use', async () => {
    const stat = compile("michelson/passed/import_arl_view_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_use_all', async () => {
    const stat = compile("michelson/passed/import_arl_view_use_all.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_use_collide', async () => {
    const stat = compile("michelson/passed/import_arl_view_use_collide.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_tz_entry_use', async () => {
    const stat = compile("michelson/passed/import_tz_entry_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_tz_view_use', async () => {
    const stat = compile("michelson/passed/import_tz_view_use.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('init_lambda', async () => {
    const stat = compile("michelson/passed/init_lambda.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('inline_michelson', async () => {
    const stat = compile("michelson/passed/inline_michelson.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_list_prepend', async () => {
    const stat = compile("michelson/passed/instr_list_prepend.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_put', async () => {
    const stat = compile("michelson/passed/instr_map_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_remove', async () => {
    const stat = compile("michelson/passed/instr_map_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_local_record', async () => {
    const stat = compile("michelson/passed/instr_map_update_local_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_local_var', async () => {
    const stat = compile("michelson/passed/instr_map_update_local_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_storage_record', async () => {
    const stat = compile("michelson/passed/instr_map_update_storage_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_storage_var', async () => {
    const stat = compile("michelson/passed/instr_map_update_storage_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_add', async () => {
    const stat = compile("michelson/passed/instr_set_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_remove', async () => {
    const stat = compile("michelson/passed/instr_set_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_update_add', async () => {
    const stat = compile("michelson/passed/instr_set_update_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_update_remove', async () => {
    const stat = compile("michelson/passed/instr_set_update_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('int_to_date', async () => {
    const stat = compile("michelson/passed/int_to_date.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('invariants_on_states', async () => {
    const stat = compile("michelson/passed/invariants_on_states.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('invariants_on_variable', async () => {
    const stat = compile("michelson/passed/invariants_on_variable.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iter_list_ticket', async () => {
    const stat = compile("michelson/passed/iter_list_ticket.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_assign', async () => {
    const stat = compile("michelson/passed/iterable_big_map_assign.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_contains', async () => {
    const stat = compile("michelson/passed/iterable_big_map_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_for', async () => {
    const stat = compile("michelson/passed/iterable_big_map_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_get', async () => {
    const stat = compile("michelson/passed/iterable_big_map_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_length', async () => {
    const stat = compile("michelson/passed/iterable_big_map_length.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_put', async () => {
    const stat = compile("michelson/passed/iterable_big_map_put.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_remove', async () => {
    const stat = compile("michelson/passed/iterable_big_map_remove.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_storage_decl', async () => {
    const stat = compile("michelson/passed/iterable_big_map_storage_decl.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_test', async () => {
    const stat = compile("michelson/passed/iterable_big_map_test.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('key_to_address', async () => {
    const stat = compile("michelson/passed/key_to_address.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_arith', async () => {
    const stat = compile("michelson/passed/lang_arith.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_asset', async () => {
    const stat = compile("michelson/passed/lang_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_assign', async () => {
    const stat = compile("michelson/passed/lang_assign.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_big_map', async () => {
    const stat = compile("michelson/passed/lang_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_cast', async () => {
    const stat = compile("michelson/passed/lang_cast.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_cmp', async () => {
    const stat = compile("michelson/passed/lang_cmp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_contract', async () => {
    const stat = compile("michelson/passed/lang_contract.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_crypto', async () => {
    const stat = compile("michelson/passed/lang_crypto.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_cst', async () => {
    const stat = compile("michelson/passed/lang_cst.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_entry', async () => {
    const stat = compile("michelson/passed/lang_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_enum', async () => {
    const stat = compile("michelson/passed/lang_enum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_formula_asset_api', async () => {
    const stat = compile("michelson/passed/lang_formula_asset_api.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_funs', async () => {
    const stat = compile("michelson/passed/lang_funs.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_list', async () => {
    const stat = compile("michelson/passed/lang_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_literals', async () => {
    const stat = compile("michelson/passed/lang_literals.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_map', async () => {
    const stat = compile("michelson/passed/lang_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_methods_asset', async () => {
    const stat = compile("michelson/passed/lang_methods_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_security', async () => {
    const stat = compile("michelson/passed/lang_security.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_set', async () => {
    const stat = compile("michelson/passed/lang_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('large_if', async () => {
    const stat = compile("michelson/passed/large_if.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_list', async () => {
    const stat = compile("michelson/passed/list_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_nth_out_of_bound', async () => {
    const stat = compile("michelson/passed/list_nth_out_of_bound.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_option', async () => {
    const stat = compile("michelson/passed/list_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_or', async () => {
    const stat = compile("michelson/passed/list_or.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_list', async () => {
    const stat = compile("michelson/passed/lit_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_map', async () => {
    const stat = compile("michelson/passed/lit_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_set', async () => {
    const stat = compile("michelson/passed/lit_set.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_tez_underscore', async () => {
    const stat = compile("michelson/passed/lit_tez_underscore.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('literal_in_argument', async () => {
    const stat = compile("michelson/passed/literal_in_argument.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('map_asset', async () => {
    const stat = compile("michelson/passed/map_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_detach_big_map', async () => {
    const stat = compile("michelson/passed/match_detach_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_detach_map', async () => {
    const stat = compile("michelson/passed/match_detach_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_detach_option', async () => {
    const stat = compile("michelson/passed/match_detach_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_entrypoint', async () => {
    const stat = compile("michelson/passed/match_entrypoint.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('max_tez', async () => {
    const stat = compile("michelson/passed/max_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('method_in_dorequire_or_dofailif', async () => {
    const stat = compile("michelson/passed/method_in_dorequire_or_dofailif.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('michelson_expression', async () => {
    const stat = compile("michelson/passed/michelson_expression.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('michelson_instruction', async () => {
    const stat = compile("michelson/passed/michelson_instruction.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('michelson_lambda', async () => {
    const stat = compile("michelson/passed/michelson_lambda.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('miles_with_expiration_spec', async () => {
    const stat = compile("michelson/passed/miles_with_expiration_spec.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('mod_rat', async () => {
    const stat = compile("michelson/passed/mod_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_e', async () => {
    const stat = compile("michelson/passed/multi_e.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_p', async () => {
    const stat = compile("michelson/passed/multi_p.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_sort', async () => {
    const stat = compile("michelson/passed/multi_sort.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_update', async () => {
    const stat = compile("michelson/passed/multi_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_var_storage', async () => {
    const stat = compile("michelson/passed/multi_var_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multivars', async () => {
    const stat = compile("michelson/passed/multivars.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multivars1', async () => {
    const stat = compile("michelson/passed/multivars1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multivars_simple', async () => {
    const stat = compile("michelson/passed/multivars_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('mutez_to_nat', async () => {
    const stat = compile("michelson/passed/mutez_to_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nat_to_string', async () => {
    const stat = compile("michelson/passed/nat_to_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nat_to_string_2', async () => {
    const stat = compile("michelson/passed/nat_to_string_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nested_for', async () => {
    const stat = compile("michelson/passed/nested_for.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nested_if_return', async () => {
    const stat = compile("michelson/passed/nested_if_return.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('no_entrypoint', async () => {
    const stat = compile("michelson/passed/no_entrypoint.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('not_int', async () => {
    const stat = compile("michelson/passed/not_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('not_nat', async () => {
    const stat = compile("michelson/passed/not_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nothing', async () => {
    const stat = compile("michelson/passed/nothing.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('one_constant', async () => {
    const stat = compile("michelson/passed/one_constant.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('op_assign_rat_update_asset', async () => {
    const stat = compile("michelson/passed/op_assign_rat_update_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('param_const', async () => {
    const stat = compile("michelson/passed/param_const.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('parameter_expr_map', async () => {
    const stat = compile("michelson/passed/parameter_expr_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('partial_record', async () => {
    const stat = compile("michelson/passed/partial_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_div', async () => {
    const stat = compile("michelson/passed/rat_arith_div.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_minus', async () => {
    const stat = compile("michelson/passed/rat_arith_minus.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_mult', async () => {
    const stat = compile("michelson/passed/rat_arith_mult.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_plus', async () => {
    const stat = compile("michelson/passed/rat_arith_plus.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_uminus', async () => {
    const stat = compile("michelson/passed/rat_arith_uminus.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_eq', async () => {
    const stat = compile("michelson/passed/rat_cmp_eq.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_ge', async () => {
    const stat = compile("michelson/passed/rat_cmp_ge.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_gt', async () => {
    const stat = compile("michelson/passed/rat_cmp_gt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_le', async () => {
    const stat = compile("michelson/passed/rat_cmp_le.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_lt', async () => {
    const stat = compile("michelson/passed/rat_cmp_lt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_dur', async () => {
    const stat = compile("michelson/passed/rat_dur.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_int', async () => {
    const stat = compile("michelson/passed/rat_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_max', async () => {
    const stat = compile("michelson/passed/rat_max.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_min', async () => {
    const stat = compile("michelson/passed/rat_min.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_nat', async () => {
    const stat = compile("michelson/passed/rat_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_neg', async () => {
    const stat = compile("michelson/passed/rat_neg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_tez', async () => {
    const stat = compile("michelson/passed/rat_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_tez_big', async () => {
    const stat = compile("michelson/passed/rat_tez_big.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_cmp', async () => {
    const stat = compile("michelson/passed/rational_cmp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_duration', async () => {
    const stat = compile("michelson/passed/rational_duration.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_full', async () => {
    const stat = compile("michelson/passed/rational_full.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_in_formula', async () => {
    const stat = compile("michelson/passed/rational_in_formula.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_rat_tez_mult', async () => {
    const stat = compile("michelson/passed/rational_rat_tez_mult.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_simple', async () => {
    const stat = compile("michelson/passed/rational_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_tez_rat_mult', async () => {
    const stat = compile("michelson/passed/rational_tez_rat_mult.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rec_update', async () => {
    const stat = compile("michelson/passed/rec_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rec_update2', async () => {
    const stat = compile("michelson/passed/rec_update2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_access', async () => {
    const stat = compile("michelson/passed/record_access.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_access2', async () => {
    const stat = compile("michelson/passed/record_access2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_container', async () => {
    const stat = compile("michelson/passed/record_container.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_double_key', async () => {
    const stat = compile("michelson/passed/record_double_key.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_in_enum', async () => {
    const stat = compile("michelson/passed/record_in_enum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_update', async () => {
    const stat = compile("michelson/passed/record_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('remove_asset_with_partition', async () => {
    const stat = compile("michelson/passed/remove_asset_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('reverse_otherwise', async () => {
    const stat = compile("michelson/passed/reverse_otherwise.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('reverse_with_enum', async () => {
    const stat = compile("michelson/passed/reverse_with_enum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rf_failif_with', async () => {
    const stat = compile("michelson/passed/rf_failif_with.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rf_require_otherwise', async () => {
    const stat = compile("michelson/passed/rf_require_otherwise.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('same_varname_in_two_distinct_scope', async () => {
    const stat = compile("michelson/passed/same_varname_in_two_distinct_scope.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sample_asset_view', async () => {
    const stat = compile("michelson/passed/sample_asset_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sample_make_sandbox_exec_operation', async () => {
    const stat = compile("michelson/passed/sample_make_sandbox_exec_operation.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sample_sandbox_exec', async () => {
    const stat = compile("michelson/passed/sample_sandbox_exec.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sample_view_asset_value', async () => {
    const stat = compile("michelson/passed/sample_view_asset_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sapling_empty_state', async () => {
    const stat = compile("michelson/passed/sapling_empty_state.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sapling_var', async () => {
    const stat = compile("michelson/passed/sapling_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sapling_verify_update', async () => {
    const stat = compile("michelson/passed/sapling_verify_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('section_constant_effect', async () => {
    const stat = compile("michelson/passed/section_constant_effect.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('section_constant_transition', async () => {
    const stat = compile("michelson/passed/section_constant_transition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_no_storage_fail', async () => {
    const stat = compile("michelson/passed/security_pred_no_storage_fail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_not_by_role', async () => {
    const stat = compile("michelson/passed/security_pred_not_by_role.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_not_by_role_in_entry', async () => {
    const stat = compile("michelson/passed/security_pred_not_by_role_in_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_not_in_entry', async () => {
    const stat = compile("michelson/passed/security_pred_not_in_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_only_by_role', async () => {
    const stat = compile("michelson/passed/security_pred_only_by_role.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_only_by_role_in_entry', async () => {
    const stat = compile("michelson/passed/security_pred_only_by_role_in_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_only_in_entry', async () => {
    const stat = compile("michelson/passed/security_pred_only_in_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_transferred_by', async () => {
    const stat = compile("michelson/passed/security_pred_transferred_by.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('security_pred_transferred_to', async () => {
    const stat = compile("michelson/passed/security_pred_transferred_to.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_partition', async () => {
    const stat = compile("michelson/passed/select_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_partition_big_map', async () => {
    const stat = compile("michelson/passed/select_partition_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_with_extra_var', async () => {
    const stat = compile("michelson/passed/select_with_extra_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_with_extra_var2', async () => {
    const stat = compile("michelson/passed/select_with_extra_var2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_with_function_in_predicate', async () => {
    const stat = compile("michelson/passed/select_with_function_in_predicate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('setdelegate', async () => {
    const stat = compile("michelson/passed/setdelegate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('shadow_field', async () => {
    const stat = compile("michelson/passed/shadow_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('shadow_global_var_effect', async () => {
    const stat = compile("michelson/passed/shadow_global_var_effect.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('shadow_var', async () => {
    const stat = compile("michelson/passed/shadow_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('shadow_var_scope', async () => {
    const stat = compile("michelson/passed/shadow_var_scope.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple', async () => {
    const stat = compile("michelson/passed/simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple2', async () => {
    const stat = compile("michelson/passed/simple2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple3', async () => {
    const stat = compile("michelson/passed/simple3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple4', async () => {
    const stat = compile("michelson/passed/simple4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_2vars', async () => {
    const stat = compile("michelson/passed/simple_2vars.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_addupdate', async () => {
    const stat = compile("michelson/passed/simple_addupdate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_addupdate_asset', async () => {
    const stat = compile("michelson/passed/simple_addupdate_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_arg_int', async () => {
    const stat = compile("michelson/passed/simple_arg_int.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_arith', async () => {
    const stat = compile("michelson/passed/simple_arith.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset', async () => {
    const stat = compile("michelson/passed/simple_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_2', async () => {
    const stat = compile("michelson/passed/simple_asset_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_get_asset1_value', async () => {
    const stat = compile("michelson/passed/simple_asset_get_asset1_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_get_asset2_value', async () => {
    const stat = compile("michelson/passed/simple_asset_get_asset2_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_get_asset2_value2', async () => {
    const stat = compile("michelson/passed/simple_asset_get_asset2_value2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_one_field', async () => {
    const stat = compile("michelson/passed/simple_asset_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip', async () => {
    const stat = compile("michelson/passed/simple_asset_skip.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip_empty', async () => {
    const stat = compile("michelson/passed/simple_asset_skip_empty.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip_empty_one_field', async () => {
    const stat = compile("michelson/passed/simple_asset_skip_empty_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip_one_field', async () => {
    const stat = compile("michelson/passed/simple_asset_skip_one_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_assign1', async () => {
    const stat = compile("michelson/passed/simple_assign1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_assign2', async () => {
    const stat = compile("michelson/passed/simple_assign2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_assign3', async () => {
    const stat = compile("michelson/passed/simple_assign3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_contract_call', async () => {
    const stat = compile("michelson/passed/simple_contract_call.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_freeze', async () => {
    const stat = compile("michelson/passed/simple_freeze.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun1', async () => {
    const stat = compile("michelson/passed/simple_fun1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun2', async () => {
    const stat = compile("michelson/passed/simple_fun2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun3', async () => {
    const stat = compile("michelson/passed/simple_fun3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun4', async () => {
    const stat = compile("michelson/passed/simple_fun4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun5', async () => {
    const stat = compile("michelson/passed/simple_fun5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun6', async () => {
    const stat = compile("michelson/passed/simple_fun6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun7', async () => {
    const stat = compile("michelson/passed/simple_fun7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun8', async () => {
    const stat = compile("michelson/passed/simple_fun8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_alt', async () => {
    const stat = compile("michelson/passed/simple_fun_alt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_with_storage', async () => {
    const stat = compile("michelson/passed/simple_fun_with_storage.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_with_storage2', async () => {
    const stat = compile("michelson/passed/simple_fun_with_storage2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_with_storage3', async () => {
    const stat = compile("michelson/passed/simple_fun_with_storage3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_get_field', async () => {
    const stat = compile("michelson/passed/simple_get_field.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_if3', async () => {
    const stat = compile("michelson/passed/simple_if3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_multi_entry', async () => {
    const stat = compile("michelson/passed/simple_multi_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_multi_entry2', async () => {
    const stat = compile("michelson/passed/simple_multi_entry2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_multi_entry3', async () => {
    const stat = compile("michelson/passed/simple_multi_entry3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_op_add', async () => {
    const stat = compile("michelson/passed/simple_op_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_op_uminus', async () => {
    const stat = compile("michelson/passed/simple_op_uminus.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_param', async () => {
    const stat = compile("michelson/passed/simple_param.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_param_const', async () => {
    const stat = compile("michelson/passed/simple_param_const.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_param_with_default', async () => {
    const stat = compile("michelson/passed/simple_param_with_default.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_assign', async () => {
    const stat = compile("michelson/passed/simple_record_assign.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_assign1', async () => {
    const stat = compile("michelson/passed/simple_record_assign1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_assign2', async () => {
    const stat = compile("michelson/passed/simple_record_assign2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_lit', async () => {
    const stat = compile("michelson/passed/simple_record_lit.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_lit_rev', async () => {
    const stat = compile("michelson/passed/simple_record_lit_rev.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_reverse', async () => {
    const stat = compile("michelson/passed/simple_reverse.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence', async () => {
    const stat = compile("michelson/passed/simple_sequence.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence_with_arg', async () => {
    const stat = compile("michelson/passed/simple_sequence_with_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence_with_arg2', async () => {
    const stat = compile("michelson/passed/simple_sequence_with_arg2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence_with_arg_var', async () => {
    const stat = compile("michelson/passed/simple_sequence_with_arg_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_string', async () => {
    const stat = compile("michelson/passed/simple_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_while', async () => {
    const stat = compile("michelson/passed/simple_while.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_with_arg_view', async () => {
    const stat = compile("michelson/passed/simple_with_arg_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_with_type_annot', async () => {
    const stat = compile("michelson/passed/simple_with_type_annot.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_with_view', async () => {
    const stat = compile("michelson/passed/simple_with_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sourced_by', async () => {
    const stat = compile("michelson/passed/sourced_by.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_asset', async () => {
    const stat = compile("michelson/passed/spec_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_definition', async () => {
    const stat = compile("michelson/passed/spec_definition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_definition_2', async () => {
    const stat = compile("michelson/passed/spec_definition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_definition_with_param', async () => {
    const stat = compile("michelson/passed/spec_definition_with_param.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_entry', async () => {
    const stat = compile("michelson/passed/spec_entry.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_fail_caller', async () => {
    const stat = compile("michelson/passed/spec_fail_caller.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_fail_source', async () => {
    const stat = compile("michelson/passed/spec_fail_source.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_fails', async () => {
    const stat = compile("michelson/passed/spec_fails.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_full', async () => {
    const stat = compile("michelson/passed/spec_full.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_function', async () => {
    const stat = compile("michelson/passed/spec_function.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_predicate', async () => {
    const stat = compile("michelson/passed/spec_predicate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('spec_variable', async () => {
    const stat = compile("michelson/passed/spec_variable.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('state_in_effect', async () => {
    const stat = compile("michelson/passed/state_in_effect.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('state_is', async () => {
    const stat = compile("michelson/passed/state_is.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('state_var', async () => {
    const stat = compile("michelson/passed/state_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_bool_false', async () => {
    const stat = compile("michelson/passed/tern_bool_false.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_bool_true', async () => {
    const stat = compile("michelson/passed/tern_bool_true.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_opt', async () => {
    const stat = compile("michelson/passed/tern_opt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_opt_3', async () => {
    const stat = compile("michelson/passed/tern_opt_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset2_with_partition', async () => {
    const stat = compile("michelson/passed/test_add_asset2_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset_with_aggregate', async () => {
    const stat = compile("michelson/passed/test_add_asset_with_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset_with_both', async () => {
    const stat = compile("michelson/passed/test_add_asset_with_both.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset_with_partition', async () => {
    const stat = compile("michelson/passed/test_add_asset_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_aggregate_1', async () => {
    const stat = compile("michelson/passed/test_addfield_aggregate_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_aggregate_2', async () => {
    const stat = compile("michelson/passed/test_addfield_aggregate_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_partition_1', async () => {
    const stat = compile("michelson/passed/test_addfield_partition_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_partition_2', async () => {
    const stat = compile("michelson/passed/test_addfield_partition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addupdate_0', async () => {
    const stat = compile("michelson/passed/test_addupdate_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addupdate_1', async () => {
    const stat = compile("michelson/passed/test_addupdate_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addupdate_2', async () => {
    const stat = compile("michelson/passed/test_addupdate_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset', async () => {
    const stat = compile("michelson/passed/test_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_agg_0', async () => {
    const stat = compile("michelson/passed/test_asset_head_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_agg_1', async () => {
    const stat = compile("michelson/passed/test_asset_head_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_agg_2', async () => {
    const stat = compile("michelson/passed/test_asset_head_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_coll_0', async () => {
    const stat = compile("michelson/passed/test_asset_head_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_coll_1', async () => {
    const stat = compile("michelson/passed/test_asset_head_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_coll_2', async () => {
    const stat = compile("michelson/passed/test_asset_head_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_view_0', async () => {
    const stat = compile("michelson/passed/test_asset_head_view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_view_1', async () => {
    const stat = compile("michelson/passed/test_asset_head_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_view_2', async () => {
    const stat = compile("michelson/passed/test_asset_head_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_multi_key', async () => {
    const stat = compile("michelson/passed/test_asset_multi_key.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_multi_key_complex', async () => {
    const stat = compile("michelson/passed/test_asset_multi_key_complex.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_agg_0', async () => {
    const stat = compile("michelson/passed/test_asset_nth_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_agg_1', async () => {
    const stat = compile("michelson/passed/test_asset_nth_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_agg_2', async () => {
    const stat = compile("michelson/passed/test_asset_nth_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_coll_0', async () => {
    const stat = compile("michelson/passed/test_asset_nth_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_coll_1', async () => {
    const stat = compile("michelson/passed/test_asset_nth_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_coll_2', async () => {
    const stat = compile("michelson/passed/test_asset_nth_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_view_0', async () => {
    const stat = compile("michelson/passed/test_asset_nth_view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_view_1', async () => {
    const stat = compile("michelson/passed/test_asset_nth_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_view_2', async () => {
    const stat = compile("michelson/passed/test_asset_nth_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_agg_0', async () => {
    const stat = compile("michelson/passed/test_asset_select_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_agg_1', async () => {
    const stat = compile("michelson/passed/test_asset_select_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_agg_2', async () => {
    const stat = compile("michelson/passed/test_asset_select_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_coll_0', async () => {
    const stat = compile("michelson/passed/test_asset_select_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_coll_1', async () => {
    const stat = compile("michelson/passed/test_asset_select_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_coll_2', async () => {
    const stat = compile("michelson/passed/test_asset_select_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_view_0', async () => {
    const stat = compile("michelson/passed/test_asset_select_view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_view_1', async () => {
    const stat = compile("michelson/passed/test_asset_select_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_view_2', async () => {
    const stat = compile("michelson/passed/test_asset_select_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_agg_0', async () => {
    const stat = compile("michelson/passed/test_asset_sort_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_agg_1', async () => {
    const stat = compile("michelson/passed/test_asset_sort_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_agg_2', async () => {
    const stat = compile("michelson/passed/test_asset_sort_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_0', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_1', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_2', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_complex', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_complex.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_random', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_random.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_random2', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_random2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_rational', async () => {
    const stat = compile("michelson/passed/test_asset_sort_coll_rational.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_view_0', async () => {
    const stat = compile("michelson/passed/test_asset_sort_view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_view_1', async () => {
    const stat = compile("michelson/passed/test_asset_sort_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_view_2', async () => {
    const stat = compile("michelson/passed/test_asset_sort_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_agg_0', async () => {
    const stat = compile("michelson/passed/test_asset_sum_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_agg_1', async () => {
    const stat = compile("michelson/passed/test_asset_sum_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_agg_2', async () => {
    const stat = compile("michelson/passed/test_asset_sum_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_0', async () => {
    const stat = compile("michelson/passed/test_asset_sum_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_1', async () => {
    const stat = compile("michelson/passed/test_asset_sum_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_2', async () => {
    const stat = compile("michelson/passed/test_asset_sum_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_rat', async () => {
    const stat = compile("michelson/passed/test_asset_sum_coll_rat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_view_0', async () => {
    const stat = compile("michelson/passed/test_asset_sum_view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_view_1', async () => {
    const stat = compile("michelson/passed/test_asset_sum_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_view_2', async () => {
    const stat = compile("michelson/passed/test_asset_sum_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_agg_0', async () => {
    const stat = compile("michelson/passed/test_asset_tail_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_agg_1', async () => {
    const stat = compile("michelson/passed/test_asset_tail_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_agg_2', async () => {
    const stat = compile("michelson/passed/test_asset_tail_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_coll_0', async () => {
    const stat = compile("michelson/passed/test_asset_tail_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_coll_1', async () => {
    const stat = compile("michelson/passed/test_asset_tail_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_coll_2', async () => {
    const stat = compile("michelson/passed/test_asset_tail_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_view_0', async () => {
    const stat = compile("michelson/passed/test_asset_tail_view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_view_1', async () => {
    const stat = compile("michelson/passed/test_asset_tail_view_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_view_2', async () => {
    const stat = compile("michelson/passed/test_asset_tail_view_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update', async () => {
    const stat = compile("michelson/passed/test_asset_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_2', async () => {
    const stat = compile("michelson/passed/test_asset_update_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_3', async () => {
    const stat = compile("michelson/passed/test_asset_update_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_4', async () => {
    const stat = compile("michelson/passed/test_asset_update_4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_5', async () => {
    const stat = compile("michelson/passed/test_asset_update_5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_aggregate_1', async () => {
    const stat = compile("michelson/passed/test_asset_update_aggregate_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_aggregate_2', async () => {
    const stat = compile("michelson/passed/test_asset_update_aggregate_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_aggregate_3', async () => {
    const stat = compile("michelson/passed/test_asset_update_aggregate_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_1', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_2', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_3', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_4', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_5', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_6', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_7', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_8', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_9', async () => {
    const stat = compile("michelson/passed/test_asset_update_partition_9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_view', async () => {
    const stat = compile("michelson/passed/test_asset_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_bound_value', async () => {
    const stat = compile("michelson/passed/test_bound_value.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_caller_getter', async () => {
    const stat = compile("michelson/passed/test_caller_getter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_cmp_bool', async () => {
    const stat = compile("michelson/passed/test_cmp_bool.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_complex_sum', async () => {
    const stat = compile("michelson/passed/test_complex_sum.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_conditions', async () => {
    const stat = compile("michelson/passed/test_conditions.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_contains_get', async () => {
    const stat = compile("michelson/passed/test_contains_get.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_contract', async () => {
    const stat = compile("michelson/passed/test_contract.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_contract_self', async () => {
    const stat = compile("michelson/passed/test_contract_self.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_fa1', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl_fa1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_fa2', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl_fa2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_string', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_with_param', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl_with_param.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_with_param_const', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl_with_param_const.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_with_param_with_default', async () => {
    const stat = compile("michelson/passed/test_create_contract_arl_with_param_with_default.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_bytes', async () => {
    const stat = compile("michelson/passed/test_create_contract_bytes.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_inline', async () => {
    const stat = compile("michelson/passed/test_create_contract_inline.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_tz_with_import', async () => {
    const stat = compile("michelson/passed/test_create_contract_tz_with_import.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_tz_with_path', async () => {
    const stat = compile("michelson/passed/test_create_contract_tz_with_path.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fget', async () => {
    const stat = compile("michelson/passed/test_fget.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_for_list_alt', async () => {
    const stat = compile("michelson/passed/test_for_list_alt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun0', async () => {
    const stat = compile("michelson/passed/test_fun0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun1', async () => {
    const stat = compile("michelson/passed/test_fun1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun2', async () => {
    const stat = compile("michelson/passed/test_fun2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun3', async () => {
    const stat = compile("michelson/passed/test_fun3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun4', async () => {
    const stat = compile("michelson/passed/test_fun4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun5', async () => {
    const stat = compile("michelson/passed/test_fun5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun6', async () => {
    const stat = compile("michelson/passed/test_fun6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun7', async () => {
    const stat = compile("michelson/passed/test_fun7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun8', async () => {
    const stat = compile("michelson/passed/test_fun8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun_asset', async () => {
    const stat = compile("michelson/passed/test_fun_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun_asset2', async () => {
    const stat = compile("michelson/passed/test_fun_asset2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun_fail', async () => {
    const stat = compile("michelson/passed/test_fun_fail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter', async () => {
    const stat = compile("michelson/passed/test_getter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter2', async () => {
    const stat = compile("michelson/passed/test_getter2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter_with_arg', async () => {
    const stat = compile("michelson/passed/test_getter_with_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter_with_args', async () => {
    const stat = compile("michelson/passed/test_getter_with_args.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_global_constant', async () => {
    const stat = compile("michelson/passed/test_global_constant.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_if_fail_expr', async () => {
    const stat = compile("michelson/passed/test_if_fail_expr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_if_int_nat', async () => {
    const stat = compile("michelson/passed/test_if_int_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_asset', async () => {
    const stat = compile("michelson/passed/test_init_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_asset2', async () => {
    const stat = compile("michelson/passed/test_init_asset2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_asset3', async () => {
    const stat = compile("michelson/passed/test_init_asset3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_rat_with_nat', async () => {
    const stat = compile("michelson/passed/test_init_rat_with_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_arith', async () => {
    const stat = compile("michelson/passed/test_init_storage_arith.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_cmp', async () => {
    const stat = compile("michelson/passed/test_init_storage_cmp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_funs', async () => {
    const stat = compile("michelson/passed/test_init_storage_funs.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_literals', async () => {
    const stat = compile("michelson/passed/test_init_storage_literals.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_simple', async () => {
    const stat = compile("michelson/passed/test_init_storage_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_initialized_with', async () => {
    const stat = compile("michelson/passed/test_initialized_with.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_initialized_with_asset', async () => {
    const stat = compile("michelson/passed/test_initialized_with_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_initializedby', async () => {
    const stat = compile("michelson/passed/test_initializedby.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_is_implicit_address', async () => {
    const stat = compile("michelson/passed/test_is_implicit_address.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_iter', async () => {
    const stat = compile("michelson/passed/test_iter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_iter2', async () => {
    const stat = compile("michelson/passed/test_iter2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_length_operations', async () => {
    const stat = compile("michelson/passed/test_length_operations.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_contains', async () => {
    const stat = compile("michelson/passed/test_list_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_contains2', async () => {
    const stat = compile("michelson/passed/test_list_contains2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_mynth', async () => {
    const stat = compile("michelson/passed/test_list_mynth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_mynth2', async () => {
    const stat = compile("michelson/passed/test_list_mynth2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_mynth3', async () => {
    const stat = compile("michelson/passed/test_list_mynth3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_nth', async () => {
    const stat = compile("michelson/passed/test_list_nth.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_metadata', async () => {
    const stat = compile("michelson/passed/test_metadata.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_operations', async () => {
    const stat = compile("michelson/passed/test_operations.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_oracle', async () => {
    const stat = compile("michelson/passed/test_oracle.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_oracle_called', async () => {
    const stat = compile("michelson/passed/test_oracle_called.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_parameter', async () => {
    const stat = compile("michelson/passed/test_parameter.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_parameter_const', async () => {
    const stat = compile("michelson/passed/test_parameter_const.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_prec', async () => {
    const stat = compile("michelson/passed/test_prec.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_quantifiers', async () => {
    const stat = compile("michelson/passed/test_quantifiers.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_rational', async () => {
    const stat = compile("michelson/passed/test_rational.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_read_asset_after_operation', async () => {
    const stat = compile("michelson/passed/test_read_asset_after_operation.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_read_asset_after_update', async () => {
    const stat = compile("michelson/passed/test_read_asset_after_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record', async () => {
    const stat = compile("michelson/passed/test_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_0', async () => {
    const stat = compile("michelson/passed/test_record_access_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_1', async () => {
    const stat = compile("michelson/passed/test_record_access_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_2', async () => {
    const stat = compile("michelson/passed/test_record_access_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_3', async () => {
    const stat = compile("michelson/passed/test_record_access_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_1', async () => {
    const stat = compile("michelson/passed/test_record_assign_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_2', async () => {
    const stat = compile("michelson/passed/test_record_assign_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_3', async () => {
    const stat = compile("michelson/passed/test_record_assign_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_full', async () => {
    const stat = compile("michelson/passed/test_record_assign_full.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_var', async () => {
    const stat = compile("michelson/passed/test_record_assign_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_simple', async () => {
    const stat = compile("michelson/passed/test_record_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_remove_asset_with_partition', async () => {
    const stat = compile("michelson/passed/test_remove_asset_with_partition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_aggregate', async () => {
    const stat = compile("michelson/passed/test_removeall_aggregate.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_aggregate_1', async () => {
    const stat = compile("michelson/passed/test_removeall_aggregate_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_aggregate_2', async () => {
    const stat = compile("michelson/passed/test_removeall_aggregate_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_partition_1', async () => {
    const stat = compile("michelson/passed/test_removeall_partition_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_partition_2', async () => {
    const stat = compile("michelson/passed/test_removeall_partition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_aggregate_1', async () => {
    const stat = compile("michelson/passed/test_removefield_aggregate_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_aggregate_2', async () => {
    const stat = compile("michelson/passed/test_removefield_aggregate_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_partition_1', async () => {
    const stat = compile("michelson/passed/test_removefield_partition_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_partition_2', async () => {
    const stat = compile("michelson/passed/test_removefield_partition_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_agg_0', async () => {
    const stat = compile("michelson/passed/test_removeif_agg_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_agg_1', async () => {
    const stat = compile("michelson/passed/test_removeif_agg_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_agg_2', async () => {
    const stat = compile("michelson/passed/test_removeif_agg_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_coll_0', async () => {
    const stat = compile("michelson/passed/test_removeif_coll_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_coll_1', async () => {
    const stat = compile("michelson/passed/test_removeif_coll_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_coll_2', async () => {
    const stat = compile("michelson/passed/test_removeif_coll_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_part_0', async () => {
    const stat = compile("michelson/passed/test_removeif_part_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_part_1', async () => {
    const stat = compile("michelson/passed/test_removeif_part_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_part_2', async () => {
    const stat = compile("michelson/passed/test_removeif_part_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_result', async () => {
    const stat = compile("michelson/passed/test_result.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_security', async () => {
    const stat = compile("michelson/passed/test_security.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_specasset', async () => {
    const stat = compile("michelson/passed/test_specasset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_specfun', async () => {
    const stat = compile("michelson/passed/test_specfun.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_specvar', async () => {
    const stat = compile("michelson/passed/test_specvar.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_split_ticket', async () => {
    const stat = compile("michelson/passed/test_split_ticket.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tez', async () => {
    const stat = compile("michelson/passed/test_tez.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_transfer', async () => {
    const stat = compile("michelson/passed/test_transfer.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_transition', async () => {
    const stat = compile("michelson/passed/test_transition.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tuple_access_1', async () => {
    const stat = compile("michelson/passed/test_tuple_access_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tuple_access_2', async () => {
    const stat = compile("michelson/passed/test_tuple_access_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tuple_access_3', async () => {
    const stat = compile("michelson/passed/test_tuple_access_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_update', async () => {
    const stat = compile("michelson/passed/test_update.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_var', async () => {
    const stat = compile("michelson/passed/test_var.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_voting', async () => {
    const stat = compile("michelson/passed/test_voting.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_create_ticket', async () => {
    const stat = compile("michelson/passed/ticket_create_ticket.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_create_ticket_list_prepend', async () => {
    const stat = compile("michelson/passed/ticket_create_ticket_list_prepend.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_big_map', async () => {
    const stat = compile("michelson/passed/ticket_detach_big_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_big_map_record', async () => {
    const stat = compile("michelson/passed/ticket_detach_big_map_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_big_map_tuple', async () => {
    const stat = compile("michelson/passed/ticket_detach_big_map_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_map', async () => {
    const stat = compile("michelson/passed/ticket_detach_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_map_record', async () => {
    const stat = compile("michelson/passed/ticket_detach_map_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_map_tuple', async () => {
    const stat = compile("michelson/passed/ticket_detach_map_tuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_option', async () => {
    const stat = compile("michelson/passed/ticket_detach_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_fun_join_tickets', async () => {
    const stat = compile("michelson/passed/ticket_fun_join_tickets.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_fun_split_ticket', async () => {
    const stat = compile("michelson/passed/ticket_fun_split_ticket.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_list', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_arg_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_record_list', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_arg_record_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_record_list2', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_arg_record_list2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_tuple_2_list', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_arg_tuple_2_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_tuple_3_list', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_arg_tuple_3_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_record', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_record.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_record_list', async () => {
    const stat = compile("michelson/passed/ticket_read_ticket_record_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_0_0', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_0_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_0_1', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_0_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_0_2', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_0_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_1_0', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_1_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_1_1', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_1_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_1_2', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_1_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_2_0', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_2_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_2_1', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_2_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_2_2', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_2_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_3_0', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_3_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_3_1', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_3_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_3_2', async () => {
    const stat = compile("michelson/passed/ticket_record_list_var_3_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_store_map', async () => {
    const stat = compile("michelson/passed/ticket_store_map.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_store_option', async () => {
    const stat = compile("michelson/passed/ticket_store_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_detach_option', async () => {
    const stat = compile("michelson/passed/ticket_var_detach_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_list', async () => {
    const stat = compile("michelson/passed/ticket_var_list.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_option', async () => {
    const stat = compile("michelson/passed/ticket_var_option.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_or_left', async () => {
    const stat = compile("michelson/passed/ticket_var_or_left.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_or_right', async () => {
    const stat = compile("michelson/passed/ticket_var_or_right.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_simple', async () => {
    const stat = compile("michelson/passed/ticket_var_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_call', async () => {
    const stat = compile("michelson/passed/transfer_call.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_entrypoint', async () => {
    const stat = compile("michelson/passed/transfer_entrypoint.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_entrypoint2', async () => {
    const stat = compile("michelson/passed/transfer_entrypoint2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_op', async () => {
    const stat = compile("michelson/passed/transfer_op.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_require_entrypoint', async () => {
    const stat = compile("michelson/passed/transfer_require_entrypoint.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_self', async () => {
    const stat = compile("michelson/passed/transfer_self.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_simple', async () => {
    const stat = compile("michelson/passed/transfer_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_simple_with_entrypoint', async () => {
    const stat = compile("michelson/passed/transfer_simple_with_entrypoint.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tuple_in_contains', async () => {
    const stat = compile("michelson/passed/tuple_in_contains.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_never', async () => {
    const stat = compile("michelson/passed/type_never.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_or', async () => {
    const stat = compile("michelson/passed/type_or.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_set_enum_param', async () => {
    const stat = compile("michelson/passed/type_set_enum_param.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_storage_or', async () => {
    const stat = compile("michelson/passed/type_storage_or.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('typetuple', async () => {
    const stat = compile("michelson/passed/typetuple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('unused_argument', async () => {
    const stat = compile("michelson/passed/unused_argument.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('unused_variable', async () => {
    const stat = compile("michelson/passed/unused_variable.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('unused_variable_opt', async () => {
    const stat = compile("michelson/passed/unused_variable_opt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('update_minus_equal', async () => {
    const stat = compile("michelson/passed/update_minus_equal.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_in_spec', async () => {
    const stat = compile("michelson/passed/var_in_spec.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_in_state_inv', async () => {
    const stat = compile("michelson/passed/var_in_state_inv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_ids', async () => {
    const stat = compile("michelson/passed/var_mult_ids.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_ids_3', async () => {
    const stat = compile("michelson/passed/var_mult_ids_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_ids_complex', async () => {
    const stat = compile("michelson/passed/var_mult_ids_complex.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_tickets', async () => {
    const stat = compile("michelson/passed/var_mult_tickets.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_without_effect', async () => {
    const stat = compile("michelson/passed/var_without_effect.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('variable_in_container', async () => {
    const stat = compile("michelson/passed/variable_in_container.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('verif_fail', async () => {
    const stat = compile("michelson/passed/verif_fail.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('verif_simple', async () => {
    const stat = compile("michelson/passed/verif_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('very_simple', async () => {
    const stat = compile("michelson/passed/very_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_0', async () => {
    const stat = compile("michelson/passed/view_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_all_chain', async () => {
    const stat = compile("michelson/passed/view_all_chain.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_0', async () => {
    const stat = compile("michelson/passed/view_args_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_1', async () => {
    const stat = compile("michelson/passed/view_args_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_storage_0', async () => {
    const stat = compile("michelson/passed/view_args_storage_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_storage_1', async () => {
    const stat = compile("michelson/passed/view_args_storage_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_asset', async () => {
    const stat = compile("michelson/passed/view_asset.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_exhaustive', async () => {
    const stat = compile("michelson/passed/view_exhaustive.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_in_arg', async () => {
    const stat = compile("michelson/passed/view_in_arg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_offchain', async () => {
    const stat = compile("michelson/passed/view_offchain.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_offchain_nat', async () => {
    const stat = compile("michelson/passed/view_offchain_nat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_onchain', async () => {
    const stat = compile("michelson/passed/view_onchain.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_onchain_offchain', async () => {
    const stat = compile("michelson/passed/view_onchain_offchain.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_simple', async () => {
    const stat = compile("michelson/passed/view_simple.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_simple_call', async () => {
    const stat = compile("michelson/passed/view_simple_call.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_simple_caller', async () => {
    const stat = compile("michelson/passed/view_simple_caller.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_0', async () => {
    const stat = compile("michelson/passed/view_storage_0.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_1', async () => {
    const stat = compile("michelson/passed/view_storage_1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_2', async () => {
    const stat = compile("michelson/passed/view_storage_2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_3', async () => {
    const stat = compile("michelson/passed/view_storage_3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_4', async () => {
    const stat = compile("michelson/passed/view_storage_4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_5', async () => {
    const stat = compile("michelson/passed/view_storage_5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_with_nat_to_string', async () => {
    const stat = compile("michelson/passed/view_with_nat_to_string.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_with_self', async () => {
    const stat = compile("michelson/passed/view_with_self.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_with_self_add', async () => {
    const stat = compile("michelson/passed/view_with_self_add.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('with_metadata_json', async () => {
    const stat = compile("michelson/passed/with_metadata_json.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('with_metadata_json_with_offchain_view', async () => {
    const stat = compile("michelson/passed/with_metadata_json_with_offchain_view.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('with_metadata_uri', async () => {
    const stat = compile("michelson/passed/with_metadata_uri.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
})
  