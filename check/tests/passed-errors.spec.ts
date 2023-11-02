/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, [p], { });
  return res
}

/* Tests ------------------------------------------------------------------- */

describe('passed-errors', async () => {
  it('add_update_record', async () => {
    const stat = compile("../tests/passed/add_update_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_partition', async () => {
    const stat = compile("../tests/passed/addupdate_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_partition2', async () => {
    const stat = compile("../tests/passed/addupdate_partition2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_partition_with_no_effect_on_default_value', async () => {
    const stat = compile("../tests/passed/addupdate_partition_with_no_effect_on_default_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('addupdate_with_no_effect_on_default_value', async () => {
    const stat = compile("../tests/passed/addupdate_with_no_effect_on_default_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('annot_enum', async () => {
    const stat = compile("../tests/passed/annot_enum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('apply_lambda', async () => {
    const stat = compile("../tests/passed/apply_lambda.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('arg_fun_constant', async () => {
    const stat = compile("../tests/passed/arg_fun_constant.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('arith_bls', async () => {
    const stat = compile("../tests/passed/arith_bls.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('arith_tez', async () => {
    const stat = compile("../tests/passed/arith_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ascii_string', async () => {
    const stat = compile("../tests/passed/ascii_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access', async () => {
    const stat = compile("../tests/passed/asset_access.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_basic', async () => {
    const stat = compile("../tests/passed/asset_access_basic.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_option_found', async () => {
    const stat = compile("../tests/passed/asset_access_option_found.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_option_not_found', async () => {
    const stat = compile("../tests/passed/asset_access_option_not_found.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_access_value', async () => {
    const stat = compile("../tests/passed/asset_access_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_addupdate', async () => {
    const stat = compile("../tests/passed/asset_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map', async () => {
    const stat = compile("../tests/passed/asset_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_add', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_effect_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_addupdate', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_effect_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_remove', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_effect_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_removeall', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_effect_removeall.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_effect_update', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_effect_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_expression_contains', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_expression_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_big_map_unit_storage', async () => {
    const stat = compile("../tests/passed/asset_big_map_unit_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_for', async () => {
    const stat = compile("../tests/passed/asset_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_init_by_const_key', async () => {
    const stat = compile("../tests/passed/asset_init_by_const_key.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_init_by_const_key_parameter', async () => {
    const stat = compile("../tests/passed/asset_init_by_const_key_parameter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_initializedby_aggregate_empty', async () => {
    const stat = compile("../tests/passed/asset_initializedby_aggregate_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_initializedby_aggregate_filled', async () => {
    const stat = compile("../tests/passed/asset_initializedby_aggregate_filled.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_intructions', async () => {
    const stat = compile("../tests/passed/asset_intructions.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_add', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_effect_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_addupdate', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_effect_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_remove', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_effect_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_removeall', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_effect_removeall.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_removeif', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_effect_removeif.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_effect_update', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_effect_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_contains', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_count', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_count.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_get', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_head', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_head.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_nth', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_select', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_select.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_sort', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_sort.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_sum', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_sum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_expression_tail', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_expression_tail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_instruction_for', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_instruction_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_add', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_effect_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_addupdate', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_effect_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_remove', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_effect_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_removeall', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_effect_removeall.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_removeif', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_effect_removeif.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_effect_update', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_effect_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_contains', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_count', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_count.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_get', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_head', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_head.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_nth', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_select', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_select.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_sort', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_sort.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_sum', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_sum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_expression_tail', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_expression_tail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_instruction_for', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_instruction_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_multi_storage', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_multi_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_storage', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_add', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_effect_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_addupdate', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_effect_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_remove', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_effect_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_removeall', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_effect_removeall.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_removeif', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_effect_removeif.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_effect_update', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_effect_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_contains', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_count', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_count.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_head', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_head.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_nth', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_select', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_select.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_sort', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_sort.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_sum', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_sum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_expression_tail', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_expression_tail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_instruction_for', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_instruction_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_iterable_big_map_unit_storage', async () => {
    const stat = compile("../tests/passed/asset_iterable_big_map_unit_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_key_in_record', async () => {
    const stat = compile("../tests/passed/asset_key_in_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_key_tuple', async () => {
    const stat = compile("../tests/passed/asset_key_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_not_found', async () => {
    const stat = compile("../tests/passed/asset_not_found.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_nth', async () => {
    const stat = compile("../tests/passed/asset_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_put_single', async () => {
    const stat = compile("../tests/passed/asset_put_single.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_simple', async () => {
    const stat = compile("../tests/passed/asset_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_simple_to_big_map', async () => {
    const stat = compile("../tests/passed/asset_simple_to_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_simple_to_iterable_big_map', async () => {
    const stat = compile("../tests/passed/asset_simple_to_iterable_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_tern_opt', async () => {
    const stat = compile("../tests/passed/asset_tern_opt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_ternary_expr_found', async () => {
    const stat = compile("../tests/passed/asset_ternary_expr_found.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_ternary_expr_notfound', async () => {
    const stat = compile("../tests/passed/asset_ternary_expr_notfound.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_types_get', async () => {
    const stat = compile("../tests/passed/asset_types_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_lit_add', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_map_lit_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_lit_remove', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_map_lit_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_list_add', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_map_var_list_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_list_remove', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_map_var_list_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_set_add', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_map_var_set_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_map_var_set_remove', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_map_var_set_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_lit_add', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_set_lit_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_lit_remove', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_set_lit_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_list_add', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_set_var_list_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_list_remove', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_set_var_list_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_set_add', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_set_var_set_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('asset_update_with_basic_container_set_var_set_remove', async () => {
    const stat = compile("../tests/passed/asset_update_with_basic_container_set_var_set_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_add_record', async () => {
    const stat = compile("../tests/passed/assign_add_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_add_tuple', async () => {
    const stat = compile("../tests/passed/assign_add_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_field', async () => {
    const stat = compile("../tests/passed/assign_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_minus_nat', async () => {
    const stat = compile("../tests/passed/assign_minus_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_opt', async () => {
    const stat = compile("../tests/passed/assign_opt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_var_rat_int', async () => {
    const stat = compile("../tests/passed/assign_var_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_vardecl_rat_int', async () => {
    const stat = compile("../tests/passed/assign_vardecl_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('assign_vardecl_rat_nat', async () => {
    const stat = compile("../tests/passed/assign_vardecl_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('before_asset_api', async () => {
    const stat = compile("../tests/passed/before_asset_api.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('before_var', async () => {
    const stat = compile("../tests/passed/before_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('bls_lit', async () => {
    const stat = compile("../tests/passed/bls_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('builtin_in_function', async () => {
    const stat = compile("../tests/passed/builtin_in_function.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('called_by_an_asset', async () => {
    const stat = compile("../tests/passed/called_by_an_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast', async () => {
    const stat = compile("../tests/passed/cast.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_dur_int', async () => {
    const stat = compile("../tests/passed/cast_dur_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_nat_int', async () => {
    const stat = compile("../tests/passed/cast_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_nat_int_lit', async () => {
    const stat = compile("../tests/passed/cast_nat_int_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_return', async () => {
    const stat = compile("../tests/passed/cast_return.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('cast_view_pklist', async () => {
    const stat = compile("../tests/passed/cast_view_pklist.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('col_iter_direct_storage', async () => {
    const stat = compile("../tests/passed/col_iter_direct_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('col_iter_filter_storage', async () => {
    const stat = compile("../tests/passed/col_iter_filter_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('compare_enum', async () => {
    const stat = compile("../tests/passed/compare_enum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('const_decl', async () => {
    const stat = compile("../tests/passed/const_decl.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('containers_of_tuple', async () => {
    const stat = compile("../tests/passed/containers_of_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_called', async () => {
    const stat = compile("../tests/passed/contract_called.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_caller', async () => {
    const stat = compile("../tests/passed/contract_caller.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_empty', async () => {
    const stat = compile("../tests/passed/contract_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_to_address', async () => {
    const stat = compile("../tests/passed/contract_to_address.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('contract_transition', async () => {
    const stat = compile("../tests/passed/contract_transition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('counter', async () => {
    const stat = compile("../tests/passed/counter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('counter_proxy', async () => {
    const stat = compile("../tests/passed/counter_proxy.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_args_with_record', async () => {
    const stat = compile("../tests/passed/custom_args_with_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage', async () => {
    const stat = compile("../tests/passed/custom_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage10', async () => {
    const stat = compile("../tests/passed/custom_storage10.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage2', async () => {
    const stat = compile("../tests/passed/custom_storage2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage3', async () => {
    const stat = compile("../tests/passed/custom_storage3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage4', async () => {
    const stat = compile("../tests/passed/custom_storage4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage5', async () => {
    const stat = compile("../tests/passed/custom_storage5.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage6', async () => {
    const stat = compile("../tests/passed/custom_storage6.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage7', async () => {
    const stat = compile("../tests/passed/custom_storage7.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage8', async () => {
    const stat = compile("../tests/passed/custom_storage8.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('custom_storage9', async () => {
    const stat = compile("../tests/passed/custom_storage9.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('dangling_else', async () => {
    const stat = compile("../tests/passed/dangling_else.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('debug_assign', async () => {
    const stat = compile("../tests/passed/debug_assign.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('dec_lit', async () => {
    const stat = compile("../tests/passed/dec_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decl_var_opt', async () => {
    const stat = compile("../tests/passed/decl_var_opt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decl_var_opt_default', async () => {
    const stat = compile("../tests/passed/decl_var_opt_default.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if', async () => {
    const stat = compile("../tests/passed/decomp_if.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if2', async () => {
    const stat = compile("../tests/passed/decomp_if2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if3', async () => {
    const stat = compile("../tests/passed/decomp_if3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_if4', async () => {
    const stat = compile("../tests/passed/decomp_if4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_ifexpr', async () => {
    const stat = compile("../tests/passed/decomp_ifexpr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_map', async () => {
    const stat = compile("../tests/passed/decomp_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_test', async () => {
    const stat = compile("../tests/passed/decomp_test.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_test2', async () => {
    const stat = compile("../tests/passed/decomp_test2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_while', async () => {
    const stat = compile("../tests/passed/decomp_while.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_while1', async () => {
    const stat = compile("../tests/passed/decomp_while1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('decomp_while2', async () => {
    const stat = compile("../tests/passed/decomp_while2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_big_map_string', async () => {
    const stat = compile("../tests/passed/detach_big_map_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_big_map_unit', async () => {
    const stat = compile("../tests/passed/detach_big_map_unit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_map_string', async () => {
    const stat = compile("../tests/passed/detach_map_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('detach_option_string', async () => {
    const stat = compile("../tests/passed/detach_option_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('duration_to_int', async () => {
    const stat = compile("../tests/passed/duration_to_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_add_asset_with_complex_partition', async () => {
    const stat = compile("../tests/passed/effect_add_asset_with_complex_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_aggregate', async () => {
    const stat = compile("../tests/passed/effect_control_for_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_collection', async () => {
    const stat = compile("../tests/passed/effect_control_for_collection.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_collection_one_field', async () => {
    const stat = compile("../tests/passed/effect_control_for_collection_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_list', async () => {
    const stat = compile("../tests/passed/effect_control_for_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_map', async () => {
    const stat = compile("../tests/passed/effect_control_for_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_partition', async () => {
    const stat = compile("../tests/passed/effect_control_for_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_set', async () => {
    const stat = compile("../tests/passed/effect_control_for_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_for_view', async () => {
    const stat = compile("../tests/passed/effect_control_for_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_if', async () => {
    const stat = compile("../tests/passed/effect_control_if.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_if_else', async () => {
    const stat = compile("../tests/passed/effect_control_if_else.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_iter', async () => {
    const stat = compile("../tests/passed/effect_control_iter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_iter_init', async () => {
    const stat = compile("../tests/passed/effect_control_iter_init.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_enum', async () => {
    const stat = compile("../tests/passed/effect_control_match_enum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_list', async () => {
    const stat = compile("../tests/passed/effect_control_match_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_option', async () => {
    const stat = compile("../tests/passed/effect_control_match_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_match_or', async () => {
    const stat = compile("../tests/passed/effect_control_match_or.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_matchwith', async () => {
    const stat = compile("../tests/passed/effect_control_matchwith.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_sequence', async () => {
    const stat = compile("../tests/passed/effect_control_sequence.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_control_while', async () => {
    const stat = compile("../tests/passed/effect_control_while.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_dofailif', async () => {
    const stat = compile("../tests/passed/effect_dofailif.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_dorequire', async () => {
    const stat = compile("../tests/passed/effect_dorequire.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_dorequire_not', async () => {
    const stat = compile("../tests/passed/effect_dorequire_not.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_fail', async () => {
    const stat = compile("../tests/passed/effect_fail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_fail_complex', async () => {
    const stat = compile("../tests/passed/effect_fail_complex.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_instruction_put_in_asset', async () => {
    const stat = compile("../tests/passed/effect_instruction_put_in_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset2', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_asset2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_one_field', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_asset_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_with_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_asset_with_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_with_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_asset_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_asset_with_partition_2', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_asset_with_partition_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_add_partition_one_field', async () => {
    const stat = compile("../tests/passed/effect_method_asset_add_partition_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_add_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_map', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_add_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_map_var', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_add_map_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_add_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_add_set', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_add_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_remove_map', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_remove_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_remove_set', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_remove_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_replace_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_replace_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_addupdate_with_replace_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_addupdate_with_replace_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_0_put_remove_put', async () => {
    const stat = compile("../tests/passed/effect_method_asset_big_map_0_put_remove_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_0_put_remove_remove', async () => {
    const stat = compile("../tests/passed/effect_method_asset_big_map_0_put_remove_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_1_put_remove_put', async () => {
    const stat = compile("../tests/passed/effect_method_asset_big_map_1_put_remove_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_1_put_remove_remove', async () => {
    const stat = compile("../tests/passed/effect_method_asset_big_map_1_put_remove_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_2_put_remove_put', async () => {
    const stat = compile("../tests/passed/effect_method_asset_big_map_2_put_remove_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_big_map_2_put_remove_remove', async () => {
    const stat = compile("../tests/passed/effect_method_asset_big_map_2_put_remove_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_clear_view', async () => {
    const stat = compile("../tests/passed/effect_method_asset_clear_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_clear_view_with_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_clear_view_with_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_clear_view_with_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_clear_view_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_0_put_remove_put', async () => {
    const stat = compile("../tests/passed/effect_method_asset_map_0_put_remove_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_0_put_remove_remove', async () => {
    const stat = compile("../tests/passed/effect_method_asset_map_0_put_remove_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_1_put_remove_put', async () => {
    const stat = compile("../tests/passed/effect_method_asset_map_1_put_remove_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_1_put_remove_remove', async () => {
    const stat = compile("../tests/passed/effect_method_asset_map_1_put_remove_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_2_put_remove_put', async () => {
    const stat = compile("../tests/passed/effect_method_asset_map_2_put_remove_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_map_2_put_remove_remove', async () => {
    const stat = compile("../tests/passed/effect_method_asset_map_2_put_remove_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_all_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_asset_one_field', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_all_asset_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_asset_with_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_all_asset_with_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_asset_with_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_all_asset_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_all_collection', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_all_collection.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset2', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_asset2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_one_field', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_asset_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_with_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_asset_with_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_with_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_asset_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_asset_with_partition_2', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_asset_with_partition_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_remove_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_remove_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeall_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeall_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeall_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeall_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeif_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_collection', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeif_collection.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_collection_with_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeif_collection_with_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_collection_with_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeif_collection_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_removeif_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_removeif_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_coll_1', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_all_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_coll_2', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_all_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_view_1', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_all_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_all_view_2', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_all_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_add_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_map', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_add_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_add_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_add_set', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_add_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_map', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_remove_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_map', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_remove_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_remove_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_remove_set', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_remove_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_replace_aggregate', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_replace_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_replace_partition', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_replace_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_method_asset_update_with_set', async () => {
    const stat = compile("../tests/passed/effect_method_asset_update_with_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_transfer_contract', async () => {
    const stat = compile("../tests/passed/effect_transfer_contract.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('effect_transfer_simple', async () => {
    const stat = compile("../tests/passed/effect_transfer_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_inspector', async () => {
    const stat = compile("../tests/passed/entry_inspector.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_called_by_otherwise', async () => {
    const stat = compile("../tests/passed/entry_section_called_by_otherwise.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_no_transfer_otherwise', async () => {
    const stat = compile("../tests/passed/entry_section_no_transfer_otherwise.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_sourced_by_otherwise', async () => {
    const stat = compile("../tests/passed/entry_section_sourced_by_otherwise.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_section_state_is_otherwise', async () => {
    const stat = compile("../tests/passed/entry_section_state_is_otherwise.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_token', async () => {
    const stat = compile("../tests/passed/entry_token.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('entry_without_effect', async () => {
    const stat = compile("../tests/passed/entry_without_effect.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_all', async () => {
    const stat = compile("../tests/passed/enum_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_key', async () => {
    const stat = compile("../tests/passed/enum_key.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_simple', async () => {
    const stat = compile("../tests/passed/enum_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_with_args', async () => {
    const stat = compile("../tests/passed/enum_with_args.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_with_args_multi', async () => {
    const stat = compile("../tests/passed/enum_with_args_multi.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('enum_without_args', async () => {
    const stat = compile("../tests/passed/enum_without_args.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_all', async () => {
    const stat = compile("../tests/passed/event_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_dup', async () => {
    const stat = compile("../tests/passed/event_dup.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_multi', async () => {
    const stat = compile("../tests/passed/event_multi.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_simple', async () => {
    const stat = compile("../tests/passed/event_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('event_single', async () => {
    const stat = compile("../tests/passed/event_single.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('exec_letin', async () => {
    const stat = compile("../tests/passed/exec_letin.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_access_asset_field', async () => {
    const stat = compile("../tests/passed/expr_access_asset_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_3wc_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_3wc_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_bool_bool', async () => {
    const stat = compile("../tests/passed/expr_arith_and_bool_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_bytes_bytes', async () => {
    const stat = compile("../tests/passed/expr_arith_and_bytes_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_and_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_and_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_and_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_div_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_div_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_div_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_int_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_div_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_div_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_div_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_div_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_rat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_div_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_div_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_div_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_div_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_div_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_divmod_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_divmod_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_divmod_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_divmod_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_tez_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_divmod_tez_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_divmod_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_divmod_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_dur_int', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_dur_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_dur_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_dur_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_tez_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_tez_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_ediv_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_ediv_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsl_bytes_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_lsl_bytes_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsl_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_lsl_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsr_bytes_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_lsr_bytes_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_lsr_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_lsr_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_date_date', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_date_date_neg', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_date_date_neg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_date_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_date_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_int_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_rat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_minus_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_minus_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_mod_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_mod_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_mod_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_mod_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mod_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_mod_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_int_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_int_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_int_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_nat_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_nat_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_nat_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_rat_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_rat_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_rat_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_mult_tez_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_mult_tez_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_bool', async () => {
    const stat = compile("../tests/passed/expr_arith_not_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_bytes', async () => {
    const stat = compile("../tests/passed/expr_arith_not_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_int', async () => {
    const stat = compile("../tests/passed/expr_arith_not_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_not_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_not_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_or_bool_bool', async () => {
    const stat = compile("../tests/passed/expr_arith_or_bool_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_or_bytes_bytes', async () => {
    const stat = compile("../tests/passed/expr_arith_or_bytes_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_or_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_or_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_date_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_date_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_dur_date', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_dur_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_int_int', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_int_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_int_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_nat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_rat_int', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_str_str', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_plus_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_arith_plus_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_uminus_int', async () => {
    const stat = compile("../tests/passed/expr_arith_uminus_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_uminus_rat', async () => {
    const stat = compile("../tests/passed/expr_arith_uminus_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_xor_bool_bool', async () => {
    const stat = compile("../tests/passed/expr_arith_xor_bool_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_xor_bytes_bytes', async () => {
    const stat = compile("../tests/passed/expr_arith_xor_bytes_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_arith_xor_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_arith_xor_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_addr_addr', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_addr_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_bool_bool', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_bool_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_date_date', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_int_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_int_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_int_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_nat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_rat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_str_str', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_eq_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_cmp_eq_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_addr_addr', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_addr_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_date_date', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_int_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_int_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_int_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_nat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_rat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_str_str', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ge_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_cmp_ge_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_addr_addr', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_addr_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_date_date', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_int_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_int_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_int_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_nat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_rat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_str_str', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_gt_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_cmp_gt_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_addr_addr', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_addr_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_date_date', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_int_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_int_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_int_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_nat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_rat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_str_str', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_le_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_cmp_le_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_addr_addr', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_addr_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_date_date', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_int_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_int_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_int_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_nat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_rat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_str_str', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_lt_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_cmp_lt_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_addr_addr', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_addr_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_bool_bool', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_bool_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_date_date', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_date_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_dur_dur', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_dur_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_int_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_int_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_int_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_nat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_rat_int', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_str_str', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_str_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cmp_ne_tez_tez', async () => {
    const stat = compile("../tests/passed/expr_cmp_ne_tez_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_fold', async () => {
    const stat = compile("../tests/passed/expr_control_fold.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_int_int', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_int_nat', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_int_rat', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_nat_int', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_rat_int', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_if_else_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_control_if_else_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_match_list', async () => {
    const stat = compile("../tests/passed/expr_control_match_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_match_option', async () => {
    const stat = compile("../tests/passed/expr_control_match_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_match_or', async () => {
    const stat = compile("../tests/passed/expr_control_match_or.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith', async () => {
    const stat = compile("../tests/passed/expr_control_matchwith.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith_with_int_rat', async () => {
    const stat = compile("../tests/passed/expr_control_matchwith_with_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith_with_nat_int', async () => {
    const stat = compile("../tests/passed/expr_control_matchwith_with_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_control_matchwith_with_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_control_matchwith_with_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_balance', async () => {
    const stat = compile("../tests/passed/expr_cst_balance.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_caller', async () => {
    const stat = compile("../tests/passed/expr_cst_caller.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_level', async () => {
    const stat = compile("../tests/passed/expr_cst_level.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_min_block_time', async () => {
    const stat = compile("../tests/passed/expr_cst_min_block_time.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_now', async () => {
    const stat = compile("../tests/passed/expr_cst_now.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_self_address', async () => {
    const stat = compile("../tests/passed/expr_cst_self_address.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_self_chain_id', async () => {
    const stat = compile("../tests/passed/expr_cst_self_chain_id.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_source', async () => {
    const stat = compile("../tests/passed/expr_cst_source.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_total_voting_power', async () => {
    const stat = compile("../tests/passed/expr_cst_total_voting_power.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_cst_transferred', async () => {
    const stat = compile("../tests/passed/expr_cst_transferred.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fail_some_none', async () => {
    const stat = compile("../tests/passed/expr_fail_some_none.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fail_some_some', async () => {
    const stat = compile("../tests/passed/expr_fail_some_some.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_abs_int', async () => {
    const stat = compile("../tests/passed/expr_fun_abs_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_abs_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_abs_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_abs_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_abs_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_address_to_contract', async () => {
    const stat = compile("../tests/passed/expr_fun_address_to_contract.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_address_to_contract_unit', async () => {
    const stat = compile("../tests/passed/expr_fun_address_to_contract_unit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_bytes_to_int', async () => {
    const stat = compile("../tests/passed/expr_fun_bytes_to_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_bytes_to_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_bytes_to_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_ceil', async () => {
    const stat = compile("../tests/passed/expr_fun_ceil.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_byt', async () => {
    const stat = compile("../tests/passed/expr_fun_concat_byt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_list_byt', async () => {
    const stat = compile("../tests/passed/expr_fun_concat_list_byt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_list_str', async () => {
    const stat = compile("../tests/passed/expr_fun_concat_list_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_concat_str', async () => {
    const stat = compile("../tests/passed/expr_fun_concat_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_exp_horner', async () => {
    const stat = compile("../tests/passed/expr_fun_exp_horner.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_floor', async () => {
    const stat = compile("../tests/passed/expr_fun_floor.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_get_denominator', async () => {
    const stat = compile("../tests/passed/expr_fun_get_denominator.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_get_numerator', async () => {
    const stat = compile("../tests/passed/expr_fun_get_numerator.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_int_to_bytes', async () => {
    const stat = compile("../tests/passed/expr_fun_int_to_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_int_to_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_int_to_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_key_hash_to_contract', async () => {
    const stat = compile("../tests/passed/expr_fun_key_hash_to_contract.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_length_bytes', async () => {
    const stat = compile("../tests/passed/expr_fun_length_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_length_str', async () => {
    const stat = compile("../tests/passed/expr_fun_length_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_make_event', async () => {
    const stat = compile("../tests/passed/expr_fun_make_event.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_make_operation', async () => {
    const stat = compile("../tests/passed/expr_fun_make_operation.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_date', async () => {
    const stat = compile("../tests/passed/expr_fun_max_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_dur', async () => {
    const stat = compile("../tests/passed/expr_fun_max_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_int_int', async () => {
    const stat = compile("../tests/passed/expr_fun_max_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_int_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_max_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_int_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_max_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_nat_int', async () => {
    const stat = compile("../tests/passed/expr_fun_max_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_max_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_max_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_rat_int', async () => {
    const stat = compile("../tests/passed/expr_fun_max_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_max_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_max_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_max_tez', async () => {
    const stat = compile("../tests/passed/expr_fun_max_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_date', async () => {
    const stat = compile("../tests/passed/expr_fun_min_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_dur', async () => {
    const stat = compile("../tests/passed/expr_fun_min_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_int_int', async () => {
    const stat = compile("../tests/passed/expr_fun_min_int_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_int_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_min_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_int_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_min_int_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_nat_int', async () => {
    const stat = compile("../tests/passed/expr_fun_min_nat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_nat_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_min_nat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_nat_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_min_nat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_rat_int', async () => {
    const stat = compile("../tests/passed/expr_fun_min_rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_rat_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_min_rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_rat_rat', async () => {
    const stat = compile("../tests/passed/expr_fun_min_rat_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_min_tez', async () => {
    const stat = compile("../tests/passed/expr_fun_min_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_nat_to_bytes', async () => {
    const stat = compile("../tests/passed/expr_fun_nat_to_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_nat_to_string', async () => {
    const stat = compile("../tests/passed/expr_fun_nat_to_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_get_some', async () => {
    const stat = compile("../tests/passed/expr_fun_opt_get_some.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_is_none', async () => {
    const stat = compile("../tests/passed/expr_fun_opt_is_none.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_is_some', async () => {
    const stat = compile("../tests/passed/expr_fun_opt_is_some.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_opt_require_some', async () => {
    const stat = compile("../tests/passed/expr_fun_opt_require_some.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_pack_complex', async () => {
    const stat = compile("../tests/passed/expr_fun_pack_complex.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_pack_lit_tuple', async () => {
    const stat = compile("../tests/passed/expr_fun_pack_lit_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_pack_string', async () => {
    const stat = compile("../tests/passed/expr_fun_pack_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_setdelegate', async () => {
    const stat = compile("../tests/passed/expr_fun_setdelegate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_simplify_rational', async () => {
    const stat = compile("../tests/passed/expr_fun_simplify_rational.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_slice_byt', async () => {
    const stat = compile("../tests/passed/expr_fun_slice_byt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_slice_str', async () => {
    const stat = compile("../tests/passed/expr_fun_slice_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_sub_mutez', async () => {
    const stat = compile("../tests/passed/expr_fun_sub_mutez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_sub_nat', async () => {
    const stat = compile("../tests/passed/expr_fun_sub_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_sub_nat_zero', async () => {
    const stat = compile("../tests/passed/expr_fun_sub_nat_zero.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_unpack_bool', async () => {
    const stat = compile("../tests/passed/expr_fun_unpack_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_unpack_complex', async () => {
    const stat = compile("../tests/passed/expr_fun_unpack_complex.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_fun_unpack_string', async () => {
    const stat = compile("../tests/passed/expr_fun_unpack_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_1_0', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_1_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_2_0', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_2_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_2_1', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_2_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_3_0', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_3_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_3_1', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_3_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_3_2', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_3_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_0', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_4_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_1', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_4_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_2', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_4_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_4_3', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_4_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_instr_rec_rollback', async () => {
    const stat = compile("../tests/passed/expr_instr_rec_rollback.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lambda', async () => {
    const stat = compile("../tests/passed/expr_lambda.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lambda2', async () => {
    const stat = compile("../tests/passed/expr_lambda2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_concat', async () => {
    const stat = compile("../tests/passed/expr_list_concat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_contains', async () => {
    const stat = compile("../tests/passed/expr_list_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_head', async () => {
    const stat = compile("../tests/passed/expr_list_head.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_length', async () => {
    const stat = compile("../tests/passed/expr_list_length.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_lit', async () => {
    const stat = compile("../tests/passed/expr_list_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_map', async () => {
    const stat = compile("../tests/passed/expr_list_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_map_string_nat', async () => {
    const stat = compile("../tests/passed/expr_list_map_string_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_nth', async () => {
    const stat = compile("../tests/passed/expr_list_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_prepend', async () => {
    const stat = compile("../tests/passed/expr_list_prepend.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_reverse', async () => {
    const stat = compile("../tests/passed/expr_list_reverse.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_list_tail', async () => {
    const stat = compile("../tests/passed/expr_list_tail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_addr', async () => {
    const stat = compile("../tests/passed/expr_lit_addr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_bytes', async () => {
    const stat = compile("../tests/passed/expr_lit_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_cur_mtz', async () => {
    const stat = compile("../tests/passed/expr_lit_cur_mtz.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_cur_tz', async () => {
    const stat = compile("../tests/passed/expr_lit_cur_tz.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_cur_utz', async () => {
    const stat = compile("../tests/passed/expr_lit_cur_utz.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_0', async () => {
    const stat = compile("../tests/passed/expr_lit_date_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_1', async () => {
    const stat = compile("../tests/passed/expr_lit_date_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_2', async () => {
    const stat = compile("../tests/passed/expr_lit_date_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_3', async () => {
    const stat = compile("../tests/passed/expr_lit_date_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_date_4', async () => {
    const stat = compile("../tests/passed/expr_lit_date_4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_dur', async () => {
    const stat = compile("../tests/passed/expr_lit_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_int', async () => {
    const stat = compile("../tests/passed/expr_lit_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_int_neg', async () => {
    const stat = compile("../tests/passed/expr_lit_int_neg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_nat', async () => {
    const stat = compile("../tests/passed/expr_lit_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_opt_none', async () => {
    const stat = compile("../tests/passed/expr_lit_opt_none.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_opt_some', async () => {
    const stat = compile("../tests/passed/expr_lit_opt_some.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_or_left', async () => {
    const stat = compile("../tests/passed/expr_lit_or_left.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_or_right', async () => {
    const stat = compile("../tests/passed/expr_lit_or_right.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_dec', async () => {
    const stat = compile("../tests/passed/expr_lit_rat_dec.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_dec_neg', async () => {
    const stat = compile("../tests/passed/expr_lit_rat_dec_neg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_div', async () => {
    const stat = compile("../tests/passed/expr_lit_rat_div.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_rat_div_neg', async () => {
    const stat = compile("../tests/passed/expr_lit_rat_div_neg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_str', async () => {
    const stat = compile("../tests/passed/expr_lit_str.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_tuple', async () => {
    const stat = compile("../tests/passed/expr_lit_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_lit_unit', async () => {
    const stat = compile("../tests/passed/expr_lit_unit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_big_map', async () => {
    const stat = compile("../tests/passed/expr_make_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_big_map_empty', async () => {
    const stat = compile("../tests/passed/expr_make_big_map_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_list', async () => {
    const stat = compile("../tests/passed/expr_make_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_list_empty', async () => {
    const stat = compile("../tests/passed/expr_make_list_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_map', async () => {
    const stat = compile("../tests/passed/expr_make_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_map_empty', async () => {
    const stat = compile("../tests/passed/expr_make_map_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_set', async () => {
    const stat = compile("../tests/passed/expr_make_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_make_set_empty', async () => {
    const stat = compile("../tests/passed/expr_make_set_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_contains', async () => {
    const stat = compile("../tests/passed/expr_map_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_get', async () => {
    const stat = compile("../tests/passed/expr_map_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_length', async () => {
    const stat = compile("../tests/passed/expr_map_length.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_lit', async () => {
    const stat = compile("../tests/passed/expr_map_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_map', async () => {
    const stat = compile("../tests/passed/expr_map_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_put', async () => {
    const stat = compile("../tests/passed/expr_map_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_remove', async () => {
    const stat = compile("../tests/passed/expr_map_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_map_update', async () => {
    const stat = compile("../tests/passed/expr_map_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains', async () => {
    const stat = compile("../tests/passed/expr_method_asset_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_contains_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_contains_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_contains_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_contains_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_contains_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count', async () => {
    const stat = compile("../tests/passed/expr_method_asset_count.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_count_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_count_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_count_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_count_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_count_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_get', async () => {
    const stat = compile("../tests/passed/expr_method_asset_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head', async () => {
    const stat = compile("../tests/passed/expr_method_asset_head.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_head_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_head_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_head_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_head_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_head_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth', async () => {
    const stat = compile("../tests/passed/expr_method_asset_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_nth_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_nth_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_nth_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_nth_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_nth_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select', async () => {
    const stat = compile("../tests/passed/expr_method_asset_select.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_select_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_select_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_select_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_select_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_select_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sort.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sort_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sort_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sort_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sort_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sort_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sum_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sum_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sum_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_rational', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sum_rational.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_sum_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_sum_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail', async () => {
    const stat = compile("../tests/passed/expr_method_asset_tail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_aggregate', async () => {
    const stat = compile("../tests/passed/expr_method_asset_tail_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_one_field', async () => {
    const stat = compile("../tests/passed/expr_method_asset_tail_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_partition', async () => {
    const stat = compile("../tests/passed/expr_method_asset_tail_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_method_asset_tail_view', async () => {
    const stat = compile("../tests/passed/expr_method_asset_tail_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_multicmp', async () => {
    const stat = compile("../tests/passed/expr_multicmp.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_option_map', async () => {
    const stat = compile("../tests/passed/expr_option_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_record_lit', async () => {
    const stat = compile("../tests/passed/expr_record_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_record_update_record_in_exec', async () => {
    const stat = compile("../tests/passed/expr_record_update_record_in_exec.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_add', async () => {
    const stat = compile("../tests/passed/expr_set_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_contains', async () => {
    const stat = compile("../tests/passed/expr_set_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_length', async () => {
    const stat = compile("../tests/passed/expr_set_length.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_lit', async () => {
    const stat = compile("../tests/passed/expr_set_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_remove', async () => {
    const stat = compile("../tests/passed/expr_set_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_set_update', async () => {
    const stat = compile("../tests/passed/expr_set_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_tuple_access', async () => {
    const stat = compile("../tests/passed/expr_tuple_access.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_tuple_access_simple', async () => {
    const stat = compile("../tests/passed/expr_tuple_access_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_list_empty', async () => {
    const stat = compile("../tests/passed/expr_var_match_list_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_list_head', async () => {
    const stat = compile("../tests/passed/expr_var_match_list_head.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_option_none', async () => {
    const stat = compile("../tests/passed/expr_var_match_option_none.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_option_some', async () => {
    const stat = compile("../tests/passed/expr_var_match_option_some.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_or_left', async () => {
    const stat = compile("../tests/passed/expr_var_match_or_left.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('expr_var_match_or_right', async () => {
    const stat = compile("../tests/passed/expr_var_match_or_right.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fa12_false', async () => {
    const stat = compile("../tests/passed/fa12_false.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fa12_simple', async () => {
    const stat = compile("../tests/passed/fa12_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_', async () => {
    const stat = compile("../tests/passed/fail_.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_for', async () => {
    const stat = compile("../tests/passed/fail_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_if', async () => {
    const stat = compile("../tests/passed/fail_if.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_match_list', async () => {
    const stat = compile("../tests/passed/fail_match_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_match_option', async () => {
    const stat = compile("../tests/passed/fail_match_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_while', async () => {
    const stat = compile("../tests/passed/fail_while.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fail_with_tuple_lit', async () => {
    const stat = compile("../tests/passed/fail_with_tuple_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fold_reverse', async () => {
    const stat = compile("../tests/passed/fold_reverse.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun', async () => {
    const stat = compile("../tests/passed/fun.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_pure', async () => {
    const stat = compile("../tests/passed/fun_entry_pure.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_read', async () => {
    const stat = compile("../tests/passed/fun_entry_read.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_write', async () => {
    const stat = compile("../tests/passed/fun_entry_write.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_entry_write_with_transfer', async () => {
    const stat = compile("../tests/passed/fun_entry_write_with_transfer.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_instr_unit', async () => {
    const stat = compile("../tests/passed/fun_instr_unit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_instr_unit_arg', async () => {
    const stat = compile("../tests/passed/fun_instr_unit_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_instr_unit_arg_side_effect', async () => {
    const stat = compile("../tests/passed/fun_instr_unit_arg_side_effect.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_unit', async () => {
    const stat = compile("../tests/passed/fun_unit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_instr_pure', async () => {
    const stat = compile("../tests/passed/fun_view_instr_pure.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_pure', async () => {
    const stat = compile("../tests/passed/fun_view_pure.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_read', async () => {
    const stat = compile("../tests/passed/fun_view_read.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('fun_view_read_asset', async () => {
    const stat = compile("../tests/passed/fun_view_read_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('function_with_nat_to_string', async () => {
    const stat = compile("../tests/passed/function_with_nat_to_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('function_with_simplify_rational', async () => {
    const stat = compile("../tests/passed/function_with_simplify_rational.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('get_in_require_failif', async () => {
    const stat = compile("../tests/passed/get_in_require_failif.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('get_some_with_msg', async () => {
    const stat = compile("../tests/passed/get_some_with_msg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('getter_called_by', async () => {
    const stat = compile("../tests/passed/getter_called_by.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('greedy_and', async () => {
    const stat = compile("../tests/passed/greedy_and.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('greedy_or', async () => {
    const stat = compile("../tests/passed/greedy_or.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('implicit_cast_to_view', async () => {
    const stat = compile("../tests/passed/implicit_cast_to_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('implicit_the', async () => {
    const stat = compile("../tests/passed/implicit_the.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_all_def', async () => {
    const stat = compile("../tests/passed/import_arl_all_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_container_use_arg', async () => {
    const stat = compile("../tests/passed/import_arl_asset_container_use_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_container_use_arg_collide', async () => {
    const stat = compile("../tests/passed/import_arl_asset_container_use_arg_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_def', async () => {
    const stat = compile("../tests/passed/import_arl_asset_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_key_use_arg', async () => {
    const stat = compile("../tests/passed/import_arl_asset_key_use_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_key_use_arg_collide', async () => {
    const stat = compile("../tests/passed/import_arl_asset_key_use_arg_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_use_all', async () => {
    const stat = compile("../tests/passed/import_arl_asset_use_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_value_use_arg', async () => {
    const stat = compile("../tests/passed/import_arl_asset_value_use_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_value_use_arg_collide', async () => {
    const stat = compile("../tests/passed/import_arl_asset_value_use_arg_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_view_use_arg', async () => {
    const stat = compile("../tests/passed/import_arl_asset_view_use_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_asset_view_use_arg_collide', async () => {
    const stat = compile("../tests/passed/import_arl_asset_view_use_arg_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_def', async () => {
    const stat = compile("../tests/passed/import_arl_constant_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_use', async () => {
    const stat = compile("../tests/passed/import_arl_constant_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_use_all', async () => {
    const stat = compile("../tests/passed/import_arl_constant_use_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_constant_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_constant_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entry_record_use_all', async () => {
    const stat = compile("../tests/passed/import_arl_entry_record_use_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entrypoint_def', async () => {
    const stat = compile("../tests/passed/import_arl_entrypoint_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entrypoint_use', async () => {
    const stat = compile("../tests/passed/import_arl_entrypoint_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_entrypoint_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_entrypoint_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_def', async () => {
    const stat = compile("../tests/passed/import_arl_enum_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use', async () => {
    const stat = compile("../tests/passed/import_arl_enum_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use_all', async () => {
    const stat = compile("../tests/passed/import_arl_enum_use_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_enum_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_use_complete', async () => {
    const stat = compile("../tests/passed/import_arl_enum_use_complete.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_def', async () => {
    const stat = compile("../tests/passed/import_arl_enum_with_args_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_use', async () => {
    const stat = compile("../tests/passed/import_arl_enum_with_args_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_enum_with_args_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_enum_with_args_use_complete', async () => {
    const stat = compile("../tests/passed/import_arl_enum_with_args_use_complete.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_event_def', async () => {
    const stat = compile("../tests/passed/import_arl_event_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_event_use', async () => {
    const stat = compile("../tests/passed/import_arl_event_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_event_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_event_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_2_pure_use', async () => {
    const stat = compile("../tests/passed/import_arl_fun_2_pure_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_def', async () => {
    const stat = compile("../tests/passed/import_arl_fun_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_def_pure', async () => {
    const stat = compile("../tests/passed/import_arl_fun_def_pure.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_instr_pure_use', async () => {
    const stat = compile("../tests/passed/import_arl_fun_instr_pure_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_fun_pure_use', async () => {
    const stat = compile("../tests/passed/import_arl_fun_pure_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_complex_0', async () => {
    const stat = compile("../tests/passed/import_arl_record_complex_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_complex_1', async () => {
    const stat = compile("../tests/passed/import_arl_record_complex_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_complex_2', async () => {
    const stat = compile("../tests/passed/import_arl_record_complex_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_def', async () => {
    const stat = compile("../tests/passed/import_arl_record_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use', async () => {
    const stat = compile("../tests/passed/import_arl_record_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use_all', async () => {
    const stat = compile("../tests/passed/import_arl_record_use_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_record_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_record_use_complete', async () => {
    const stat = compile("../tests/passed/import_arl_record_use_complete.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_transfer_use', async () => {
    const stat = compile("../tests/passed/import_arl_transfer_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_def', async () => {
    const stat = compile("../tests/passed/import_arl_view_def.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_use', async () => {
    const stat = compile("../tests/passed/import_arl_view_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_use_all', async () => {
    const stat = compile("../tests/passed/import_arl_view_use_all.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_arl_view_use_collide', async () => {
    const stat = compile("../tests/passed/import_arl_view_use_collide.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_tz_entry_use', async () => {
    const stat = compile("../tests/passed/import_tz_entry_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('import_tz_view_use', async () => {
    const stat = compile("../tests/passed/import_tz_view_use.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('init_lambda', async () => {
    const stat = compile("../tests/passed/init_lambda.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_list_prepend', async () => {
    const stat = compile("../tests/passed/instr_list_prepend.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_put', async () => {
    const stat = compile("../tests/passed/instr_map_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_remove', async () => {
    const stat = compile("../tests/passed/instr_map_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_local_record', async () => {
    const stat = compile("../tests/passed/instr_map_update_local_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_local_var', async () => {
    const stat = compile("../tests/passed/instr_map_update_local_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_storage_record', async () => {
    const stat = compile("../tests/passed/instr_map_update_storage_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_map_update_storage_var', async () => {
    const stat = compile("../tests/passed/instr_map_update_storage_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_add', async () => {
    const stat = compile("../tests/passed/instr_set_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_remove', async () => {
    const stat = compile("../tests/passed/instr_set_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_update_add', async () => {
    const stat = compile("../tests/passed/instr_set_update_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('instr_set_update_remove', async () => {
    const stat = compile("../tests/passed/instr_set_update_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('int_to_date', async () => {
    const stat = compile("../tests/passed/int_to_date.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('invariants_on_states', async () => {
    const stat = compile("../tests/passed/invariants_on_states.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('invariants_on_variable', async () => {
    const stat = compile("../tests/passed/invariants_on_variable.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iter_list_ticket', async () => {
    const stat = compile("../tests/passed/iter_list_ticket.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_assign', async () => {
    const stat = compile("../tests/passed/iterable_big_map_assign.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_contains', async () => {
    const stat = compile("../tests/passed/iterable_big_map_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_for', async () => {
    const stat = compile("../tests/passed/iterable_big_map_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_get', async () => {
    const stat = compile("../tests/passed/iterable_big_map_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_length', async () => {
    const stat = compile("../tests/passed/iterable_big_map_length.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_put', async () => {
    const stat = compile("../tests/passed/iterable_big_map_put.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_remove', async () => {
    const stat = compile("../tests/passed/iterable_big_map_remove.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_storage_decl', async () => {
    const stat = compile("../tests/passed/iterable_big_map_storage_decl.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('iterable_big_map_test', async () => {
    const stat = compile("../tests/passed/iterable_big_map_test.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('key_to_address', async () => {
    const stat = compile("../tests/passed/key_to_address.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_arith', async () => {
    const stat = compile("../tests/passed/lang_arith.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_asset', async () => {
    const stat = compile("../tests/passed/lang_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_assign', async () => {
    const stat = compile("../tests/passed/lang_assign.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_big_map', async () => {
    const stat = compile("../tests/passed/lang_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_cast', async () => {
    const stat = compile("../tests/passed/lang_cast.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_cmp', async () => {
    const stat = compile("../tests/passed/lang_cmp.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_contract', async () => {
    const stat = compile("../tests/passed/lang_contract.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_crypto', async () => {
    const stat = compile("../tests/passed/lang_crypto.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_cst', async () => {
    const stat = compile("../tests/passed/lang_cst.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_entry', async () => {
    const stat = compile("../tests/passed/lang_entry.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_enum', async () => {
    const stat = compile("../tests/passed/lang_enum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_funs', async () => {
    const stat = compile("../tests/passed/lang_funs.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_list', async () => {
    const stat = compile("../tests/passed/lang_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_literals', async () => {
    const stat = compile("../tests/passed/lang_literals.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_map', async () => {
    const stat = compile("../tests/passed/lang_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_methods_asset', async () => {
    const stat = compile("../tests/passed/lang_methods_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lang_set', async () => {
    const stat = compile("../tests/passed/lang_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('large_if', async () => {
    const stat = compile("../tests/passed/large_if.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_list', async () => {
    const stat = compile("../tests/passed/list_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_nth_out_of_bound', async () => {
    const stat = compile("../tests/passed/list_nth_out_of_bound.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_option', async () => {
    const stat = compile("../tests/passed/list_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('list_or', async () => {
    const stat = compile("../tests/passed/list_or.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_list', async () => {
    const stat = compile("../tests/passed/lit_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_map', async () => {
    const stat = compile("../tests/passed/lit_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_set', async () => {
    const stat = compile("../tests/passed/lit_set.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('lit_tez_underscore', async () => {
    const stat = compile("../tests/passed/lit_tez_underscore.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('literal_in_argument', async () => {
    const stat = compile("../tests/passed/literal_in_argument.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('map_asset', async () => {
    const stat = compile("../tests/passed/map_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_detach_big_map', async () => {
    const stat = compile("../tests/passed/match_detach_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_detach_map', async () => {
    const stat = compile("../tests/passed/match_detach_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_detach_option', async () => {
    const stat = compile("../tests/passed/match_detach_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('match_entrypoint', async () => {
    const stat = compile("../tests/passed/match_entrypoint.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('max_tez', async () => {
    const stat = compile("../tests/passed/max_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('method_in_dorequire_or_dofailif', async () => {
    const stat = compile("../tests/passed/method_in_dorequire_or_dofailif.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('michelson_expression', async () => {
    const stat = compile("../tests/passed/michelson_expression.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('michelson_instruction', async () => {
    const stat = compile("../tests/passed/michelson_instruction.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('michelson_lambda', async () => {
    const stat = compile("../tests/passed/michelson_lambda.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('miles_with_expiration_spec', async () => {
    const stat = compile("../tests/passed/miles_with_expiration_spec.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('mod_rat', async () => {
    const stat = compile("../tests/passed/mod_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_e', async () => {
    const stat = compile("../tests/passed/multi_e.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_p', async () => {
    const stat = compile("../tests/passed/multi_p.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_sort', async () => {
    const stat = compile("../tests/passed/multi_sort.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_update', async () => {
    const stat = compile("../tests/passed/multi_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multi_var_storage', async () => {
    const stat = compile("../tests/passed/multi_var_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multivars', async () => {
    const stat = compile("../tests/passed/multivars.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multivars1', async () => {
    const stat = compile("../tests/passed/multivars1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('multivars_simple', async () => {
    const stat = compile("../tests/passed/multivars_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('mutez_to_nat', async () => {
    const stat = compile("../tests/passed/mutez_to_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nat_to_string', async () => {
    const stat = compile("../tests/passed/nat_to_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nat_to_string_2', async () => {
    const stat = compile("../tests/passed/nat_to_string_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nested_for', async () => {
    const stat = compile("../tests/passed/nested_for.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nested_if_return', async () => {
    const stat = compile("../tests/passed/nested_if_return.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('no_entrypoint', async () => {
    const stat = compile("../tests/passed/no_entrypoint.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('not_int', async () => {
    const stat = compile("../tests/passed/not_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('not_nat', async () => {
    const stat = compile("../tests/passed/not_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('nothing', async () => {
    const stat = compile("../tests/passed/nothing.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('one_constant', async () => {
    const stat = compile("../tests/passed/one_constant.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('op_assign_rat_update_asset', async () => {
    const stat = compile("../tests/passed/op_assign_rat_update_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('param_const', async () => {
    const stat = compile("../tests/passed/param_const.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('parameter_expr_map', async () => {
    const stat = compile("../tests/passed/parameter_expr_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('partial_record', async () => {
    const stat = compile("../tests/passed/partial_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_div', async () => {
    const stat = compile("../tests/passed/rat_arith_div.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_minus', async () => {
    const stat = compile("../tests/passed/rat_arith_minus.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_mult', async () => {
    const stat = compile("../tests/passed/rat_arith_mult.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_plus', async () => {
    const stat = compile("../tests/passed/rat_arith_plus.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_arith_uminus', async () => {
    const stat = compile("../tests/passed/rat_arith_uminus.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_eq', async () => {
    const stat = compile("../tests/passed/rat_cmp_eq.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_ge', async () => {
    const stat = compile("../tests/passed/rat_cmp_ge.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_gt', async () => {
    const stat = compile("../tests/passed/rat_cmp_gt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_le', async () => {
    const stat = compile("../tests/passed/rat_cmp_le.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_cmp_lt', async () => {
    const stat = compile("../tests/passed/rat_cmp_lt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_dur', async () => {
    const stat = compile("../tests/passed/rat_dur.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_int', async () => {
    const stat = compile("../tests/passed/rat_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_max', async () => {
    const stat = compile("../tests/passed/rat_max.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_min', async () => {
    const stat = compile("../tests/passed/rat_min.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_nat', async () => {
    const stat = compile("../tests/passed/rat_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_neg', async () => {
    const stat = compile("../tests/passed/rat_neg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_tez', async () => {
    const stat = compile("../tests/passed/rat_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rat_tez_big', async () => {
    const stat = compile("../tests/passed/rat_tez_big.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_cmp', async () => {
    const stat = compile("../tests/passed/rational_cmp.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_duration', async () => {
    const stat = compile("../tests/passed/rational_duration.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_full', async () => {
    const stat = compile("../tests/passed/rational_full.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_rat_tez_mult', async () => {
    const stat = compile("../tests/passed/rational_rat_tez_mult.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_simple', async () => {
    const stat = compile("../tests/passed/rational_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rational_tez_rat_mult', async () => {
    const stat = compile("../tests/passed/rational_tez_rat_mult.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rec_update', async () => {
    const stat = compile("../tests/passed/rec_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rec_update2', async () => {
    const stat = compile("../tests/passed/rec_update2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_access', async () => {
    const stat = compile("../tests/passed/record_access.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_access2', async () => {
    const stat = compile("../tests/passed/record_access2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_container', async () => {
    const stat = compile("../tests/passed/record_container.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_double_key', async () => {
    const stat = compile("../tests/passed/record_double_key.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_in_enum', async () => {
    const stat = compile("../tests/passed/record_in_enum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('record_update', async () => {
    const stat = compile("../tests/passed/record_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('remove_asset_with_partition', async () => {
    const stat = compile("../tests/passed/remove_asset_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('reverse_otherwise', async () => {
    const stat = compile("../tests/passed/reverse_otherwise.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('reverse_with_enum', async () => {
    const stat = compile("../tests/passed/reverse_with_enum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rf_failif_with', async () => {
    const stat = compile("../tests/passed/rf_failif_with.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('rf_require_otherwise', async () => {
    const stat = compile("../tests/passed/rf_require_otherwise.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('same_varname_in_two_distinct_scope', async () => {
    const stat = compile("../tests/passed/same_varname_in_two_distinct_scope.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sample_asset_view', async () => {
    const stat = compile("../tests/passed/sample_asset_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sample_view_asset_value', async () => {
    const stat = compile("../tests/passed/sample_view_asset_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sapling_empty_state', async () => {
    const stat = compile("../tests/passed/sapling_empty_state.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sapling_var', async () => {
    const stat = compile("../tests/passed/sapling_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sapling_verify_update', async () => {
    const stat = compile("../tests/passed/sapling_verify_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('section_constant_effect', async () => {
    const stat = compile("../tests/passed/section_constant_effect.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('section_constant_transition', async () => {
    const stat = compile("../tests/passed/section_constant_transition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_partition', async () => {
    const stat = compile("../tests/passed/select_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_partition_big_map', async () => {
    const stat = compile("../tests/passed/select_partition_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_with_extra_var', async () => {
    const stat = compile("../tests/passed/select_with_extra_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_with_extra_var2', async () => {
    const stat = compile("../tests/passed/select_with_extra_var2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('select_with_function_in_predicate', async () => {
    const stat = compile("../tests/passed/select_with_function_in_predicate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('setdelegate', async () => {
    const stat = compile("../tests/passed/setdelegate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple', async () => {
    const stat = compile("../tests/passed/simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple2', async () => {
    const stat = compile("../tests/passed/simple2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple3', async () => {
    const stat = compile("../tests/passed/simple3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple4', async () => {
    const stat = compile("../tests/passed/simple4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_2vars', async () => {
    const stat = compile("../tests/passed/simple_2vars.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_addupdate', async () => {
    const stat = compile("../tests/passed/simple_addupdate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_addupdate_asset', async () => {
    const stat = compile("../tests/passed/simple_addupdate_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_arg_int', async () => {
    const stat = compile("../tests/passed/simple_arg_int.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_arith', async () => {
    const stat = compile("../tests/passed/simple_arith.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset', async () => {
    const stat = compile("../tests/passed/simple_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_2', async () => {
    const stat = compile("../tests/passed/simple_asset_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_get_asset1_value', async () => {
    const stat = compile("../tests/passed/simple_asset_get_asset1_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_get_asset2_value', async () => {
    const stat = compile("../tests/passed/simple_asset_get_asset2_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_get_asset2_value2', async () => {
    const stat = compile("../tests/passed/simple_asset_get_asset2_value2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_one_field', async () => {
    const stat = compile("../tests/passed/simple_asset_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip', async () => {
    const stat = compile("../tests/passed/simple_asset_skip.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip_empty', async () => {
    const stat = compile("../tests/passed/simple_asset_skip_empty.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip_empty_one_field', async () => {
    const stat = compile("../tests/passed/simple_asset_skip_empty_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_asset_skip_one_field', async () => {
    const stat = compile("../tests/passed/simple_asset_skip_one_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_assign1', async () => {
    const stat = compile("../tests/passed/simple_assign1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_assign2', async () => {
    const stat = compile("../tests/passed/simple_assign2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_assign3', async () => {
    const stat = compile("../tests/passed/simple_assign3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_contract_call', async () => {
    const stat = compile("../tests/passed/simple_contract_call.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_freeze', async () => {
    const stat = compile("../tests/passed/simple_freeze.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun1', async () => {
    const stat = compile("../tests/passed/simple_fun1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun2', async () => {
    const stat = compile("../tests/passed/simple_fun2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun3', async () => {
    const stat = compile("../tests/passed/simple_fun3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun4', async () => {
    const stat = compile("../tests/passed/simple_fun4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun5', async () => {
    const stat = compile("../tests/passed/simple_fun5.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun6', async () => {
    const stat = compile("../tests/passed/simple_fun6.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun7', async () => {
    const stat = compile("../tests/passed/simple_fun7.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun8', async () => {
    const stat = compile("../tests/passed/simple_fun8.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_alt', async () => {
    const stat = compile("../tests/passed/simple_fun_alt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_with_storage', async () => {
    const stat = compile("../tests/passed/simple_fun_with_storage.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_with_storage2', async () => {
    const stat = compile("../tests/passed/simple_fun_with_storage2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_fun_with_storage3', async () => {
    const stat = compile("../tests/passed/simple_fun_with_storage3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_get_field', async () => {
    const stat = compile("../tests/passed/simple_get_field.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_if3', async () => {
    const stat = compile("../tests/passed/simple_if3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_multi_entry', async () => {
    const stat = compile("../tests/passed/simple_multi_entry.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_multi_entry2', async () => {
    const stat = compile("../tests/passed/simple_multi_entry2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_multi_entry3', async () => {
    const stat = compile("../tests/passed/simple_multi_entry3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_op_add', async () => {
    const stat = compile("../tests/passed/simple_op_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_op_uminus', async () => {
    const stat = compile("../tests/passed/simple_op_uminus.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_param', async () => {
    const stat = compile("../tests/passed/simple_param.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_param_const', async () => {
    const stat = compile("../tests/passed/simple_param_const.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_param_with_default', async () => {
    const stat = compile("../tests/passed/simple_param_with_default.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_assign', async () => {
    const stat = compile("../tests/passed/simple_record_assign.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_assign1', async () => {
    const stat = compile("../tests/passed/simple_record_assign1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_assign2', async () => {
    const stat = compile("../tests/passed/simple_record_assign2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_lit', async () => {
    const stat = compile("../tests/passed/simple_record_lit.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_record_lit_rev', async () => {
    const stat = compile("../tests/passed/simple_record_lit_rev.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_reverse', async () => {
    const stat = compile("../tests/passed/simple_reverse.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence', async () => {
    const stat = compile("../tests/passed/simple_sequence.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence_with_arg', async () => {
    const stat = compile("../tests/passed/simple_sequence_with_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence_with_arg2', async () => {
    const stat = compile("../tests/passed/simple_sequence_with_arg2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_sequence_with_arg_var', async () => {
    const stat = compile("../tests/passed/simple_sequence_with_arg_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_string', async () => {
    const stat = compile("../tests/passed/simple_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_while', async () => {
    const stat = compile("../tests/passed/simple_while.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_with_arg_view', async () => {
    const stat = compile("../tests/passed/simple_with_arg_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_with_type_annot', async () => {
    const stat = compile("../tests/passed/simple_with_type_annot.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('simple_with_view', async () => {
    const stat = compile("../tests/passed/simple_with_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('sourced_by', async () => {
    const stat = compile("../tests/passed/sourced_by.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('state_in_effect', async () => {
    const stat = compile("../tests/passed/state_in_effect.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('state_is', async () => {
    const stat = compile("../tests/passed/state_is.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('state_var', async () => {
    const stat = compile("../tests/passed/state_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_bool_false', async () => {
    const stat = compile("../tests/passed/tern_bool_false.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_bool_true', async () => {
    const stat = compile("../tests/passed/tern_bool_true.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_opt', async () => {
    const stat = compile("../tests/passed/tern_opt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tern_opt_3', async () => {
    const stat = compile("../tests/passed/tern_opt_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset2_with_partition', async () => {
    const stat = compile("../tests/passed/test_add_asset2_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset_with_aggregate', async () => {
    const stat = compile("../tests/passed/test_add_asset_with_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset_with_both', async () => {
    const stat = compile("../tests/passed/test_add_asset_with_both.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_add_asset_with_partition', async () => {
    const stat = compile("../tests/passed/test_add_asset_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_aggregate_1', async () => {
    const stat = compile("../tests/passed/test_addfield_aggregate_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_aggregate_2', async () => {
    const stat = compile("../tests/passed/test_addfield_aggregate_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_partition_1', async () => {
    const stat = compile("../tests/passed/test_addfield_partition_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addfield_partition_2', async () => {
    const stat = compile("../tests/passed/test_addfield_partition_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addupdate_0', async () => {
    const stat = compile("../tests/passed/test_addupdate_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addupdate_1', async () => {
    const stat = compile("../tests/passed/test_addupdate_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_addupdate_2', async () => {
    const stat = compile("../tests/passed/test_addupdate_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset', async () => {
    const stat = compile("../tests/passed/test_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_agg_0', async () => {
    const stat = compile("../tests/passed/test_asset_head_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_agg_1', async () => {
    const stat = compile("../tests/passed/test_asset_head_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_agg_2', async () => {
    const stat = compile("../tests/passed/test_asset_head_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_coll_0', async () => {
    const stat = compile("../tests/passed/test_asset_head_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_coll_1', async () => {
    const stat = compile("../tests/passed/test_asset_head_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_coll_2', async () => {
    const stat = compile("../tests/passed/test_asset_head_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_view_0', async () => {
    const stat = compile("../tests/passed/test_asset_head_view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_view_1', async () => {
    const stat = compile("../tests/passed/test_asset_head_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_head_view_2', async () => {
    const stat = compile("../tests/passed/test_asset_head_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_multi_key', async () => {
    const stat = compile("../tests/passed/test_asset_multi_key.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_multi_key_complex', async () => {
    const stat = compile("../tests/passed/test_asset_multi_key_complex.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_agg_0', async () => {
    const stat = compile("../tests/passed/test_asset_nth_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_agg_1', async () => {
    const stat = compile("../tests/passed/test_asset_nth_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_agg_2', async () => {
    const stat = compile("../tests/passed/test_asset_nth_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_coll_0', async () => {
    const stat = compile("../tests/passed/test_asset_nth_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_coll_1', async () => {
    const stat = compile("../tests/passed/test_asset_nth_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_coll_2', async () => {
    const stat = compile("../tests/passed/test_asset_nth_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_view_0', async () => {
    const stat = compile("../tests/passed/test_asset_nth_view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_view_1', async () => {
    const stat = compile("../tests/passed/test_asset_nth_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_nth_view_2', async () => {
    const stat = compile("../tests/passed/test_asset_nth_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_agg_0', async () => {
    const stat = compile("../tests/passed/test_asset_select_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_agg_1', async () => {
    const stat = compile("../tests/passed/test_asset_select_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_agg_2', async () => {
    const stat = compile("../tests/passed/test_asset_select_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_coll_0', async () => {
    const stat = compile("../tests/passed/test_asset_select_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_coll_1', async () => {
    const stat = compile("../tests/passed/test_asset_select_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_coll_2', async () => {
    const stat = compile("../tests/passed/test_asset_select_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_view_0', async () => {
    const stat = compile("../tests/passed/test_asset_select_view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_view_1', async () => {
    const stat = compile("../tests/passed/test_asset_select_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_select_view_2', async () => {
    const stat = compile("../tests/passed/test_asset_select_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_agg_0', async () => {
    const stat = compile("../tests/passed/test_asset_sort_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_agg_1', async () => {
    const stat = compile("../tests/passed/test_asset_sort_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_agg_2', async () => {
    const stat = compile("../tests/passed/test_asset_sort_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_0', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_1', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_2', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_complex', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_complex.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_random', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_random.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_random2', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_random2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_coll_rational', async () => {
    const stat = compile("../tests/passed/test_asset_sort_coll_rational.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_view_0', async () => {
    const stat = compile("../tests/passed/test_asset_sort_view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_view_1', async () => {
    const stat = compile("../tests/passed/test_asset_sort_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sort_view_2', async () => {
    const stat = compile("../tests/passed/test_asset_sort_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_agg_0', async () => {
    const stat = compile("../tests/passed/test_asset_sum_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_agg_1', async () => {
    const stat = compile("../tests/passed/test_asset_sum_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_agg_2', async () => {
    const stat = compile("../tests/passed/test_asset_sum_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_0', async () => {
    const stat = compile("../tests/passed/test_asset_sum_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_1', async () => {
    const stat = compile("../tests/passed/test_asset_sum_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_2', async () => {
    const stat = compile("../tests/passed/test_asset_sum_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_coll_rat', async () => {
    const stat = compile("../tests/passed/test_asset_sum_coll_rat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_view_0', async () => {
    const stat = compile("../tests/passed/test_asset_sum_view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_view_1', async () => {
    const stat = compile("../tests/passed/test_asset_sum_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_sum_view_2', async () => {
    const stat = compile("../tests/passed/test_asset_sum_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_agg_0', async () => {
    const stat = compile("../tests/passed/test_asset_tail_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_agg_1', async () => {
    const stat = compile("../tests/passed/test_asset_tail_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_agg_2', async () => {
    const stat = compile("../tests/passed/test_asset_tail_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_coll_0', async () => {
    const stat = compile("../tests/passed/test_asset_tail_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_coll_1', async () => {
    const stat = compile("../tests/passed/test_asset_tail_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_coll_2', async () => {
    const stat = compile("../tests/passed/test_asset_tail_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_view_0', async () => {
    const stat = compile("../tests/passed/test_asset_tail_view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_view_1', async () => {
    const stat = compile("../tests/passed/test_asset_tail_view_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_tail_view_2', async () => {
    const stat = compile("../tests/passed/test_asset_tail_view_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update', async () => {
    const stat = compile("../tests/passed/test_asset_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_2', async () => {
    const stat = compile("../tests/passed/test_asset_update_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_3', async () => {
    const stat = compile("../tests/passed/test_asset_update_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_4', async () => {
    const stat = compile("../tests/passed/test_asset_update_4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_5', async () => {
    const stat = compile("../tests/passed/test_asset_update_5.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_aggregate_1', async () => {
    const stat = compile("../tests/passed/test_asset_update_aggregate_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_aggregate_2', async () => {
    const stat = compile("../tests/passed/test_asset_update_aggregate_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_aggregate_3', async () => {
    const stat = compile("../tests/passed/test_asset_update_aggregate_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_1', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_2', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_3', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_4', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_5', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_5.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_6', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_6.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_7', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_7.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_8', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_8.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_update_partition_9', async () => {
    const stat = compile("../tests/passed/test_asset_update_partition_9.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_asset_view', async () => {
    const stat = compile("../tests/passed/test_asset_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_bound_value', async () => {
    const stat = compile("../tests/passed/test_bound_value.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_caller_getter', async () => {
    const stat = compile("../tests/passed/test_caller_getter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_cmp_bool', async () => {
    const stat = compile("../tests/passed/test_cmp_bool.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_complex_sum', async () => {
    const stat = compile("../tests/passed/test_complex_sum.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_conditions', async () => {
    const stat = compile("../tests/passed/test_conditions.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_contains_get', async () => {
    const stat = compile("../tests/passed/test_contains_get.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_contract', async () => {
    const stat = compile("../tests/passed/test_contract.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_contract_self', async () => {
    const stat = compile("../tests/passed/test_contract_self.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_fa1', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl_fa1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_fa2', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl_fa2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_string', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_with_param', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl_with_param.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_with_param_const', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl_with_param_const.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_arl_with_param_with_default', async () => {
    const stat = compile("../tests/passed/test_create_contract_arl_with_param_with_default.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_bytes', async () => {
    const stat = compile("../tests/passed/test_create_contract_bytes.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_inline', async () => {
    const stat = compile("../tests/passed/test_create_contract_inline.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_tz_with_import', async () => {
    const stat = compile("../tests/passed/test_create_contract_tz_with_import.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_create_contract_tz_with_path', async () => {
    const stat = compile("../tests/passed/test_create_contract_tz_with_path.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fget', async () => {
    const stat = compile("../tests/passed/test_fget.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_for_list_alt', async () => {
    const stat = compile("../tests/passed/test_for_list_alt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun0', async () => {
    const stat = compile("../tests/passed/test_fun0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun1', async () => {
    const stat = compile("../tests/passed/test_fun1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun2', async () => {
    const stat = compile("../tests/passed/test_fun2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun3', async () => {
    const stat = compile("../tests/passed/test_fun3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun4', async () => {
    const stat = compile("../tests/passed/test_fun4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun5', async () => {
    const stat = compile("../tests/passed/test_fun5.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun6', async () => {
    const stat = compile("../tests/passed/test_fun6.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun7', async () => {
    const stat = compile("../tests/passed/test_fun7.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun8', async () => {
    const stat = compile("../tests/passed/test_fun8.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun_asset', async () => {
    const stat = compile("../tests/passed/test_fun_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun_asset2', async () => {
    const stat = compile("../tests/passed/test_fun_asset2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_fun_fail', async () => {
    const stat = compile("../tests/passed/test_fun_fail.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter', async () => {
    const stat = compile("../tests/passed/test_getter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter2', async () => {
    const stat = compile("../tests/passed/test_getter2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter_with_arg', async () => {
    const stat = compile("../tests/passed/test_getter_with_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_getter_with_args', async () => {
    const stat = compile("../tests/passed/test_getter_with_args.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_global_constant', async () => {
    const stat = compile("../tests/passed/test_global_constant.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_if_fail_expr', async () => {
    const stat = compile("../tests/passed/test_if_fail_expr.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_if_int_nat', async () => {
    const stat = compile("../tests/passed/test_if_int_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_asset', async () => {
    const stat = compile("../tests/passed/test_init_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_asset2', async () => {
    const stat = compile("../tests/passed/test_init_asset2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_asset3', async () => {
    const stat = compile("../tests/passed/test_init_asset3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_rat_with_nat', async () => {
    const stat = compile("../tests/passed/test_init_rat_with_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_arith', async () => {
    const stat = compile("../tests/passed/test_init_storage_arith.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_cmp', async () => {
    const stat = compile("../tests/passed/test_init_storage_cmp.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_funs', async () => {
    const stat = compile("../tests/passed/test_init_storage_funs.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_literals', async () => {
    const stat = compile("../tests/passed/test_init_storage_literals.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_init_storage_simple', async () => {
    const stat = compile("../tests/passed/test_init_storage_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_initialized_with', async () => {
    const stat = compile("../tests/passed/test_initialized_with.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_initialized_with_asset', async () => {
    const stat = compile("../tests/passed/test_initialized_with_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_is_implicit_address', async () => {
    const stat = compile("../tests/passed/test_is_implicit_address.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_iter', async () => {
    const stat = compile("../tests/passed/test_iter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_iter2', async () => {
    const stat = compile("../tests/passed/test_iter2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_length_operations', async () => {
    const stat = compile("../tests/passed/test_length_operations.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_contains', async () => {
    const stat = compile("../tests/passed/test_list_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_contains2', async () => {
    const stat = compile("../tests/passed/test_list_contains2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_mynth', async () => {
    const stat = compile("../tests/passed/test_list_mynth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_mynth2', async () => {
    const stat = compile("../tests/passed/test_list_mynth2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_mynth3', async () => {
    const stat = compile("../tests/passed/test_list_mynth3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_list_nth', async () => {
    const stat = compile("../tests/passed/test_list_nth.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_metadata', async () => {
    const stat = compile("../tests/passed/test_metadata.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_operations', async () => {
    const stat = compile("../tests/passed/test_operations.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_oracle', async () => {
    const stat = compile("../tests/passed/test_oracle.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_oracle_called', async () => {
    const stat = compile("../tests/passed/test_oracle_called.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_parameter', async () => {
    const stat = compile("../tests/passed/test_parameter.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_parameter_const', async () => {
    const stat = compile("../tests/passed/test_parameter_const.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_prec', async () => {
    const stat = compile("../tests/passed/test_prec.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_rational', async () => {
    const stat = compile("../tests/passed/test_rational.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_read_asset_after_operation', async () => {
    const stat = compile("../tests/passed/test_read_asset_after_operation.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_read_asset_after_update', async () => {
    const stat = compile("../tests/passed/test_read_asset_after_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record', async () => {
    const stat = compile("../tests/passed/test_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_0', async () => {
    const stat = compile("../tests/passed/test_record_access_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_1', async () => {
    const stat = compile("../tests/passed/test_record_access_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_2', async () => {
    const stat = compile("../tests/passed/test_record_access_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_access_3', async () => {
    const stat = compile("../tests/passed/test_record_access_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_1', async () => {
    const stat = compile("../tests/passed/test_record_assign_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_2', async () => {
    const stat = compile("../tests/passed/test_record_assign_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_3', async () => {
    const stat = compile("../tests/passed/test_record_assign_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_full', async () => {
    const stat = compile("../tests/passed/test_record_assign_full.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_assign_var', async () => {
    const stat = compile("../tests/passed/test_record_assign_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_record_simple', async () => {
    const stat = compile("../tests/passed/test_record_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_remove_asset_with_partition', async () => {
    const stat = compile("../tests/passed/test_remove_asset_with_partition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_aggregate', async () => {
    const stat = compile("../tests/passed/test_removeall_aggregate.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_aggregate_1', async () => {
    const stat = compile("../tests/passed/test_removeall_aggregate_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_aggregate_2', async () => {
    const stat = compile("../tests/passed/test_removeall_aggregate_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_partition_1', async () => {
    const stat = compile("../tests/passed/test_removeall_partition_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeall_partition_2', async () => {
    const stat = compile("../tests/passed/test_removeall_partition_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_aggregate_1', async () => {
    const stat = compile("../tests/passed/test_removefield_aggregate_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_aggregate_2', async () => {
    const stat = compile("../tests/passed/test_removefield_aggregate_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_partition_1', async () => {
    const stat = compile("../tests/passed/test_removefield_partition_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removefield_partition_2', async () => {
    const stat = compile("../tests/passed/test_removefield_partition_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_agg_0', async () => {
    const stat = compile("../tests/passed/test_removeif_agg_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_agg_1', async () => {
    const stat = compile("../tests/passed/test_removeif_agg_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_agg_2', async () => {
    const stat = compile("../tests/passed/test_removeif_agg_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_coll_0', async () => {
    const stat = compile("../tests/passed/test_removeif_coll_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_coll_1', async () => {
    const stat = compile("../tests/passed/test_removeif_coll_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_coll_2', async () => {
    const stat = compile("../tests/passed/test_removeif_coll_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_part_0', async () => {
    const stat = compile("../tests/passed/test_removeif_part_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_part_1', async () => {
    const stat = compile("../tests/passed/test_removeif_part_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_removeif_part_2', async () => {
    const stat = compile("../tests/passed/test_removeif_part_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_result', async () => {
    const stat = compile("../tests/passed/test_result.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_split_ticket', async () => {
    const stat = compile("../tests/passed/test_split_ticket.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tez', async () => {
    const stat = compile("../tests/passed/test_tez.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_transfer', async () => {
    const stat = compile("../tests/passed/test_transfer.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_transition', async () => {
    const stat = compile("../tests/passed/test_transition.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tuple_access_1', async () => {
    const stat = compile("../tests/passed/test_tuple_access_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tuple_access_2', async () => {
    const stat = compile("../tests/passed/test_tuple_access_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_tuple_access_3', async () => {
    const stat = compile("../tests/passed/test_tuple_access_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_update', async () => {
    const stat = compile("../tests/passed/test_update.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_var', async () => {
    const stat = compile("../tests/passed/test_var.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('test_voting', async () => {
    const stat = compile("../tests/passed/test_voting.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_create_ticket', async () => {
    const stat = compile("../tests/passed/ticket_create_ticket.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_create_ticket_list_prepend', async () => {
    const stat = compile("../tests/passed/ticket_create_ticket_list_prepend.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_big_map', async () => {
    const stat = compile("../tests/passed/ticket_detach_big_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_big_map_record', async () => {
    const stat = compile("../tests/passed/ticket_detach_big_map_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_big_map_tuple', async () => {
    const stat = compile("../tests/passed/ticket_detach_big_map_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_map', async () => {
    const stat = compile("../tests/passed/ticket_detach_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_map_record', async () => {
    const stat = compile("../tests/passed/ticket_detach_map_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_map_tuple', async () => {
    const stat = compile("../tests/passed/ticket_detach_map_tuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_detach_option', async () => {
    const stat = compile("../tests/passed/ticket_detach_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_fun_join_tickets', async () => {
    const stat = compile("../tests/passed/ticket_fun_join_tickets.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_fun_split_ticket', async () => {
    const stat = compile("../tests/passed/ticket_fun_split_ticket.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_list', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_arg_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_record_list', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_arg_record_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_record_list2', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_arg_record_list2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_tuple_2_list', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_arg_tuple_2_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_arg_tuple_3_list', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_arg_tuple_3_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_record', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_record.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_read_ticket_record_list', async () => {
    const stat = compile("../tests/passed/ticket_read_ticket_record_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_0_0', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_0_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_0_1', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_0_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_0_2', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_0_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_1_0', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_1_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_1_1', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_1_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_1_2', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_1_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_2_0', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_2_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_2_1', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_2_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_2_2', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_2_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_3_0', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_3_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_3_1', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_3_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_record_list_var_3_2', async () => {
    const stat = compile("../tests/passed/ticket_record_list_var_3_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_store_map', async () => {
    const stat = compile("../tests/passed/ticket_store_map.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_store_option', async () => {
    const stat = compile("../tests/passed/ticket_store_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_detach_option', async () => {
    const stat = compile("../tests/passed/ticket_var_detach_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_list', async () => {
    const stat = compile("../tests/passed/ticket_var_list.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_option', async () => {
    const stat = compile("../tests/passed/ticket_var_option.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_or_left', async () => {
    const stat = compile("../tests/passed/ticket_var_or_left.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_or_right', async () => {
    const stat = compile("../tests/passed/ticket_var_or_right.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('ticket_var_simple', async () => {
    const stat = compile("../tests/passed/ticket_var_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_call', async () => {
    const stat = compile("../tests/passed/transfer_call.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_entrypoint', async () => {
    const stat = compile("../tests/passed/transfer_entrypoint.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_entrypoint2', async () => {
    const stat = compile("../tests/passed/transfer_entrypoint2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_op', async () => {
    const stat = compile("../tests/passed/transfer_op.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_require_entrypoint', async () => {
    const stat = compile("../tests/passed/transfer_require_entrypoint.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_self', async () => {
    const stat = compile("../tests/passed/transfer_self.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_simple', async () => {
    const stat = compile("../tests/passed/transfer_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('transfer_simple_with_entrypoint', async () => {
    const stat = compile("../tests/passed/transfer_simple_with_entrypoint.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('tuple_in_contains', async () => {
    const stat = compile("../tests/passed/tuple_in_contains.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_never', async () => {
    const stat = compile("../tests/passed/type_never.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_or', async () => {
    const stat = compile("../tests/passed/type_or.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_set_enum_param', async () => {
    const stat = compile("../tests/passed/type_set_enum_param.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('type_storage_or', async () => {
    const stat = compile("../tests/passed/type_storage_or.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('typetuple', async () => {
    const stat = compile("../tests/passed/typetuple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('unused_argument', async () => {
    const stat = compile("../tests/passed/unused_argument.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('unused_variable', async () => {
    const stat = compile("../tests/passed/unused_variable.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('unused_variable_opt', async () => {
    const stat = compile("../tests/passed/unused_variable_opt.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('update_minus_equal', async () => {
    const stat = compile("../tests/passed/update_minus_equal.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_ids', async () => {
    const stat = compile("../tests/passed/var_mult_ids.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_ids_3', async () => {
    const stat = compile("../tests/passed/var_mult_ids_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_ids_complex', async () => {
    const stat = compile("../tests/passed/var_mult_ids_complex.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_mult_tickets', async () => {
    const stat = compile("../tests/passed/var_mult_tickets.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('var_without_effect', async () => {
    const stat = compile("../tests/passed/var_without_effect.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('variable_in_container', async () => {
    const stat = compile("../tests/passed/variable_in_container.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('very_simple', async () => {
    const stat = compile("../tests/passed/very_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_0', async () => {
    const stat = compile("../tests/passed/view_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_all_chain', async () => {
    const stat = compile("../tests/passed/view_all_chain.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_0', async () => {
    const stat = compile("../tests/passed/view_args_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_1', async () => {
    const stat = compile("../tests/passed/view_args_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_storage_0', async () => {
    const stat = compile("../tests/passed/view_args_storage_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_args_storage_1', async () => {
    const stat = compile("../tests/passed/view_args_storage_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_asset', async () => {
    const stat = compile("../tests/passed/view_asset.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_exhaustive', async () => {
    const stat = compile("../tests/passed/view_exhaustive.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_in_arg', async () => {
    const stat = compile("../tests/passed/view_in_arg.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_offchain', async () => {
    const stat = compile("../tests/passed/view_offchain.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_offchain_nat', async () => {
    const stat = compile("../tests/passed/view_offchain_nat.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_onchain', async () => {
    const stat = compile("../tests/passed/view_onchain.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_onchain_offchain', async () => {
    const stat = compile("../tests/passed/view_onchain_offchain.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_simple', async () => {
    const stat = compile("../tests/passed/view_simple.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_simple_call', async () => {
    const stat = compile("../tests/passed/view_simple_call.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_simple_caller', async () => {
    const stat = compile("../tests/passed/view_simple_caller.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_0', async () => {
    const stat = compile("../tests/passed/view_storage_0.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_1', async () => {
    const stat = compile("../tests/passed/view_storage_1.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_2', async () => {
    const stat = compile("../tests/passed/view_storage_2.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_3', async () => {
    const stat = compile("../tests/passed/view_storage_3.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_4', async () => {
    const stat = compile("../tests/passed/view_storage_4.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_storage_5', async () => {
    const stat = compile("../tests/passed/view_storage_5.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_with_nat_to_string', async () => {
    const stat = compile("../tests/passed/view_with_nat_to_string.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_with_self', async () => {
    const stat = compile("../tests/passed/view_with_self.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('view_with_self_add', async () => {
    const stat = compile("../tests/passed/view_with_self_add.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('with_metadata_json', async () => {
    const stat = compile("../tests/passed/with_metadata_json.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('with_metadata_json_with_offchain_view', async () => {
    const stat = compile("../tests/passed/with_metadata_json_with_offchain_view.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('with_metadata_uri', async () => {
    const stat = compile("../tests/passed/with_metadata_uri.arl")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
})
  