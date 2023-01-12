/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../archetype.exe'
  const res = spawn.sync(bin, [p], { });
  return res
}

/* Tests ------------------------------------------------------------------- */

describe('Type errors', async () => {
  it('arith_div_tez_int', async () => {
    const stat = compile("../tests/type-errors/arith_div_tez_int.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('arith_div_tez_nat', async () => {
    const stat = compile("../tests/type-errors/arith_div_tez_nat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('arith_div_tez_rat', async () => {
    const stat = compile("../tests/type-errors/arith_div_tez_rat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('arith_mult_tez_rat', async () => {
    const stat = compile("../tests/type-errors/arith_mult_tez_rat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_collect_to_view', async () => {
    const stat = compile("../tests/type-errors/asset_collect_to_view.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_initializedby_aggregate_filled_bad_init', async () => {
    const stat = compile("../tests/type-errors/asset_initializedby_aggregate_filled_bad_init.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_key_is_not_comparable', async () => {
    const stat = compile("../tests/type-errors/asset_key_is_not_comparable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_multi_key_bad_order', async () => {
    const stat = compile("../tests/type-errors/asset_multi_key_bad_order.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_multi_key_doublon', async () => {
    const stat = compile("../tests/type-errors/asset_multi_key_doublon.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_multi_key_invalid_type', async () => {
    const stat = compile("../tests/type-errors/asset_multi_key_invalid_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_value_key', async () => {
    const stat = compile("../tests/type-errors/asset_value_key.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('asset_value_key_opt', async () => {
    const stat = compile("../tests/type-errors/asset_value_key_opt.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('bad_at_unknown_label', async () => {
    const stat = compile("../tests/type-errors/bad_at_unknown_label.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('bad_type_return', async () => {
    const stat = compile("../tests/type-errors/bad_type_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('called_by_an_asset_without_address_typed_key', async () => {
    const stat = compile("../tests/type-errors/called_by_an_asset_without_address_typed_key.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('cast_tez_nat', async () => {
    const stat = compile("../tests/type-errors/cast_tez_nat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('cmp_dur_int', async () => {
    const stat = compile("../tests/type-errors/cmp_dur_int.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('cmp_tez_nat', async () => {
    const stat = compile("../tests/type-errors/cmp_tez_nat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('cmp_tuple_not_comparable', async () => {
    const stat = compile("../tests/type-errors/cmp_tuple_not_comparable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('compare_list_int', async () => {
    const stat = compile("../tests/type-errors/compare_list_int.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('compare_list_list', async () => {
    const stat = compile("../tests/type-errors/compare_list_list.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('const_assign', async () => {
    const stat = compile("../tests/type-errors/const_assign.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('contract_invalid_self_entry', async () => {
    const stat = compile("../tests/type-errors/contract_invalid_self_entry.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('contract_unknown_self_entry', async () => {
    const stat = compile("../tests/type-errors/contract_unknown_self_entry.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('dec_lit_error', async () => {
    const stat = compile("../tests/type-errors/dec_lit_error.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('decl_no_asset', async () => {
    const stat = compile("../tests/type-errors/decl_no_asset.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('decl_no_asset_aggregate', async () => {
    const stat = compile("../tests/type-errors/decl_no_asset_aggregate.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('decl_no_asset_option', async () => {
    const stat = compile("../tests/type-errors/decl_no_asset_option.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('duplicated_action', async () => {
    const stat = compile("../tests/type-errors/duplicated_action.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('effect_control_for_collection_big_map', async () => {
    const stat = compile("../tests/type-errors/effect_control_for_collection_big_map.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('effect_method_asset_addupdate_aggregate', async () => {
    const stat = compile("../tests/type-errors/effect_method_asset_addupdate_aggregate.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('effect_method_asset_update_aggregate', async () => {
    const stat = compile("../tests/type-errors/effect_method_asset_update_aggregate.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('effect_method_asset_update_all_coll_0', async () => {
    const stat = compile("../tests/type-errors/effect_method_asset_update_all_coll_0.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('effect_method_asset_update_partition', async () => {
    const stat = compile("../tests/type-errors/effect_method_asset_update_partition.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_eq_list', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_eq_list.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_eq_map', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_eq_map.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_eq_option', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_eq_option.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_eq_set', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_eq_set.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_ne_list', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_ne_list.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_ne_map', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_ne_map.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_ne_option', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_ne_option.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_cmp_ne_set', async () => {
    const stat = compile("../tests/type-errors/expr_cmp_ne_set.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_fold_bad_return', async () => {
    const stat = compile("../tests/type-errors/expr_control_fold_bad_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_fold_bad_type', async () => {
    const stat = compile("../tests/type-errors/expr_control_fold_bad_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_list_bad_return', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_list_bad_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_list_bad_type', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_list_bad_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_option_bad_return', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_option_bad_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_option_bad_type', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_option_bad_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_option_non_homogeneous', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_option_non_homogeneous.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_or_bad_return', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_or_bad_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_or_bad_type', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_or_bad_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_control_match_or_subptn_tyerror', async () => {
    const stat = compile("../tests/type-errors/expr_control_match_or_subptn_tyerror.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_formula_asset_method_sort', async () => {
    const stat = compile("../tests/type-errors/expr_formula_asset_method_sort.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_lambda_error1', async () => {
    const stat = compile("../tests/type-errors/expr_lambda_error1.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_lambda_error2', async () => {
    const stat = compile("../tests/type-errors/expr_lambda_error2.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_method_asset_addupdate_missing_field', async () => {
    const stat = compile("../tests/type-errors/expr_method_asset_addupdate_missing_field.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_method_asset_addupdate_no_init_value', async () => {
    const stat = compile("../tests/type-errors/expr_method_asset_addupdate_no_init_value.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_method_asset_get_view', async () => {
    const stat = compile("../tests/type-errors/expr_method_asset_get_view.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_record_update_asset_in_exec', async () => {
    const stat = compile("../tests/type-errors/expr_record_update_asset_in_exec.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_record_update_record_invalid_type', async () => {
    const stat = compile("../tests/type-errors/expr_record_update_record_invalid_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('expr_record_update_record_unkown_field', async () => {
    const stat = compile("../tests/type-errors/expr_record_update_record_unkown_field.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('field_method', async () => {
    const stat = compile("../tests/type-errors/field_method.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('for_ident_double_asset_container', async () => {
    const stat = compile("../tests/type-errors/for_ident_double_asset_container.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('for_ident_double_list', async () => {
    const stat = compile("../tests/type-errors/for_ident_double_list.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('for_ident_double_set', async () => {
    const stat = compile("../tests/type-errors/for_ident_double_set.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('for_ident_simple_map', async () => {
    const stat = compile("../tests/type-errors/for_ident_simple_map.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_type_do_require', async () => {
    const stat = compile("../tests/type-errors/invalid_type_do_require.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_type_for_key_map', async () => {
    const stat = compile("../tests/type-errors/invalid_type_for_key_map.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('iterable_big_map_update', async () => {
    const stat = compile("../tests/type-errors/iterable_big_map_update.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_asset_effect_on_view_add', async () => {
    const stat = compile("../tests/type-errors/no_asset_effect_on_view_add.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_asset_effect_on_view_addupdate', async () => {
    const stat = compile("../tests/type-errors/no_asset_effect_on_view_addupdate.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_asset_effect_on_view_remove', async () => {
    const stat = compile("../tests/type-errors/no_asset_effect_on_view_remove.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_asset_effect_on_view_update', async () => {
    const stat = compile("../tests/type-errors/no_asset_effect_on_view_update.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_asset_in_asset', async () => {
    const stat = compile("../tests/type-errors/no_asset_in_asset.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_assignment_for_argument', async () => {
    const stat = compile("../tests/type-errors/no_assignment_for_argument.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_assignment_of_prog_var_in_shadow_effect', async () => {
    const stat = compile("../tests/type-errors/no_assignment_of_prog_var_in_shadow_effect.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_assignment_of_shadow_var_in_prog_effect', async () => {
    const stat = compile("../tests/type-errors/no_assignment_of_shadow_var_in_prog_effect.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_assignment_var_iter', async () => {
    const stat = compile("../tests/type-errors/no_assignment_var_iter.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_at_in_postcondition', async () => {
    const stat = compile("../tests/type-errors/no_at_in_postcondition.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_big_map_value_type', async () => {
    const stat = compile("../tests/type-errors/no_big_map_value_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_builtin_for_aggregate', async () => {
    const stat = compile("../tests/type-errors/no_builtin_for_aggregate.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_container_aggregate_arg', async () => {
    const stat = compile("../tests/type-errors/no_container_aggregate_arg.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_container_aggregate_var', async () => {
    const stat = compile("../tests/type-errors/no_container_aggregate_var.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_container_aggregate_variable', async () => {
    const stat = compile("../tests/type-errors/no_container_aggregate_variable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_container_partition_arg', async () => {
    const stat = compile("../tests/type-errors/no_container_partition_arg.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_container_partition_var', async () => {
    const stat = compile("../tests/type-errors/no_container_partition_var.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_container_partition_variable', async () => {
    const stat = compile("../tests/type-errors/no_container_partition_variable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_contract_invariant_in_fun_spec', async () => {
    const stat = compile("../tests/type-errors/no_contract_invariant_in_fun_spec.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_effect_local_var_in_formula', async () => {
    const stat = compile("../tests/type-errors/no_effect_local_var_in_formula.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_letin_instr_in_execution', async () => {
    const stat = compile("../tests/type-errors/no_letin_instr_in_execution.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_method_in_select', async () => {
    const stat = compile("../tests/type-errors/no_method_in_select.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_otherwise', async () => {
    const stat = compile("../tests/type-errors/no_otherwise.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_packable_pack', async () => {
    const stat = compile("../tests/type-errors/no_packable_pack.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_packable_unpack', async () => {
    const stat = compile("../tests/type-errors/no_packable_unpack.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_pkey_assignment_in_addupdate', async () => {
    const stat = compile("../tests/type-errors/no_pkey_assignment_in_addupdate.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_pkey_assignment_in_update', async () => {
    const stat = compile("../tests/type-errors/no_pkey_assignment_in_update.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_postcondition_in_global_spec', async () => {
    const stat = compile("../tests/type-errors/no_postcondition_in_global_spec.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_read_global_shadow_var', async () => {
    const stat = compile("../tests/type-errors/no_read_global_shadow_var.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_result_in_prog', async () => {
    const stat = compile("../tests/type-errors/no_result_in_prog.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_side_effect_in_function1', async () => {
    const stat = compile("../tests/type-errors/no_side_effect_in_function1.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_side_effect_in_function3', async () => {
    const stat = compile("../tests/type-errors/no_side_effect_in_function3.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_update_asset_key', async () => {
    const stat = compile("../tests/type-errors/no_update_asset_key.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_var_sum', async () => {
    const stat = compile("../tests/type-errors/no_var_sum.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('same_field_name', async () => {
    const stat = compile("../tests/type-errors/same_field_name.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('same_id_variable_parameter', async () => {
    const stat = compile("../tests/type-errors/same_id_variable_parameter.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('shadow_field_errors_1', async () => {
    const stat = compile("../tests/type-errors/shadow_field_errors_1.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('shadow_field_errors_2', async () => {
    const stat = compile("../tests/type-errors/shadow_field_errors_2.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('shadow_field_errors_3', async () => {
    const stat = compile("../tests/type-errors/shadow_field_errors_3.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('sort_in_formula', async () => {
    const stat = compile("../tests/type-errors/sort_in_formula.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('spec_fail_fid_invalid_type', async () => {
    const stat = compile("../tests/type-errors/spec_fail_fid_invalid_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('spec_fail_fid_unknown', async () => {
    const stat = compile("../tests/type-errors/spec_fail_fid_unknown.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('specasset_unknown_field', async () => {
    const stat = compile("../tests/type-errors/specasset_unknown_field.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('specasset_unknown_id', async () => {
    const stat = compile("../tests/type-errors/specasset_unknown_id.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('specfun_bad_kind', async () => {
    const stat = compile("../tests/type-errors/specfun_bad_kind.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('specfun_bad_sig', async () => {
    const stat = compile("../tests/type-errors/specfun_bad_sig.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('specfun_unknown_id', async () => {
    const stat = compile("../tests/type-errors/specfun_unknown_id.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('specvar_unknown_id', async () => {
    const stat = compile("../tests/type-errors/specvar_unknown_id.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('state_assignment', async () => {
    const stat = compile("../tests/type-errors/state_assignment.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('state_is_bad_value', async () => {
    const stat = compile("../tests/type-errors/state_is_bad_value.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('tuple_index_out_of_bound', async () => {
    const stat = compile("../tests/type-errors/tuple_index_out_of_bound.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('tuple_with_no_literals_access', async () => {
    const stat = compile("../tests/type-errors/tuple_with_no_literals_access.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('update_type_error', async () => {
    const stat = compile("../tests/type-errors/update_type_error.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
})
  