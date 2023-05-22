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

describe('type-errors', async () => {
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
  it('create_contract_arl_with_param_anonymous_field', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_anonymous_field.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_bad_storage_lit', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_bad_storage_lit.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_bad_type', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_bad_type.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_const_no_lit', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_const_no_lit.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_duplicated_field', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_duplicated_field.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_missing_field', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_missing_field.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_no_param', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_no_param.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('create_contract_arl_with_param_unknown_field', async () => {
    const stat = compile("../tests/type-errors/create_contract_arl_with_param_unknown_field.arl")
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
  it('error_bls_arith', async () => {
    const stat = compile("../tests/type-errors/error_bls_arith.arl")
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
  it('fun_error_unit', async () => {
    const stat = compile("../tests/type-errors/fun_error_unit.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_error_view_write', async () => {
    const stat = compile("../tests/type-errors/fun_error_view_write.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_error_view_write_with_transfer', async () => {
    const stat = compile("../tests/type-errors/fun_error_view_write_with_transfer.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_error_void_call_as_expression', async () => {
    const stat = compile("../tests/type-errors/fun_error_void_call_as_expression.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_instr_error_invalid_number_args', async () => {
    const stat = compile("../tests/type-errors/fun_instr_error_invalid_number_args.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_instr_error_invalid_type_arg', async () => {
    const stat = compile("../tests/type-errors/fun_instr_error_invalid_type_arg.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_instr_error_not_found', async () => {
    const stat = compile("../tests/type-errors/fun_instr_error_not_found.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_instr_error_return_nat', async () => {
    const stat = compile("../tests/type-errors/fun_instr_error_return_nat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_instr_error_return_unit', async () => {
    const stat = compile("../tests/type-errors/fun_instr_error_return_unit.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_view_error', async () => {
    const stat = compile("../tests/type-errors/fun_view_error.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('fun_view_instr_side_effect', async () => {
    const stat = compile("../tests/type-errors/fun_view_instr_side_effect.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('import_arl_fun_error_instr', async () => {
    const stat = compile("../tests/type-errors/import_arl_fun_error_instr.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('import_arl_fun_error_side_effect', async () => {
    const stat = compile("../tests/type-errors/import_arl_fun_error_side_effect.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('import_arl_fun_error_storage_usage', async () => {
    const stat = compile("../tests/type-errors/import_arl_fun_error_storage_usage.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('import_arl_fun_error_storage_var_usage', async () => {
    const stat = compile("../tests/type-errors/import_arl_fun_error_storage_var_usage.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('import_arl_fun_error_unknown_fun', async () => {
    const stat = compile("../tests/type-errors/import_arl_fun_error_unknown_fun.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('import_arl_fun_error_unknown_namespace', async () => {
    const stat = compile("../tests/type-errors/import_arl_fun_error_unknown_namespace.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_instruction_view_asset', async () => {
    const stat = compile("../tests/type-errors/invalid_instruction_view_asset.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_instruction_view_assign', async () => {
    const stat = compile("../tests/type-errors/invalid_instruction_view_assign.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_instruction_view_container_instr', async () => {
    const stat = compile("../tests/type-errors/invalid_instruction_view_container_instr.arl")
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
  it('no_acces_to_field_in_partition', async () => {
    const stat = compile("../tests/type-errors/no_acces_to_field_in_partition.arl")
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
  it('no_assignment_var_iter', async () => {
    const stat = compile("../tests/type-errors/no_assignment_var_iter.arl")
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
  it('no_letin_instr_in_execution', async () => {
    const stat = compile("../tests/type-errors/no_letin_instr_in_execution.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_method_in_select', async () => {
    const stat = compile("../tests/type-errors/no_method_in_select.arl")
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
  it('no_result_in_prog', async () => {
    const stat = compile("../tests/type-errors/no_result_in_prog.arl")
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
  it('sapling_type_error', async () => {
    const stat = compile("../tests/type-errors/sapling_type_error.arl")
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
  it('view_error_bad_arguments_0', async () => {
    const stat = compile("../tests/type-errors/view_error_bad_arguments_0.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_bad_arguments_1', async () => {
    const stat = compile("../tests/type-errors/view_error_bad_arguments_1.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_bad_arguments_2', async () => {
    const stat = compile("../tests/type-errors/view_error_bad_arguments_2.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_bad_fs_kind_fun', async () => {
    const stat = compile("../tests/type-errors/view_error_bad_fs_kind_fun.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_bad_fs_kind_getter', async () => {
    const stat = compile("../tests/type-errors/view_error_bad_fs_kind_getter.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_bad_return_type_fun', async () => {
    const stat = compile("../tests/type-errors/view_error_bad_return_type_fun.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_off_chain_visibility', async () => {
    const stat = compile("../tests/type-errors/view_error_off_chain_visibility.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('view_error_unknown', async () => {
    const stat = compile("../tests/type-errors/view_error_unknown.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
})
  