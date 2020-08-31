#! /bin/bash

BIN=./extra/script/check_compile.sh

PASSED="\
./tests/passed/simple_op_add.arl \
./tests/passed/simple_op_uminus.arl \
./tests/passed/simple_assign1.arl \
./tests/passed/simple_assign2.arl \
./tests/passed/simple_assign3.arl \
./tests/passed/simple_sequence.arl \
./tests/passed/simple_sequence_with_arg.arl \
./tests/passed/simple_sequence_with_arg_var.arl \
./tests/passed/simple_sequence_with_arg2.arl \
./tests/passed/expr_set_lit.arl \
./tests/passed/expr_set_add.arl \
./tests/passed/expr_set_remove.arl \
./tests/passed/expr_set_length.arl  \
./tests/passed/expr_set_contains.arl \
./tests/passed/expr_list_lit.arl \
./tests/passed/expr_list_prepend.arl \
./tests/passed/expr_list_head_tail.arl \
./tests/passed/expr_list_length.arl \
./tests/passed/expr_map_lit.arl \
./tests/passed/expr_map_put.arl \
./tests/passed/expr_map_remove.arl \
./tests/passed/expr_map_get.arl \
./tests/passed/expr_map_getopt.arl \
./tests/passed/expr_map_length.arl \
./tests/passed/expr_map_contains.arl \
./tests/passed/simple_multi_entry.arl \
./tests/passed/simple_multi_entry2.arl \
./tests/passed/simple_multi_entry3.arl \
./tests/passed/effect_control_if.arl \
./tests/passed/effect_control_if_else.arl \
./tests/passed/expr_control_if_else_int_int.arl \
./tests/passed/expr_control_if_else_int_nat.arl \
./tests/passed/expr_control_if_else_nat_int.arl \
./tests/passed/expr_control_if_else_nat_nat.arl \
./tests/passed/test_if_int_nat.arl \
./tests/passed/simple_while.arl \
./tests/passed/effect_control_while.arl \
./tests/passed/effect_control_for_set.arl \
./tests/passed/effect_control_for_list.arl \
./tests/passed/effect_control_for_map.arl \
./tests/passed/test_for_list_alt.arl \
./tests/passed/expr_lit_opt_none.arl \
./tests/passed/expr_lit_opt_some.arl \
./tests/passed/expr_fun_opt_isnone.arl \
./tests/passed/expr_fun_opt_issome.arl \
./tests/passed/expr_fun_opt_optget.arl \
./tests/passed/expr_arith_ediv_nat_nat.arl \
./tests/passed/expr_arith_mod_nat_nat.arl \
./tests/passed/simple_fun1.arl \
./tests/passed/simple_fun2.arl \
./tests/passed/simple_fun3.arl \
./tests/passed/simple_fun4.arl \
./tests/passed/simple_fun5.arl \
./tests/passed/simple_fun6.arl \
./tests/passed/simple_fun7.arl \
./tests/passed/simple_fun8.arl \
./tests/passed/simple_fun_alt.arl \
./tests/passed/transfer_simple.arl \
./tests/passed/transfer_call.arl \
./tests/passed/transfer_entrypoint.arl \
./tests/passed/transfer_self.arl \
./tests/passed/transfer_simple_with_entrypoint.arl \
./tests/passed/test_transfer.arl \
./tests/passed/test_entrysig_self.arl \
./tests/passed/lang_entrysig.arl \
./tests/passed/test_operations.arl \
./tests/passed/simple_record_lit.arl \
./tests/passed/simple_record_lit_rev.arl \
./tests/passed/simple_record_assign.arl \
./tests/passed/simple_record_assign1.arl \
./tests/passed/simple_record_assign2.arl \
./tests/passed/expr_fun_floor.arl \
./tests/passed/expr_fun_ceil.arl \
./tests/passed/expr_fun_to_string_nat.arl \
./tests/passed/test_list_contains.arl \
./tests/passed/test_list_contains2.arl \
./tests/passed/test_list_nth.arl \
./tests/passed/test_list_mynth.arl \
./tests/passed/test_list_mynth2.arl \
./tests/passed/simple_fun_with_storage.arl \
./tests/passed/simple_fun_with_storage2.arl \
./tests/passed/simple_fun_with_storage3.arl \
./tests/passed/rat_nat.arl \
./tests/passed/rat_int.arl \
./tests/passed/rat_cmp_eq.arl \
./tests/passed/rat_cmp_lt.arl \
./tests/passed/rat_cmp_le.arl \
./tests/passed/rat_cmp_gt.arl \
./tests/passed/rat_cmp_ge.arl \
./tests/passed/rat_arith_uminus.arl \
./tests/passed/rat_arith_plus.arl \
./tests/passed/rat_arith_minus.arl \
./tests/passed/rat_arith_mult.arl \
./tests/passed/rat_arith_div.arl \
./tests/passed/rat_tez.arl \
./tests/passed/rat_dur.arl \
./tests/passed/cast_tez_nat.arl \
./tests/passed/simple_asset_skip_empty_one_field.arl \
./tests/passed/simple_asset_skip_empty.arl \
./tests/passed/simple_asset_skip_one_field.arl \
./tests/passed/simple_asset_skip.arl \
./tests/passed/simple_asset_get_one_field.arl \
./tests/passed/simple_asset_get_asset1_key.arl \
./tests/passed/simple_asset_get_asset1_value.arl \
./tests/passed/simple_asset_get_asset2_key.arl \
./tests/passed/simple_asset_get_asset2_value.arl \
./tests/passed/simple_asset_get_asset2_value2.arl \
./tests/passed/expr_method_asset_count.arl \
./tests/passed/effect_method_asset_add_asset_one_field.arl \
./tests/passed/effect_method_asset_add_asset.arl \
./tests/passed/effect_method_asset_add_asset2.arl \
./tests/passed/effect_method_asset_remove_asset_one_field.arl \
./tests/passed/effect_method_asset_remove_asset.arl \
./tests/passed/effect_method_asset_remove_asset2.arl \
./tests/passed/effect_method_asset_add_asset_with_aggregate.arl \
./tests/passed/test_add_asset_with_aggregate.arl \
./tests/passed/effect_method_asset_add_asset_with_partition.arl \
./tests/passed/effect_method_asset_add_asset_with_partition_2.arl \
./tests/passed/test_add_asset_with_partition.arl \
./tests/passed/test_add_asset_with_both.arl \
"

RET=0

echo "Check michelson"
echo ""
echo "                                                             RET GEN COMPILE"
for i in $PASSED; do
    ${BIN} $i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

exit $RET
