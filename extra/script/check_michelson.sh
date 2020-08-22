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
./tests/passed/expr_list_length.arl \
./tests/passed/expr_map_lit.arl \
./tests/passed/expr_map_put.arl \
./tests/passed/expr_map_remove.arl \
./tests/passed/expr_map_getopt.arl \
./tests/passed/expr_map_length.arl \
./tests/passed/expr_map_contains.arl \
./tests/passed/simple_multi_entry.arl \
./tests/passed/simple_multi_entry2.arl \
./tests/passed/effect_control_if.arl \
./tests/passed/effect_control_if_else.arl \
./tests/passed/expr_control_if_else_int_int.arl \
./tests/passed/expr_control_if_else_int_nat.arl \
./tests/passed/expr_control_if_else_nat_int.arl \
./tests/passed/expr_control_if_else_nat_nat.arl \
./tests/passed/test_if_int_nat.arl \
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
