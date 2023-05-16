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

describe('proposal-type-errors', async () => {
  it('assign_error_int_rat', async () => {
    const stat = compile("../tests/proposal-type-errors/assign_error_int_rat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('assign_op_nat_rat', async () => {
    const stat = compile("../tests/proposal-type-errors/assign_op_nat_rat.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('bad_type_for_pack', async () => {
    const stat = compile("../tests/proposal-type-errors/bad_type_for_pack.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('getter_used_like_function', async () => {
    const stat = compile("../tests/proposal-type-errors/getter_used_like_function.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('id_redefinition', async () => {
    const stat = compile("../tests/proposal-type-errors/id_redefinition.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('id_too_large', async () => {
    const stat = compile("../tests/proposal-type-errors/id_too_large.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('init_constant_with_variable', async () => {
    const stat = compile("../tests/proposal-type-errors/init_constant_with_variable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('instruction_const', async () => {
    const stat = compile("../tests/proposal-type-errors/instruction_const.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_cond_type_ternary', async () => {
    const stat = compile("../tests/proposal-type-errors/invalid_cond_type_ternary.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_contract_type_pushable', async () => {
    const stat = compile("../tests/proposal-type-errors/invalid_contract_type_pushable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_contract_type_storable', async () => {
    const stat = compile("../tests/proposal-type-errors/invalid_contract_type_storable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_instruction_view_fun', async () => {
    const stat = compile("../tests/proposal-type-errors/invalid_instruction_view_fun.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('invalid_rational_literal', async () => {
    const stat = compile("../tests/proposal-type-errors/invalid_rational_literal.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('iterable_big_map_invalid_put', async () => {
    const stat = compile("../tests/proposal-type-errors/iterable_big_map_invalid_put.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('iterable_big_map_invalid_remove', async () => {
    const stat = compile("../tests/proposal-type-errors/iterable_big_map_invalid_remove.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('iterable_big_map_no_assign', async () => {
    const stat = compile("../tests/proposal-type-errors/iterable_big_map_no_assign.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('iterable_big_map_no_local_var', async () => {
    const stat = compile("../tests/proposal-type-errors/iterable_big_map_no_local_var.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('missing_return', async () => {
    const stat = compile("../tests/proposal-type-errors/missing_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('neg_literal_tez', async () => {
    const stat = compile("../tests/proposal-type-errors/neg_literal_tez.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_passable', async () => {
    const stat = compile("../tests/proposal-type-errors/no_passable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_return', async () => {
    const stat = compile("../tests/proposal-type-errors/no_return.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_side_effect_in_function2', async () => {
    const stat = compile("../tests/proposal-type-errors/no_side_effect_in_function2.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_storable_in_asset', async () => {
    const stat = compile("../tests/proposal-type-errors/no_storable_in_asset.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('no_storable_in_variable', async () => {
    const stat = compile("../tests/proposal-type-errors/no_storable_in_variable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('plus_equals_nat_rational', async () => {
    const stat = compile("../tests/proposal-type-errors/plus_equals_nat_rational.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
  it('record_not_comparable', async () => {
    const stat = compile("../tests/proposal-type-errors/record_not_comparable.arl")
    assert(stat.status == 3, "Invalid status code, actual: " + stat.status + ", expected: 3")
  })
})
  