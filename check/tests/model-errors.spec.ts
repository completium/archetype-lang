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

describe('model-errors', async () => {
  it('asset_initializedby_partition_filled_no_contained_asset_init', async () => {
    const stat = compile("../tests/model-errors/asset_initializedby_partition_filled_no_contained_asset_init.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('asset_with_not_empty_partition', async () => {
    const stat = compile("../tests/model-errors/asset_with_not_empty_partition.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('caller_as_key_in_asset_init', async () => {
    const stat = compile("../tests/model-errors/caller_as_key_in_asset_init.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('complex_expr_key_in_init_asset', async () => {
    const stat = compile("../tests/model-errors/complex_expr_key_in_init_asset.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('duplicate_key_in_init_asset', async () => {
    const stat = compile("../tests/model-errors/duplicate_key_in_init_asset.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('init_forbidden_value', async () => {
    const stat = compile("../tests/model-errors/init_forbidden_value.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_add_on_partition', async () => {
    const stat = compile("../tests/model-errors/no_add_on_partition.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_caller_in_init', async () => {
    const stat = compile("../tests/model-errors/no_caller_in_init.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_field_aggregate_in_aggregate', async () => {
    const stat = compile("../tests/model-errors/no_field_aggregate_in_aggregate.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_field_aggregate_in_partition', async () => {
    const stat = compile("../tests/model-errors/no_field_aggregate_in_partition.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_field_partition_in_aggregate', async () => {
    const stat = compile("../tests/model-errors/no_field_partition_in_aggregate.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_field_partition_in_partition', async () => {
    const stat = compile("../tests/model-errors/no_field_partition_in_partition.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_sort_on_key_with_multi_key', async () => {
    const stat = compile("../tests/model-errors/no_sort_on_key_with_multi_key.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_source_in_init', async () => {
    const stat = compile("../tests/model-errors/no_source_in_init.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('storage_init_caller', async () => {
    const stat = compile("../tests/model-errors/storage_init_caller.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('storage_init_source', async () => {
    const stat = compile("../tests/model-errors/storage_init_source.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('test_caller_in_init', async () => {
    const stat = compile("../tests/model-errors/test_caller_in_init.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
})
  