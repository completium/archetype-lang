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

describe('proposal-model-errors', async () => {
  it('asset_initializedby_aggregate_filled_not_exist', async () => {
    const stat = compile("../tests/proposal-model-errors/asset_initializedby_aggregate_filled_not_exist.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('asset_initializedby_partition_filled_duplicated', async () => {
    const stat = compile("../tests/proposal-model-errors/asset_initializedby_partition_filled_duplicated.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
  it('no_clear_for_asset_in_partition', async () => {
    const stat = compile("../tests/proposal-model-errors/no_clear_for_asset_in_partition.arl")
    assert(stat.status == 5, "Invalid status code, actual: " + stat.status + ", expected: 5")
  })
})
  