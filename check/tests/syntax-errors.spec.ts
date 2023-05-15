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

describe('syntax-errors', async () => {
  it('effect_with_several_errors', async () => {
    const stat = compile("../tests/syntax-errors/effect_with_several_errors.arl")
    assert(stat.status == 1, "Invalid status code, actual: " + stat.status + ", expected: 1")
  })
  it('empty_effect', async () => {
    const stat = compile("../tests/syntax-errors/empty_effect.arl")
    assert(stat.status == 1, "Invalid status code, actual: " + stat.status + ", expected: 1")
  })
  it('large_message_error', async () => {
    const stat = compile("../tests/syntax-errors/large_message_error.arl")
    assert(stat.status == 1, "Invalid status code, actual: " + stat.status + ", expected: 1")
  })
  it('simple_error', async () => {
    const stat = compile("../tests/syntax-errors/simple_error.arl")
    assert(stat.status == 1, "Invalid status code, actual: " + stat.status + ", expected: 1")
  })
  it('test_error', async () => {
    const stat = compile("../tests/syntax-errors/test_error.arl")
    assert(stat.status == 1, "Invalid status code, actual: " + stat.status + ", expected: 1")
  })
})
  