import { get_account, set_mockup, set_quiet } from '@completium/experiment-ts';

import * as simple_assign1 from '../bindings/passed/simple_assign1'

import assert from 'assert'
import { Nat } from '@completium/archetype-ts-types';

const alice = get_account('alice')

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Tests-------------------------------------------------------------------- */

describe('Tests', async () => {
  describe('Passed', async () => {
    it('Simple_assign1', async () => {
      await simple_assign1.simple_assign1.deploy({ as: alice })
      const n_before = await simple_assign1.simple_assign1.get_n();
      assert(n_before.equals(new Nat(0)), "Invalid Value")
      await simple_assign1.simple_assign1.exec({as : alice})
      const n_after = await simple_assign1.simple_assign1.get_n();
      assert(n_after.equals(new Nat(1)), "Invalid Value")
    })
  })
})
