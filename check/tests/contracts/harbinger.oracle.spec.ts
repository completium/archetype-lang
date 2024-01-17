/* Imports ----------------------------------------------------------------- */

import { Key, Nat, Option, option_to_mich_type, pair_array_to_mich_type, pair_to_mich, prim_to_mich_type, string_to_mich } from '@completium/archetype-ts-types'
import { Account, expect_to_fail, get_account, pack, set_mockup, set_mockup_now, set_quiet, sign } from '@completium/experiment-ts'

const assert = require('assert')

import {
  oracle,
  oracleData_value,
  states,
  oracleData_value_mich_type
} from '../../bindings/contracts/harbinger/oracle'

/* Accounts ---------------------------------------------------------------- */

const alice = get_account('alice');
const bob = get_account('bob');

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Now --------------------------------------------------------------------- */

set_mockup_now(new Date(Date.now()))

/* Utils ------------------------------------------------------------------- */

export const sign_oracle_data = async (key: string, data: oracleData_value, signer: Account) => {
  const value = pair_to_mich([string_to_mich(key), data.to_mich()])
  const type = pair_array_to_mich_type([prim_to_mich_type("string"), oracleData_value_mich_type])
  const packed = pack(value, type)
  return await sign(packed, signer)
}

export const sign_oracle_revoke = async (signer: Account) => {
  const value: Option<Key> = Option.None();
  const type = option_to_mich_type(prim_to_mich_type("key"))
  const packed = pack(value.to_mich(((x: Key) => { return x.to_mich() })), type)
  return await sign(packed, signer)
}

/* Data -------------------------------------------------------------------- */

export const asset1 = "XTZ-USD"
export const asset2 = "BTC-USD"
const asset_untracked = "XTZ-BTC"
const input1 = new oracleData_value(
  new Date('1970-01-01T00:00:01Z'),
  new Date('1970-01-01T00:00:02Z'),
  new Nat(3),
  new Nat(4),
  new Nat(5),
  new Nat(6),
  new Nat(7)
)
const input2 = new oracleData_value(
  new Date('1970-01-01T00:00:08Z'),
  new Date('1970-01-01T00:00:09Z'),
  new Nat(10),
  new Nat(11),
  new Nat(12),
  new Nat(13),
  new Nat(14)
)
const input_past = new oracleData_value(
  new Date('1970-01-01T00:00:08Z'),
  new Date('1970-01-01T00:00:09Z'),
  new Nat(15),
  new Nat(16),
  new Nat(17),
  new Nat(18),
  new Nat(19)
)
const input3 = new oracleData_value(
  new Date('1970-01-01T00:00:15Z'),
  new Date('1970-01-01T00:00:16Z'),
  new Nat(17),
  new Nat(18),
  new Nat(19),
  new Nat(20),
  new Nat(21)
)
const input4 = new oracleData_value(
  new Date('1970-01-01T00:00:22Z'),
  new Date('1970-01-01T00:00:23Z'),
  new Nat(24),
  new Nat(25),
  new Nat(26),
  new Nat(27),
  new Nat(28)
)

const init_date = new Date('1970-01-01')
const zero = new Nat(0)
const init_data_value : [ Date, Date, Nat, Nat, Nat, Nat, Nat] = [init_date, init_date, zero, zero, zero, zero, zero]
export const init_data : Array<[string, [ Date, Date, Nat, Nat, Nat, Nat, Nat]]> = [[asset2, init_data_value], [asset1, init_data_value]]

/* Scenario ---------------------------------------------------------------- */

describe('[Oracle] Contract deployment', async () => {
  it('Deploy Oracle', async () => {
    await oracle.deploy(alice.get_public_key(), init_data, { as: alice })
  });
})

describe('[Oracle] Update', async () => {
  it('Update once with valid data', async () => {
    const sig = await sign_oracle_data(asset1, input1, alice)
    await oracle.update([[asset1, [sig, input1]]], {
      as: alice
    })
    const output = await oracle.get_oracleData_value(asset1);
    (output != undefined) ? assert(output.equals(input1)) : assert(false)
  })
  it('Second Update Overwrites First Update', async () => {
    const sig = await sign_oracle_data(asset1, input2, alice)
    await oracle.update([[asset1, [sig, input2]]], {
      as: alice
    })
    const output = await oracle.get_oracleData_value(asset1);
    (output != undefined) ? assert(output.equals(input2)) : assert(false)
  })
  it('Correctly Processes Updates With Data From The Past', async () => {
    const sig = await sign_oracle_data(asset1, input_past, alice)
    await oracle.update([[asset1, [sig, input_past]]], {
      as: alice
    })
    const output = await oracle.get_oracleData_value(asset1);
    (output != undefined) ? assert(output.equals(input2)) : assert(false)
  })
  it('Untracked Asset does not update oracle', async () => {
    const sig = await sign_oracle_data(asset_untracked, input1, alice)
    await oracle.update([[asset_untracked, [sig, input1]]], {
      as: alice
    })
    const output = await oracle.get_oracleData_value(asset_untracked);
    assert(output == undefined)
  })
  it('Update Fails With Bad Signature', async () => {
    const sig = await sign_oracle_data(asset1, input3, bob)
    expect_to_fail(async () => {
      await oracle.update([[asset1, [sig, input3]]], {
        as: alice
      })
    }, oracle.errors.BAD_SIG)
  })
  it('Update with stale asset does not fail', async () => {
    const sig1 = await sign_oracle_data(asset1, input3, alice)
    const sig2 = await sign_oracle_data(asset2, input1, alice)
    await oracle.update([[asset2, [sig2, input1]], [asset1, [sig1, input3]],], {
      as: alice
    })
    const output1 = await oracle.get_oracleData_value(asset1);
    (output1 != undefined) ? assert(output1.equals(input3)) : assert(false)
    const output2 = await oracle.get_oracleData_value(asset2);
    (output2 != undefined) ? assert(output2.equals(input1)) : assert(false)
    const sig3 = await sign_oracle_data(asset1, input4, alice)
    await oracle.update([[asset2, [sig2, input1]], [asset1, [sig3, input4]]], {
      as: alice
    })
    const output3 = await oracle.get_oracleData_value(asset1);
    (output3 != undefined) ? assert(output3.equals(input4)) : assert(false)
    const output4 = await oracle.get_oracleData_value(asset2);
    (output4 != undefined) ? assert(output4.equals(input1)) : assert(false)
  })
})

describe('[Oracle] Revoke', async () => {
  it('Incorrect Revoke Fails to Revoke An Oracle', async () => {
    const sig = await sign_oracle_revoke(bob)
    expect_to_fail(async () => {
      await oracle.revoke(sig, { as: alice })
    }, oracle.errors.BAD_SIG)
  })
  it('Revoke Oracle', async () => {
    const sig = await sign_oracle_revoke(alice)
    await oracle.revoke(sig, { as: alice })
    const state = await oracle.get_state()
    assert(state == states.Revoked)
    const output = await oracle.get_oracleData_value(asset1);
    assert(output == undefined)
  })
  it('Update Fails when Revoked', async () => {
    const sig = await sign_oracle_data(asset1, input3, alice)
    expect_to_fail(async () => {
      await oracle.update([[asset1, [sig, input3]]], {
        as: alice
      })
    }, oracle.errors.REVOKED)
  })
})
