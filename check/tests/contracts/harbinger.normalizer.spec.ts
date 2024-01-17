/* Imports ----------------------------------------------------------------- */

import { Entrypoint, Int, Nat } from '@completium/archetype-ts-types'
import { expect_to_fail, get_account, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import { asset1, init_data, sign_oracle_data } from './harbinger.oracle.spec'
import { normalizer, queue } from '../../bindings/contracts/harbinger/normalizer'
import { oracle, oracleData_value } from '../../bindings/contracts/harbinger/oracle'

const assert = require('assert')

/* Accounts ---------------------------------------------------------------- */

const alice = get_account('alice');

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Now --------------------------------------------------------------------- */

set_mockup_now(new Date(Date.now()))

/* Utils ------------------------------------------------------------------- */

// Should implement real euclidean division
const quotient = (a: Nat, b: Nat): Nat => {
  return new Nat(a.div(b).floor().to_big_number())
}

const computeVWAP = (high: Nat, low: Nat, close: Nat, volume: Nat): Nat => {
  return (quotient(high.plus(low).plus(close), new Nat(3))).times(volume)
}

/* Data -------------------------------------------------------------------- */

const numDataPoints = 3

let update_entry: Entrypoint

const input0 = new oracleData_value(
  new Date('2020-07-18T22:35:01Z'),
  new Date('2020-07-18T22:35:31Z'),
  new Nat(3059701),
  new Nat(1),
  new Nat(2),
  new Nat(3),
  new Nat(4)
)
const input1 = new oracleData_value(
  new Date('1970-01-01T00:00:01Z'),
  new Date('1970-01-01T00:00:02Z'),
  new Nat(1),
  new Nat(2),
  new Nat(3),
  new Nat(4),
  new Nat(5)
)
const input1_same_date = new oracleData_value(
  new Date('1970-01-01T00:00:01Z'),
  new Date('1970-01-01T00:00:02Z'),
  new Nat(6),
  new Nat(7),
  new Nat(8),
  new Nat(9),
  new Nat(10)
)
const input2 = new oracleData_value(
  new Date('1970-01-01T00:00:03Z'),
  new Date('1970-01-01T00:00:04Z'),
  new Nat(6),
  new Nat(7),
  new Nat(8),
  new Nat(9),
  new Nat(10)
)
const input3 = new oracleData_value(
  new Date('1970-01-01T00:00:05Z'),
  new Date('1970-01-01T00:00:06Z'),
  new Nat(11),
  new Nat(12),
  new Nat(13),
  new Nat(14),
  new Nat(15)
)
const input4 = new oracleData_value(
  new Date('1970-01-01T00:00:07Z'),
  new Date('1970-01-01T00:00:08Z'),
  new Nat(16),
  new Nat(17),
  new Nat(18),
  new Nat(19),
  new Nat(20)
)
const input5 = new oracleData_value(
  new Date('1970-01-01T00:00:09Z'),
  new Date('1970-01-01T00:00:10Z'),
  new Nat(21),
  new Nat(22),
  new Nat(23),
  new Nat(24),
  new Nat(25)
)
const input6 = new oracleData_value(
  new Date('1970-01-01T00:00:11Z'),
  new Date('1970-01-01T00:00:12Z'),
  new Nat(26),
  new Nat(27),
  new Nat(28),
  new Nat(29),
  new Nat(30)
)
const VWAP1 = computeVWAP(input1.high, input1.low, input1.close, input1.volume)
const VWAP2 = computeVWAP(input2.high, input2.low, input2.close, input2.volume)
const VWAP3 = computeVWAP(input3.high, input3.low, input3.close, input3.volume)
const VWAP4 = computeVWAP(input4.high, input4.low, input4.close, input4.volume)
const VWAP5 = computeVWAP(input5.high, input5.low, input5.close, input5.volume)
const VWAP6 = computeVWAP(input6.high, input6.low, input6.close, input6.volume)

/* Scenario ---------------------------------------------------------------- */

describe('[Normalizer] Contracts deployment', async () => {
  it('Deploy Oracle', async () => {
    await oracle.deploy(alice.get_public_key(), init_data, { as: alice })
  });
  it('Deploy Normalizer', async () => {
    const oracle_addr = oracle.get_address()
    if (oracle_addr != undefined) {
      const empty_queue = new queue(new Int(0), new Int(0), new Nat(0), [])
      await normalizer.deploy(["XTZ-USD", "BTC-USD"], oracle_addr, new Nat(numDataPoints), empty_queue, { as: alice })
    } else {
      assert(false)
    }
    const normalizer_addr = normalizer.get_address()
    if (normalizer_addr != undefined) {
      update_entry = new Entrypoint(normalizer_addr, "update")
    } else {
      assert(false)
    }
  });
})

describe('[Normalizer] Update', async () => {
  it('Fails when data is pushed from bad address', async () => {
    expect_to_fail(async () => {
      await normalizer.update([["XTZ-USD", input0]], { as: alice })
    }, normalizer.errors.BAD_SENDER)
  })
  it('Correctly processes updates', async () => {
    const sig1 = await sign_oracle_data(asset1, input1, alice)
    await oracle.update([[asset1, [sig1, input1]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(new Nat(VWAP1.div(input1.volume).floor().to_big_number())))
    } else {
      assert(false)
    }
  })
  it('Update with same time does not update', async () => {
    const sig = await sign_oracle_data(asset1, input1_same_date, alice)
    await oracle.update([[asset1, [sig, input1_same_date]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(quotient(VWAP1, input1.volume)))
      assert(assetMap.prices.saved.length == 1)
    } else {
      assert(false)
    }
  })
  it('Update with input2', async () => {
    const sig = await sign_oracle_data(asset1, input2, alice)
    await oracle.update([[asset1, [sig, input2]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(quotient(VWAP1.plus(VWAP2), input1.volume.plus(input2.volume))))
      assert(assetMap.prices.saved.length == 2)
    } else {
      assert(false)
    }
  })
  it('Update with input3', async () => {
    const sig = await sign_oracle_data(asset1, input3, alice)
    await oracle.update([[asset1, [sig, input3]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(quotient(VWAP1.plus(VWAP2).plus(VWAP3), input1.volume.plus(input2.volume).plus(input3.volume))))
      assert(assetMap.prices.saved.length == 3)
    } else {
      assert(false)
    }
  })
  it('Update with input4', async () => {
    const sig = await sign_oracle_data(asset1, input4, alice)
    await oracle.update([[asset1, [sig, input4]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(quotient(VWAP2.plus(VWAP3).plus(VWAP4), input2.volume.plus(input3.volume).plus(input4.volume))))
      assert(assetMap.prices.saved.length == 3)
    } else {
      assert(false)
    }
  })
  it('Update with input5', async () => {
    const sig = await sign_oracle_data(asset1, input5, alice)
    await oracle.update([[asset1, [sig, input5]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(quotient(VWAP3.plus(VWAP4).plus(VWAP5), input3.volume.plus(input4.volume).plus(input5.volume))))
      assert(assetMap.prices.saved.length == 3)
    } else {
      assert(false)
    }
  })
  it('Update with input6', async () => {
    const sig = await sign_oracle_data(asset1, input6, alice)
    await oracle.update([[asset1, [sig, input6]]], {
      as: alice
    })
    await oracle.push(update_entry, { as: alice })
    const assetMap = await normalizer.get_assetMap_value(asset1)
    if (assetMap != undefined) {
      assert(assetMap.computedPrice.equals(quotient(VWAP4.plus(VWAP5).plus(VWAP6), input4.volume.plus(input5.volume).plus(input6.volume))))
      assert(assetMap.prices.saved.length == 3)
    } else {
      assert(false)
    }
  })
})
