import { Bytes, Key, Nat, Option, Or, pair_to_mich, Signature, string_to_mich } from '@completium/archetype-ts-types'
import { blake2b, expect_to_fail, get_account, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import { get_packed_transfer_params, get_transfer_permit_data, get_missigned_error, wrong_packed_transfer_params, wrong_sig } from './fa2.utils'

import assert from 'assert';

/* Contracts */

import { ledger_key, balance_of_request, fa2_multi, gasless_param, operator_key, operator_param, transfer_destination, transfer_param } from '../../bindings/contracts/fa2/fa2_multi';
import { add, permits, permits_value, user_permit } from '../../bindings/contracts/fa2/permits';


/* Accounts ----------------------------------------------------------------- */

const alice = get_account('alice');
const bob = get_account('bob');
const carl = get_account('carl');
const user1 = get_account('bootstrap1');
const user2 = get_account('bootstrap2');

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Now --------------------------------------------------------------------- */

const now = new Date("2022-01-01")

/* Constants & Utils ------------------------------------------------------- */

const amount = new Nat(1000);
const token_id = new Nat(0);
const a_token_id = new Nat(1)
const expiry = new Nat(31556952)

const testAmount_1 = new Nat(1);
const testAmount_2 = new Nat(11);
let alicePermitNb = new Nat(0);
let bobPermitNb = new Nat(0);
let carlPermitNb = new Nat(0);

const error_key_exists_ledger = pair_to_mich([string_to_mich("\"KEY_EXISTS\""), string_to_mich("\"ledger\"")])
const error_permit_expired = (v: number) => pair_to_mich([string_to_mich("\"PERMIT_EXPIRED\""), new Nat(v).to_mich()])
const get_ref_user_permits = (counter: Nat, data: Bytes, expiry: Nat, now: Date) => {
  return new permits_value(counter, Option.None<Nat>(), [[
    blake2b(data),
    new user_permit(Option.Some<Nat>(expiry), new Date(now.getTime() - now.getMilliseconds()))
  ]])
}

/* Scenarios --------------------------------------------------------------- */

describe('[FA2 multi-asset] Initialization', async () => {
  it('Set time', async () => {
    set_mockup_now(now)
  });
});

describe('[FA2 multi-asset] Contracts deployment', async () => {
  it('Permits contract deployment should succeed', async () => {
    await permits.deploy(alice.get_address(), { as: alice })
  });
  it('FA2 multi-asset contract deployment should succeed', async () => {
    await fa2_multi.deploy(alice.get_address(), permits.get_address(), { as: alice })
  });
});

describe('[FA2 multi-asset] Contract configuration', async () => {
  it("Add FA2 as permit consumer", async () => {
    await permits.manage_consumer(new add(fa2_multi.get_address()), { as: alice })
  })

  it("Add token_id 0 & 1", async () => {
    await fa2_multi.set_token_metadata(token_id, [["", new Bytes("")]], {as: alice})
    await fa2_multi.set_token_metadata(a_token_id, [["", new Bytes("")]], {as: alice})
  })
})

describe('[FA2 multi-asset] Minting', async () => {
  it('Mint tokens as owner for ourself should succeed', async () => {
    await fa2_multi.mint(
      alice.get_address(),      // owner
      token_id,                 // token id
      amount, {                 // amount
      as: alice,
    }
    );
  });

  it('Mint tokens as non owner for ourself should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_multi.mint(
        bob.get_address(),      // owner
        token_id,               // token id
        amount, {               // amount
        as: bob,
      }
      );
    }, fa2_multi.errors.INVALID_CALLER);
  });

  it('Mint tokens as non owner for someone else should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_multi.mint(
        carl.get_address(),      // owner
        token_id,                // token id
        amount, {                // amount
        as: bob,
      }
      );
    }, fa2_multi.errors.INVALID_CALLER);
  });

  it('Mint tokens as owner for someone else should succeed', async () => {
    const balance_before = await fa2_multi.get_ledger_value(new ledger_key(carl.get_address(), a_token_id))
    assert(balance_before == undefined)

    await fa2_multi.mint(
      carl.get_address(),   // owner
      a_token_id,           // token id
      amount, {             // amount
      as: alice,
    }
    );

    const balance_after = await fa2_multi.get_ledger_value(new ledger_key(carl.get_address(), a_token_id))
    assert(balance_after?.equals(amount))
  });

  it('Mint token for user 1', async () => {
    const a_token_id = new Nat(1)

    const balance_before = await fa2_multi.get_ledger_value(new ledger_key(user1.get_address(), a_token_id))
    assert(balance_before == undefined)

    await fa2_multi.mint(
      user1.get_address(),      // owner
      a_token_id,               // token id
      new Nat(2), {             // amount
      as: alice,
    }
    );

    const balance_after = await fa2_multi.get_ledger_value(new ledger_key(user1.get_address(), a_token_id))
    assert(balance_after?.equals(new Nat(2)))

  });
});

describe('[FA2 multi-asset] Transfers', async () => {
  it('Transfer simple amount of token', async () => {
    const a_token_id = new Nat(1)

    const balance_before_user1 = await fa2_multi.get_ledger_value(new ledger_key(user1.get_address(), a_token_id))
    assert(balance_before_user1?.equals(new Nat(2)))
    const balance_before_user2 = await fa2_multi.get_ledger_value(new ledger_key(user2.get_address(), a_token_id))
    assert(balance_before_user2 == undefined)

    const tps = [new transfer_param(user1.get_address(), [new transfer_destination(user2.get_address(), a_token_id, new Nat(1))])]

    await fa2_multi.transfer(tps, { as: user1 })

    const balance_after_user1 = await fa2_multi.get_ledger_value(new ledger_key(user1.get_address(), a_token_id))
    assert(balance_after_user1?.equals(new Nat(1)))
    const balance_after_user2 = await fa2_multi.get_ledger_value(new ledger_key(user2.get_address(), a_token_id))
    assert(balance_after_user2?.equals(new Nat(1)))
  })
})

describe('[FA2 multi-asset] Balance of', async () => {

  it('Unknown token_id should fail', async () => {
    const fake_token = new Nat(56)
    await expect_to_fail(async () => {
      await fa2_multi.balance_of([new balance_of_request(alice.get_address(), fake_token)], { as: alice })
    }, fa2_multi.errors.FA2_TOKEN_UNDEFINED)
  });

});