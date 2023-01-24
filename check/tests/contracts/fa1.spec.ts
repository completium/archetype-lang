import { Bytes, Nat, pair_to_mich, string_to_mich } from '@completium/archetype-ts-types'
import { expect_to_fail, get_account, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import assert from 'assert';

/* Contracts */

import { fa1_2 } from '../../bindings/contracts/fa1.2/fa1_2';

/* Accounts ----------------------------------------------------------------- */

const alice = get_account('alice');
const bob   = get_account('bob');
const carl  = get_account('carl');
const user1 = get_account('bootstrap1');


/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Now --------------------------------------------------------------------- */

const now = new Date(Date.now())


/* Constants --------------------------------------------------------------- */

const total_supply = new Nat(1000000)
const metadata_coin = new Bytes('697066733a2f2f516d586a6633734e6848544d523665434e52746d74746b6b50636f6948556a4c4765794b6b436852626d5373726a')


const error_not_enough_allowance = pair_to_mich([string_to_mich("\"NotEnoughAllowance\""), pair_to_mich([new Nat(1).to_mich(), new Nat(0).to_mich()])])
const error_ledger_not_found = pair_to_mich([string_to_mich("\"ASSET_NOT_FOUND\""), string_to_mich("\"ledger\"")])

/* Scenarios --------------------------------------------------------------- */

describe('[FA1.2] Initialization', async () => {
  it('Set time', async () => {
    set_mockup_now(now)
  });
});

describe('[FA1.2] Contracts deployment', async () => {
  it('FA1.2 contract deployment should succeed', async () => {
    await fa1_2.deploy(alice.get_address(), total_supply, metadata_coin, { as: alice })
  });
});

describe('[FA1.2] Transfer', async () => {
  it('Check if balances are right', async () => {
    const alice_balance = await fa1_2.get_ledger_value(alice.get_address());
    assert(alice_balance?.tokens.equals(total_supply), "INVALID_BALANCE");

    const bob_balance = await fa1_2.get_ledger_value(bob.get_address());
    assert(bob_balance === undefined);
  })

  it('Transfer from <> caller without allowance should failed', async () => {
    await expect_to_fail(async () => {
      await fa1_2.transfer(alice.get_address(), bob.get_address(), new Nat(1), { as: bob });
    }, error_not_enough_allowance);
  })

  it('Transfer unkown from should failed', async () => {
    await expect_to_fail(async () => {
      await fa1_2.transfer(bob.get_address(), carl.get_address(), new Nat(1), { as: bob });
    }, error_ledger_not_found);
  })

  it('Transfer from = caller with insufficient balance should failed', async () => {
    await expect_to_fail(async () => {
      await fa1_2.transfer(alice.get_address(), bob.get_address(), total_supply.plus(new Nat(1)), { as: alice });
    }, fa1_2.errors.r1);
  })

  it('Transfer from = call with sufficient balance should succeed', async () => {
    const v = new Nat(total_supply.to_big_number().div(5));
    await fa1_2.transfer(alice.get_address(), bob.get_address(), v, { as: alice });

    const alice_balance = await fa1_2.get_ledger_value(alice.get_address());
    assert(alice_balance?.tokens.equals(new Nat(total_supply.minus(v).to_big_number())), "INVALID_BALANCE");

    const bob_balance = await fa1_2.get_ledger_value(bob.get_address());
    assert(bob_balance?.tokens.equals(v), "INVALID_BALANCE");
  })
});


describe('[FA1.2] Allowance ', async () => {
  it('Approve known caller with 0 spender allowance should succeed', async () => {
    await fa1_2.approve( carl.get_address(), new Nat(0), {as: bob})
  })

  it('Approve unknown caller should succeed', async () => {
    await fa1_2.approve( carl.get_address(), new Nat(10), {as: user1})
  })

  it('Approve known caller with positive spender allowance should failed', async () => {
    await fa1_2.approve(carl.get_address(), new Nat(100), {as: bob})
  })

  it('Approve 0 allowance known caller with positive spender allowance should succeed', async () => {
    await fa1_2.approve(carl.get_address(), new Nat(0), {as: bob})
  })
})

describe('[FA1.2] Allowance and Transfer ', async () => {

  it('Approve and transfer known caller with 0 spender allowance should succeed', async () => {
    const alice_balance_before = await fa1_2.get_ledger_value(alice.get_address());
    const bob_balance_before = await fa1_2.get_ledger_value(bob.get_address());
    const v = 100;

    await fa1_2.approve(carl.get_address(), new Nat(v), {as: alice})

    await fa1_2.transfer(alice.get_address(), bob.get_address(), new Nat(v), {as: carl});

    const alice_balance = await fa1_2.get_ledger_value(alice.get_address());
    let value_alice_balance = alice_balance_before?.tokens.to_big_number().minus(v);
    const vab = value_alice_balance === undefined ? 0 : value_alice_balance
    assert(alice_balance?.tokens.equals(new Nat(vab)), "INVALID_BALANCE");

    const bob_balance = await fa1_2.get_ledger_value(bob.get_address());
    let value_bob_balance = bob_balance_before?.tokens.to_big_number().plus(v);
    const vbb = value_bob_balance === undefined ? 0 : value_bob_balance
    assert(bob_balance?.tokens.equals(new Nat(vbb)), "INVALID_BALANCE");
  })

})

describe('[FA1.2] Getters', async () => {

  it('Check getAllowance values', async () => {
    const value = await fa1_2.getAllowance(user1.get_address(), carl.get_address(), {as: alice} );
    assert(value.equals(new Nat(10)), "Invalid value for getAllowance")
  })

  it('Check getBalance values', async () => {
    const alice_balance = await fa1_2.get_ledger_value(alice.get_address());

    const value = await fa1_2.getBalance(alice.get_address(), {as: alice});
    assert(alice_balance?.tokens.equals(value), "Invalid value for getBalance")
  })

  it('Check getTotalSupply value', async () => {
    const value = await fa1_2.getTotalSupply({as: alice});
    assert(value.equals(total_supply), "Invalid value for getTotalSupply")
  })
})
