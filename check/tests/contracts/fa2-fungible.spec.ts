import { Bytes, Key, MichelineType, Nat, Option, Or, pair_to_mich, Signature, string_to_mich } from '@completium/archetype-ts-types'
import { blake2b, expect_to_fail, get_account, get_chain_id, pack, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import { get_packed_transfer_params, get_transfer_permit_data, get_missigned_error, wrong_packed_transfer_params, wrong_sig } from './fa2.utils'

import assert from 'assert';

/* Contracts */

import { add_operator, balance_of_request, fa2_fungible, gasless_param, operator_key, operator_param, remove_operator, transfer_destination, transfer_param } from '../../bindings/contracts/fa2/fa2_fungible';
import { add, permits, permits_value, user_permit } from '../../bindings/contracts/fa2/permits';

/* Accounts ----------------------------------------------------------------- */

const alice = get_account('alice');
const bob = get_account('bob');
const carl = get_account('carl');
const user1 = get_account('bootstrap1');
const user2 = get_account('bootstrap2');
const user3 = get_account('bootstrap3');
const user4 = get_account('bootstrap4');
const user5 = get_account('bootstrap5');

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Now --------------------------------------------------------------------- */

const now = new Date("2022-01-01")

/* Constants & Utils ------------------------------------------------------- */

const token_id = new Nat(0)
const amount = new Nat(123)
const expiry = new Nat(31556952)

const get_ref_user_permits = (counter: Nat, data: Bytes, expiry: Nat, now: Date) => {
  return new permits_value(counter, Option.None<Nat>(), [[
    blake2b(data),
    new user_permit(Option.Some<Nat>(expiry), new Date(now.getTime() - now.getMilliseconds()))
  ]])
}

/* Scenarios --------------------------------------------------------------- */

describe('[FA2 fungible] Initialization', async () => {
  it('Set time', async () => {
    set_mockup_now(now)
  });
});

describe('[FA2 fungible] Contracts deployment', async () => {
  it('Permits contract deployment should succeed', async () => {
    await permits.deploy(alice.get_address(), { as: alice })
  });
  it('FA2 fungible contract deployment should succeed', async () => {
    await fa2_fungible.deploy(alice.get_address(), permits.get_address(), { as: alice })
  });
});

describe('[FA2 fungible] Contract configuration', async () => {
  it("Add FA2 as permit consumer", async () => {
    await permits.manage_consumer(new add(fa2_fungible.get_address()), { as: alice })
  })
})

describe('[FA2 fungible] Minting', async () => {
  it('Mint tokens as owner for ourself should succeed', async () => {
    const balance_alice_before = await fa2_fungible.get_ledger_value(alice.get_address())
    assert(balance_alice_before?.equals(new Nat('123000000000000')), "Invalid amount")

    await fa2_fungible.mint(alice.get_address(), new Nat(1000), { as: alice })

    const balance_alice_after = await fa2_fungible.get_ledger_value(alice.get_address())
    assert(balance_alice_after?.equals(new Nat('123000000001000')), "Invalid amount")
  });

  it('Mint tokens as non owner for ourself should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.mint(bob.get_address(), new Nat(1000), { as: bob })
    }, fa2_fungible.errors.INVALID_CALLER);
  });

  it('Mint tokens as non owner for someone else should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.mint(carl.get_address(), new Nat(1000), { as: bob })
    }, fa2_fungible.errors.INVALID_CALLER);
  });

  it('Mint tokens as owner for someone else should succeed', async () => {
    const balance_carl_before = await fa2_fungible.get_ledger_value(carl.get_address())
    assert(balance_carl_before == undefined, "Invalid amount")

    await fa2_fungible.mint(carl.get_address(), new Nat(1000), { as: alice })

    const balance_carl_after = await fa2_fungible.get_ledger_value(carl.get_address())
    assert(balance_carl_after?.equals(new Nat(1000)), "Invalid amount")
  });

  it('Mint token for user 1', async () => {
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    assert(balance_user1_before == undefined, "Invalid amount")

    await fa2_fungible.mint(user1.get_address(), new Nat(1), { as: alice })

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    assert(balance_user1_after?.equals(new Nat(1)), "Invalid amount")
  });
});

describe('[FA2 fungible] Update operators', async () => {
  it('Add an operator for ourself should succeed', async () => {
    const op_key = new operator_key(fa2_fungible.get_address(), token_id, alice.get_address())
    const has_operator_before = await fa2_fungible.has_operator_value(op_key)
    assert(has_operator_before == false)
    await fa2_fungible.update_operators([
      new add_operator(new operator_param(alice.get_address(), fa2_fungible.get_address(), token_id))
    ], { as: alice })
    const has_operator_after = await fa2_fungible.has_operator_value(op_key)
    assert(has_operator_after == true)
  });

  it('Remove a non existing operator should succeed', async () => {
    await fa2_fungible.update_operators([
      new remove_operator(new operator_param(alice.get_address(), bob.get_address(), token_id))
    ], { as: alice })
  });

  it('Remove an existing operator for another user should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.update_operators([
        new remove_operator(new operator_param(alice.get_address(), fa2_fungible.get_address(), token_id))
      ], { as: bob })
    }, fa2_fungible.errors.FA2_NOT_OWNER);
  });

  it('Add operator for another user should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.update_operators([
        new add_operator(new operator_param(bob.get_address(), fa2_fungible.get_address(), token_id))
      ], { as: alice });
    }, fa2_fungible.errors.FA2_NOT_OWNER);
  });

  it('Remove an existing operator should succeed', async () => {
    const op_key = new operator_key(fa2_fungible.get_address(), token_id, alice.get_address())
    const has_operator_before = await fa2_fungible.has_operator_value(op_key)
    assert(has_operator_before == true)
    await fa2_fungible.update_operators([
      new remove_operator(new operator_param(alice.get_address(), fa2_fungible.get_address(), token_id))
    ], { as: alice })
    const has_operator_after = await fa2_fungible.has_operator_value(op_key)
    assert(has_operator_after == false)
  });
});

describe('[FA2 fungible] Add permit', async () => {
  it('Add a permit with the wrong signature should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id();
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    await expect_to_fail(async () => {
      await permits.permit(new Key(alice.pubk), wrong_sig, blake2b(packed_transfer_params), { as: bob })
    }, get_missigned_error(permit_data))

  });

  it('Add a permit with the wrong hash should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id();

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);

    const wrong_permit_data = get_transfer_permit_data(
      wrong_packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);

    await expect_to_fail(async () => {
      const sig = await alice.sign(blake2b(permit_data))
      await permits.permit(new Key(alice.pubk), sig, blake2b(wrong_packed_transfer_params), { as: bob })
    }, get_missigned_error(wrong_permit_data));
  });

  it('Add a permit with the wrong public key should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id();
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);

    await expect_to_fail(async () => {
      const sig = await alice.sign(blake2b(permit_data))
      await permits.permit(new Key(bob.pubk), sig, blake2b(packed_transfer_params), { as: bob })
    }, get_missigned_error(permit_data));
  });

  it('Add a permit with the good hash, signature and public key should succeed', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id();
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)
    await permits.permit(new Key(alice.pubk), sig, blake2b(packed_transfer_params), { as: bob })

    const added_permit = await permits.get_permits_value(alice.get_address())
    assert(added_permit?.equals(get_ref_user_permits(new Nat(1), packed_transfer_params, expiry, now)))
  });

  it('Add a duplicated permit should fail', async () => {
    const initial_permit = await permits.get_permits_value(alice.get_address())

    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id();

    assert(initial_permit?.equals(get_ref_user_permits(new Nat(1), packed_transfer_params, expiry, now)))
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)
    await expect_to_fail(async () => {
      await permits.permit(new Key(alice.pubk), sig, blake2b(packed_transfer_params), { as: bob })
    }, pair_to_mich([string_to_mich("DUP_PERMIT"), blake2b(packed_transfer_params).to_mich()]))
  });

});

describe('[FA2 fungible] Transfers', async () => {
  it('Transfer simple amount of token', async () => {
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_before?.equals(new Nat(1)), "Invalid amount user 1")
    assert(balance_user2_before == undefined, "Invalid amount user 2")

    await fa2_fungible.transfer([new transfer_param(
      user1.get_address(),
      [new transfer_destination(user2.get_address(), token_id, new Nat(1))])],
      { as: user1 }
    );

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount after user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount after user2")
  });

  it('Transfer a token from another user without a permit or an operator should fail', async () => {
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    await expect_to_fail(async () => {
      await fa2_fungible.transfer([new transfer_param(
        user1.get_address(),
        [new transfer_destination(user2.get_address(), token_id, new Nat(1))])],
        { as: user2 }
      );
    }, fa2_fungible.errors.FA2_NOT_OPERATOR);

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount after user 1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount after user 2")
  });

  it('Transfer more tokens than owned should fail', async () => {
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    await expect_to_fail(async () => {
      await fa2_fungible.transfer([new transfer_param(
        user1.get_address(),
        [new transfer_destination(user2.get_address(), token_id, new Nat(2))])],
        { as: user1 }
      );
    }, fa2_fungible.errors.FA2_INSUFFICIENT_BALANCE);

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount after user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount after user2")
  });

  it('Transfer tokens with an operator', async () => {
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    await fa2_fungible.update_operators([
      new add_operator(new operator_param(user2.get_address(), user3.get_address(), token_id))
    ],
      { as: user2 }
    );

    await fa2_fungible.transfer([new transfer_param(
      user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, new Nat(1))])],
      { as: user3 }
    );

    await fa2_fungible.update_operators([
      new remove_operator(new operator_param(user2.get_address(), user3.get_address(), token_id))
    ],
      { as: user2 }
    );

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after?.equals(new Nat(1)), "Invalid amount after user1")
    assert(balance_user2_after == undefined, "Invalid amount after user2")
  });

});

describe('[FA2 fungible] Transfers gasless ', async () => {
  it('Transfer gasless simple amount of token', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user1.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before?.equals(new Nat(1)), "Invalid amount user1")
    assert(balance_user2_before == undefined, "Invalid amount user2")

    const tps = [new transfer_param(user1.get_address(),
      [new transfer_destination(user2.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const after_permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user1.sign(after_permit_data)
    await fa2_fungible.transfer_gasless([
      new gasless_param(tps, user1.get_public_key(), sig)
    ], { as: user3 }
    )

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount after user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount after user2")
  });

  it('Transfer a token from another user with wrong permit should fail', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user2.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user1.sign(permit_data)

    await expect_to_fail(async () => {
      await fa2_fungible.transfer_gasless([
        new gasless_param(tps, user2.get_public_key(), sig)
      ], { as: user3 }
      )
    }, get_missigned_error(permit_data));

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount user2")
  });

  it('Transfer tokens gasless with from different signer should fail', async () => {
    await fa2_fungible.mint(user4.get_address(), new Nat(1), { as: alice })

    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user4.get_address())
    const counter = permit?.counter
    const balance_user4_before = await fa2_fungible.get_ledger_value(user4.get_address())
    const balance_user5_before = await fa2_fungible.get_ledger_value(user5.get_address())
    const chain_id = await get_chain_id();
    assert(balance_user4_before?.equals(new Nat(1)), "Invalid amount user4")
    assert(balance_user5_before == undefined, "Invalid amount user5")

    const tps = [new transfer_param(user4.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const after_permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user3.sign(after_permit_data)

    await expect_to_fail(async () => {
      await fa2_fungible.transfer_gasless([
        new gasless_param(tps, user3.get_public_key(), sig)
      ], { as: user3 }
      )
    }, fa2_fungible.errors.SIGNER_NOT_FROM);
  });

  it('Transfer gasless should succed', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user2.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const after_permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user2.sign(after_permit_data)
    await fa2_fungible.transfer_gasless([
      new gasless_param(tps, user2.get_public_key(), sig)
    ], { as: user3 }
    )

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after?.equals(new Nat(1)), "Invalid amount after user1")
    assert(balance_user2_after == undefined, "Invalid amount after user2")
  });
});

describe('[FA2 fungible] Transfers one-step ', async () => {
  it('Transfer one-step simple amount of token', async () => {
    const permit = await permits.get_permits_value(user1.get_address())
    const counter = permit?.counter
    const amount = new Nat(1);
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before?.equals(new Nat(1)), "Invalid amount user1")
    assert(balance_user2_before == undefined, "Invalid amount user2")
    const tps = [new transfer_param(user1.get_address(),
      [new transfer_destination(user2.get_address(), token_id, amount)])
    ]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user1.sign(permit_data);
    const lpermit = Option.Some<[Key, Signature]>([user1.get_public_key(), sig]);
    await fa2_fungible.permit_transfer(tps, lpermit, { as: user3 })

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount after user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount after user2")
  });

  it('Transfer one-step a token from another user with wrong permit should fail', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user2.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user1.sign(permit_data)
    const lpermit = Option.Some<[Key, Signature]>([user2.get_public_key(), sig]);

    await expect_to_fail(async () => {
      await fa2_fungible.permit_transfer(tps, lpermit, { as: user3 })
    }, get_missigned_error(permit_data));

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount user2")
  });

  it('Transfer one-step tokens with from different signer should fail', async () => {
    // await fa2_fungible.mint(user4.get_address(), new Nat(1), { as: alice })

    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user4.get_address())
    const counter = permit?.counter
    const balance_user4_before = await fa2_fungible.get_ledger_value(user4.get_address())
    const balance_user5_before = await fa2_fungible.get_ledger_value(user5.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user4_before?.equals(new Nat(1)), "Invalid amount user4")
    assert(balance_user5_before == undefined, "Invalid amount user5")

    const tps = [new transfer_param(user4.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const after_permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user3.sign(after_permit_data)
    const lpermit = Option.Some<[Key, Signature]>([user3.get_public_key(), sig]);

    await expect_to_fail(async () => {
      await fa2_fungible.permit_transfer(tps, lpermit, { as: user3 })
    }, fa2_fungible.errors.SIGNER_NOT_FROM);
  });

  it('Transfer one-step without permit (None value) should succed', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user2.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    // const packed_transfer_params = get_packed_transfer_params(tps)
    // const after_permit_data = get_transfer_permit_data(
    //   packed_transfer_params,
    //   permits.get_address(),
    //   counter);
    // const sig = await user2.sign(after_permit_data)
    // const lpermit = Option.Some<[Key, Signature]>([user2.get_public_key(), sig]);

    await fa2_fungible.permit_transfer(tps, Option.None<[Key, Signature]>(), { as: user2 })

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after?.equals(new Nat(1)), "Invalid amount after user1")
    assert(balance_user2_after == undefined, "Invalid amount after user2")
  });

});

describe('[FA2 fungible] Consume permit', async () => {

  it('Set global expiry with too big value should fail', async () => {
    await expect_to_fail(async () => {
      await permits.set_expiry(
        Option.Some<Nat>(new Nat('999999999999999999999999999999999999999')),
        Option.None(),
        { as: alice }
      );
    }, permits.errors.r2);
  });

  it('Simple transfer with permit', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user1.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before?.equals(new Nat(1)), "Invalid amount user1")
    assert(balance_user2_before == undefined, "Invalid amount user2")

    const tps = [new transfer_param(user1.get_address(),
      [new transfer_destination(user2.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user1.sign(permit_data)

    await permits.permit(user1.get_public_key(), sig, blake2b(packed_transfer_params), { as: bob })

    const permit_after = await permits.get_permits_value(user1.get_address())
    assert(permit_after?.user_permits.length == 1, "Invalid user permits")

    await fa2_fungible.transfer(tps, { as: user3 })

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after == undefined, "Invalid amount user1")
    assert(balance_user2_after?.equals(new Nat(1)), "Invalid amount user2")
  });

  it('Set expiry for an existing permit with too big value should fail', async () => {
    const amount = new Nat(1);
    const permit = await permits.get_permits_value(user2.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(new Nat(1)), "Invalid amount user2")

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user2.sign(permit_data)

    await permits.permit(user2.get_public_key(), sig, blake2b(packed_transfer_params), { as: user2 })

    await expect_to_fail(async () => {
      await permits.set_expiry(
        Option.Some<Nat>(new Nat('999999999999999999999999999999999999999')),
        Option.Some<Bytes>(blake2b(packed_transfer_params)),
        { as: alice }
      )
    }, permits.errors.r2);
  });

  it('Set expiry with a correct value should succeed', async () => {
    await fa2_fungible.mint(user2.get_address(), new Nat(1), { as: alice })

    const amount = new Nat(2);
    const permit = await permits.get_permits_value(user2.get_address())
    const counter = permit?.counter
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_before = await fa2_fungible.get_ledger_value(user2.get_address())
    const chain_id = await get_chain_id()
    assert(balance_user1_before == undefined, "Invalid amount user1")
    assert(balance_user2_before?.equals(amount), "Invalid amount user2")

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user2.sign(permit_data)

    set_mockup_now(now)

    await permits.permit(user2.get_public_key(), sig, blake2b(packed_transfer_params), { as: user2 })

    const expiry = new Nat(3600);

    await permits.set_expiry(Option.Some(expiry), Option.Some(blake2b(packed_transfer_params)), { as: user2 })

    //set_mockup_now(new Date(now.setSeconds(now.getSeconds() + expiry.to_big_number().toNumber() + 10)))

    //await expect_to_fail(async () => {
    //  await fa2_fungible.transfer(tps, { as: user3 });
    //}, permits.errors.p7);

    set_mockup_now(now)

    await fa2_fungible.transfer(tps, { as: user3 });

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    const balance_user2_after = await fa2_fungible.get_ledger_value(user2.get_address())
    assert(balance_user1_after?.equals(amount), "Invalid amount user1")
    assert(balance_user2_after == undefined, "Invalid amount user2")
  });

  it('Set expiry to 0 (permit gets deleted) should succeed', async () => {
    const amount = new Nat(12);
    const permit = await permits.get_permits_value(carl.get_address())
    assert(permit == undefined, "Carl's permit should be undefined")
    const counter = new Nat(0)
    const chain_id = await get_chain_id()

    const tps = [new transfer_param(carl.get_address(),
      [new transfer_destination(bob.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);

    const sig = await carl.sign(permit_data)
    await permits.permit(carl.get_public_key(), sig, blake2b(packed_transfer_params), { as: carl })

    const added_permit = await permits.get_permits_value(carl.get_address())
    assert(added_permit?.equals(get_ref_user_permits(new Nat(1), packed_transfer_params, expiry, now)))

    await permits.set_expiry(Option.Some(new Nat(0)), Option.Some(blake2b(packed_transfer_params)), { as: carl })

    const final_permit = await permits.get_permits_value(carl.get_address())

    assert(final_permit?.equals(new permits_value(new Nat(1), Option.None<Nat>(), [])))
  });

});

describe('[FA2 fungible] Set metadata', async () => {
  it('Set metadata with empty content should succeed', async () => {
    const metadata_before = await fa2_fungible.get_metadata_value("key")
    assert(metadata_before == undefined);

    await fa2_fungible.set_metadata("key", Option.Some(new Bytes("")), { as: alice })

    const metadata_after = await fa2_fungible.get_metadata_value("key")
    assert(metadata_after?.equals(new Bytes("")));
  });

  it('Set metadata called by not owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.set_metadata("key", Option.Some(new Bytes("")), { as: bob })
    }, fa2_fungible.errors.INVALID_CALLER);
  });

  it('Set metadata with valid content should succeed', async () => {
    const data = new Bytes('697066733a2f2f516d617635756142437a4d77377871446f55364d444534743473695855484e4737664a68474c746f79774b35694a');
    const metadata_before = await fa2_fungible.get_metadata_value("key")
    assert(metadata_before?.equals(new Bytes("")), "Invalid metadata before");

    await fa2_fungible.set_metadata("key", Option.Some(data), { as: alice })

    const metadata_after = await fa2_fungible.get_metadata_value("key")
    assert(metadata_after?.equals(data));
  });
});

describe('[FA2 fungible] Burn', async () => {
  it('Burn token should succeed', async () => {
    const balance_user1_before = await fa2_fungible.get_ledger_value(user1.get_address())
    assert(balance_user1_before?.equals(new Nat(2)), "Invalid amount user1")

    await fa2_fungible.burn(new Nat(2), { as: user1 });

    const balance_user1_after = await fa2_fungible.get_ledger_value(user1.get_address())
    assert(balance_user1_after === undefined, "Invalid amount")
  });

  it('Burn without tokens should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.burn(new Nat(1), { as: user1 });
    }, fa2_fungible.errors.FA2_INSUFFICIENT_BALANCE);
  });

  it('Burn tokens with a partial amount of tokens should succeed', async () => {
    const amount = new Nat(500)
    const balance_user1_before = await fa2_fungible.get_ledger_value(carl.get_address())

    await fa2_fungible.burn(amount, { as: carl });

    const balance_user1_after = await fa2_fungible.get_ledger_value(carl.get_address())
    assert(balance_user1_after?.plus(amount).equals(balance_user1_before ? balance_user1_before : new Nat(0)), "Invalid value")
  });

  it('Burn tokens with more tokens owned should fail', async () => {
    const balance_carl_before = await fa2_fungible.get_ledger_value(carl.get_address())
    assert(balance_carl_before?.equals(new Nat(500)), "Invalid amount")

    await expect_to_fail(async () => {
      await fa2_fungible.burn(new Nat(1000), { as: carl });
    }, fa2_fungible.errors.FA2_INSUFFICIENT_BALANCE);

    const balance_carl_after = await fa2_fungible.get_ledger_value(carl.get_address())
    assert(balance_carl_after?.equals(new Nat(500)), "Invalid amount")
  });

});

describe('[FA2 fungible] Pause', async () => {
  it('Set FA2 on pause should succeed', async () => {
    await fa2_fungible.pause({ as: alice });
    const is_paused = await fa2_fungible.get_paused()
    assert(is_paused);
  });
  it('Set Permits on pause should succeed', async () => {
    await permits.pause({ as: alice });
    const is_paused = await fa2_fungible.get_paused()
    assert(is_paused);
  });

  it('Minting is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.mint(alice.get_address(), new Nat(1000), { as: alice })
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Update operators is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.update_operators([
        new add_operator(new operator_param(alice.get_address(), fa2_fungible.get_address(), token_id))
      ], { as: alice })
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Add permit is not possible when contract is paused should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    await expect_to_fail(async () => {
      await permits.permit(alice.get_public_key(), sig, packed_transfer_params, { as: alice });
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Transfer is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.transfer([new transfer_param(
        user1.get_address(),
        [new transfer_destination(user2.get_address(), token_id, new Nat(1))])],
        { as: user1 });
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Set metadata is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.set_metadata("key", Option.Some(new Bytes("")), { as: alice })
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Set expiry is not possible when contract is paused should fail', async () => {
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)

    await expect_to_fail(async () => {
      await permits.set_expiry(Option.Some(new Nat(0)), Option.Some(blake2b(packed_transfer_params)), { as: alice })
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Burn is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.burn(new Nat(1), { as: alice })
    }, fa2_fungible.errors.CONTRACT_PAUSED);
  });

  it('Unpause by not owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.unpause({ as: bob });
    }, fa2_fungible.errors.INVALID_CALLER);
  });

  it('Unpause by owner should succeed', async () => {
    await fa2_fungible.unpause({ as: alice });
    await permits.unpause({ as: alice });
  });
});

describe('[FA2 fungible] Transfer ownership', async () => {

  it('Transfer ownership when contract is paused should succeed', async () => {
    const owner = await fa2_fungible.get_owner()
    assert(owner.equals(alice.get_address()));
    await fa2_fungible.declare_ownership(alice.get_address(), { as: alice });
    const new_owner = await fa2_fungible.get_owner()
    assert(owner.equals(new_owner));
  });

  it('Transfer ownership as non owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_fungible.declare_ownership(bob.get_address(), { as: bob });
    }, fa2_fungible.errors.INVALID_CALLER);
  });

  it('Transfer ownership as owner should succeed', async () => {
    const owner = await fa2_fungible.get_owner()
    assert(owner.equals(alice.get_address()));
    await fa2_fungible.declare_ownership(bob.get_address(), { as: alice })
    await fa2_fungible.claim_ownership({ as: bob });
    const new_owner = await fa2_fungible.get_owner()
    assert(new_owner.equals(bob.get_address()));
  });
});


describe('[FA2 fungible] Balance of', async () => {

  it('Simple balance of', async () => {
    const balance_alice = await fa2_fungible.get_ledger_value(alice.get_address())

    const res = await fa2_fungible.balance_of([new balance_of_request(alice.get_address(), token_id)], { as: alice })

    assert(res.length == 1)
    assert(balance_alice?.equals(res[0].balance_), "Invalid balance amount")
    assert(alice.get_address().equals(res[0].request.bo_owner), "Invalid address")
  });

  it('Call balance of with unknown address', async () => {
    const res = await fa2_fungible.balance_of([new balance_of_request(user5.get_address(), token_id)], { as: alice })

    assert(res.length == 1)
    assert((new Nat(0)).equals(res[0].balance_), "Invalid balance amount")
    assert(user5.get_address().equals(res[0].request.bo_owner), "Invalid address")
  });

  it('Unknown token_id should fail', async () => {
    const fake_token = new Nat(56)
    await expect_to_fail(async () => {
      await fa2_fungible.balance_of([new balance_of_request(alice.get_address(), fake_token)], { as: alice })
    }, fa2_fungible.errors.FA2_TOKEN_UNDEFINED)
  });
});
