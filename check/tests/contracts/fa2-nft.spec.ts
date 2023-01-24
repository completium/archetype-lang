import { Bytes, Chain_id, Key, Nat, Option, Or, pair_to_mich, Signature, string_to_mich } from '@completium/archetype-ts-types'
import { blake2b, expect_to_fail, get_account, get_chain_id, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import { get_packed_transfer_params, get_transfer_permit_data, get_missigned_error, wrong_packed_transfer_params, wrong_sig } from './fa2.utils'

import assert from 'assert';

/* Contracts */

import { add_operator, balance_of_request, fa2_nft, gasless_param, operator_key, operator_param, part, transfer_destination, transfer_param } from '../../bindings/contracts/fa2/fa2_nft';
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

const amount = new Nat(1);
const token_id = new Nat(0);
const one_token_id = new Nat(1);
const expiry = new Nat(31556952)

const error_key_exists_ledger = pair_to_mich([string_to_mich("\"KEY_EXISTS\""), string_to_mich("\"ledger\"")])
const error_permit_expired = (v: number) => pair_to_mich([string_to_mich("\"PERMIT_EXPIRED\""), new Nat(v).to_mich()])
const get_ref_user_permits = (counter: Nat, data: Bytes, expiry: Nat, now: Date) => {
  return new permits_value(counter, Option.None<Nat>(), [[
    blake2b(data),
    new user_permit(Option.Some<Nat>(expiry), new Date(now.getTime() - now.getMilliseconds()))
  ]])
}

/* Scenarios --------------------------------------------------------------- */

describe('[FA2 NFT] Initialization', async () => {
  it('Set time', async () => {
    set_mockup_now(now)
  });
});

describe('[FA2 NFT] Contracts deployment', async () => {
  it('Permits contract deployment should succeed', async () => {
    await permits.deploy(alice.get_address(), { as: alice })
  });
  it('FA2 NFT contract deployment should succeed', async () => {
    await fa2_nft.deploy(alice.get_address(), permits.get_address(), { as: alice })
  });
});

describe('[FA2 NFT] Contract configuration', async () => {
  it("Add FA2 as permit consumer", async () => {
    await permits.manage_consumer(new add(fa2_nft.get_address()), { as: alice })
  })
})

describe('[FA2 NFT] Minting', async () => {
  it('Mint tokens on FA2 as owner for owner should succeed', async () => {
    await fa2_nft.mint(
      alice.get_address(),      // owner
      token_id,                 // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );
    const owner = await fa2_nft.get_ledger_value(token_id)
    assert(owner?.equals(alice.get_address()))
  });

  it('Mint tokens on FA2 as non owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.mint(
        bob.get_address(),         // owner
        token_id.plus(new Nat(1)), // token id
        [['', new Bytes('')]],     // metadata
        [                          // royalties
          new part(alice.get_address(), new Nat(1000)),
          new part(bob.get_address(), new Nat(500))
        ], {
        as: bob,
      });
    }, fa2_nft.errors.INVALID_CALLER);
  });

  it('Mint tokens on FA2 as owner for someone else should succeed', async () => {
    await fa2_nft.mint(
      carl.get_address(),        // owner
      token_id.plus(new Nat(1)), // token id
      [['', new Bytes('')]],     // metadata
      [                          // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    });
    const owner = await fa2_nft.get_ledger_value(token_id.plus(new Nat(1)))
    assert(owner?.equals(carl.get_address()))
  });

  it('Re-Mint same tokens on FA2 contract should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.mint(
        alice.get_address(),       // owner
        token_id,                  // token id
        [['', new Bytes('')]],     // metadata
        [                          // royalties
          new part(alice.get_address(), new Nat(1000)),
          new part(bob.get_address(), new Nat(500))
        ], {
        as: alice,
      }
      );
    }, error_key_exists_ledger);
  });

});

describe('[FA2 NFT] Add permit', async () => {
  it('Add a permit with the wrong signature should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()
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
    const chain_id = await get_chain_id()

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
      const sig = await alice.sign(permit_data)
      await permits.permit(new Key(alice.pubk), sig, blake2b(wrong_packed_transfer_params), { as: bob })
    }, get_missigned_error(wrong_permit_data));
  });

  it('Add a permit with the wrong public key should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(carl.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);

    await expect_to_fail(async () => {
      const sig = await alice.sign(permit_data)
      await permits.permit(new Key(bob.pubk), sig, blake2b(packed_transfer_params), { as: bob })
    }, get_missigned_error(permit_data));
  });

  it('Add a permit with the good hash, signature and public key should succeed', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(carl.get_address(), one_token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()
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
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(carl.get_address(), one_token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    assert(initial_permit?.equals(get_ref_user_permits(new Nat(1), packed_transfer_params, expiry, now)))

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    await expect_to_fail(async () => {
      await permits.permit(new Key(alice.pubk), sig, blake2b(packed_transfer_params), { as: bob })
    }, pair_to_mich([string_to_mich("DUP_PERMIT"), blake2b(packed_transfer_params).to_mich()]));
  });

});

describe('[FA2 NFT] Transfers', async () => {
  it('Transfer a token not owned should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.transfer([new transfer_param(
        alice.get_address(),
        [new transfer_destination(bob.get_address(), new Nat(777), amount)])],
        { as: alice });
    }, fa2_nft.errors.FA2_TOKEN_UNDEFINED);
  });

  it('Transfer a token from another user without a permit or an operator should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.transfer([new transfer_param(
        alice.get_address(),
        [new transfer_destination(bob.get_address(), token_id, amount)])
      ],
        { as: bob });
    }, fa2_nft.errors.FA2_NOT_OPERATOR);
  });

  it('Transfer more tokens than owned should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.transfer([new transfer_param(
        alice.get_address(),
        [new transfer_destination(bob.get_address(), token_id, new Nat(999))])],
        { as: alice });
    }, fa2_nft.errors.FA2_INSUFFICIENT_BALANCE);
  });

  it('Transfer tokens without operator and an expired permit should fail', async () => {

    set_mockup_now(now);

    const new_expiry = new Nat(3600);

    let alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    await permits.permit(new Key(alice.pubk), sig, blake2b(packed_transfer_params), { as: carl })

    const alice_permit = await permits.get_permits_value(alice.get_address())
    const created_at = alice_permit?.user_permits[0][1].created_at

    await permits.set_expiry(Option.Some<Nat>(new_expiry), Option.Some<Bytes>(blake2b(packed_transfer_params)), { as: alice })


    set_mockup_now(new Date(now.getTime() + 3610 * 1000))

    await expect_to_fail(async () => {
      await fa2_nft.transfer(tps, { as: bob })
    }, error_permit_expired(created_at != undefined ? created_at.getTime() / 1000 + 3600 : 0))
  });

  it('Transfer tokens with an operator and with permit (permit not consumed) should succeed', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(carl.get_address(), token_id, amount)])]
    const chain_id = await get_chain_id()

    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    await permits.permit(alice.get_public_key(), sig, blake2b(packed_transfer_params), { as: bob })

    const added_permits = await permits.get_permits_value(alice.get_address())
    const permits_count = added_permits?.user_permits.length

    await fa2_nft.update_operators([
      new add_operator(new operator_param(alice.get_address(), carl.get_address(), token_id))
    ], { as: alice })

    const token_owner = await fa2_nft.get_ledger_value(token_id)
    assert(token_owner?.equals(alice.get_address()))

    await fa2_nft.transfer(tps, { as: carl })

    const token_owner_after = await fa2_nft.get_ledger_value(token_id)
    assert(token_owner_after?.equals(carl.get_address()))

    // check that permits was NOT consumed
    const added_permits_after = await permits.get_permits_value(alice.get_address())
    const permits_count_after = added_permits_after?.user_permits.length
    assert(permits_count == permits_count_after)
  });

  it('Transfer tokens without an operator and a valid permit (permit consumed)', async () => {
    const another_token = new Nat(12)

    await fa2_nft.mint(
      alice.get_address(),      // owner
      another_token,            // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );

    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), another_token, amount)])]
    const chain_id = await get_chain_id()

    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    await permits.permit(alice.get_public_key(), sig, blake2b(packed_transfer_params), { as: carl })

    const added_permits = await permits.get_permits_value(alice.get_address())
    const permits_count = added_permits?.user_permits.length

    const token_owner = await fa2_nft.get_ledger_value(another_token)
    assert(token_owner?.equals(alice.get_address()), "Invalid owner before")

    await fa2_nft.transfer(tps, { as: carl })

    const token_owner_after = await fa2_nft.get_ledger_value(another_token)
    assert(token_owner_after?.equals(bob.get_address()), "Invalid owner after")

    // check that permits WAS consumed
    const added_permits_after = await permits.get_permits_value(alice.get_address())
    const permits_count_after = added_permits_after?.user_permits.length
    if (permits_count && permits_count_after)
      assert(permits_count == permits_count_after + 1, "Invalid counts: " + permits_count + " vs. " + permits_count_after)
    else
      assert(false, "Invalid counts")
  });
})

describe('[FA2 NFT] Transfers gasless ', async () => {
  it('Transfer a token not owned should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), token_id, amount)
      ])]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    const not_owned_token = new Nat(777)
    const another_tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), not_owned_token, amount)
      ])]

    const another_packed = get_packed_transfer_params(another_tps)
    const another_permit_data = get_transfer_permit_data(
      another_packed,
      permits.get_address(),
      chain_id,
      alice_permit_counter);

    await expect_to_fail(async () => {
      await fa2_nft.transfer_gasless([
        new gasless_param(another_tps, alice.get_public_key(), sig)
      ], { as: alice })
    }, get_missigned_error(another_permit_data))

  });

  it('Transfer a token from another user with wrong a permit should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), token_id, amount)
      ])]
    const chain_id = await get_chain_id()
    const packed_transfer_params = get_packed_transfer_params(tps)

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    const not_owned_token = new Nat(1)
    const another_tps = [
      new transfer_param(alice.get_address(),
        [new transfer_destination(bob.get_address(), not_owned_token, amount)]
      )
    ]

    const another_packed = get_packed_transfer_params(another_tps)
    const another_permit_data = get_transfer_permit_data(
      another_packed,
      permits.get_address(),
      chain_id,
      alice_permit_counter);

    await expect_to_fail(async () => {
      await fa2_nft.transfer_gasless([
        new gasless_param(another_tps, alice.get_public_key(), sig)
      ], { as: alice })
    }, get_missigned_error(another_permit_data))
  });

  it('Transfer more tokens than owned should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), token_id, new Nat(777777))
      ])]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    await expect_to_fail(async () => {
      await fa2_nft.transfer_gasless([
        new gasless_param(tps, alice.get_public_key(), sig)
      ], { as: alice })
    }, fa2_nft.errors.FA2_INSUFFICIENT_BALANCE)
  });

  it('Transfer tokens with from different signer should fail', async () => {
    const new_token = new Nat(11111)

    await fa2_nft.mint(
      carl.get_address(),       // owner
      new_token,                // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );

    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(carl.get_address(),
      [new transfer_destination(bob.get_address(), new_token, amount)]
    )]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    const token_owner = await fa2_nft.get_ledger_value(new_token)
    assert(token_owner?.equals(carl.get_address()), "Invalid owner before")

    await expect_to_fail(async () => {
      await fa2_nft.transfer_gasless([
        new gasless_param(tps, alice.get_public_key(), sig)
      ], { as: alice })
    }, fa2_nft.errors.SIGNER_NOT_FROM)
  });

  it('Transfer tokens with permit should succeed', async () => {
    const new_token = new Nat(11112)

    await fa2_nft.mint(
      alice.get_address(),      // owner
      new_token,                // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );

    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), new_token, amount)]
    )]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    const token_owner = await fa2_nft.get_ledger_value(new_token)
    assert(token_owner?.equals(alice.get_address()), "Invalid owner before")

    await fa2_nft.transfer_gasless([
      new gasless_param(tps, alice.get_public_key(), sig)
    ], { as: alice })

    const token_owner_after = await fa2_nft.get_ledger_value(new_token)
    assert(token_owner_after?.equals(bob.get_address()), "Invalid owner after")
  });
});

describe('[FA2 NFT] Transfers one-step ', async () => {
  it('Transfer a token not owned should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), token_id, amount)
      ])]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await bob.sign(permit_data)

    const not_owned_token = new Nat(777)
    const another_tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), not_owned_token, amount)
      ])]

    const lpermit = Option.Some<[Key, Signature]>([bob.get_public_key(), sig]);

    await expect_to_fail(async () => {
      await fa2_nft.permit_transfer(another_tps, lpermit, { as: bob })
    }, fa2_nft.errors.SIGNER_NOT_FROM)

  });

  it('Transfer a token from another user with wrong a permit should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), token_id, amount)
      ])]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)

    const not_owned_token = new Nat(1)
    const another_tps = [
      new transfer_param(alice.get_address(),
        [new transfer_destination(bob.get_address(), not_owned_token, amount)]
      )
    ]

    const another_packed = get_packed_transfer_params(another_tps)
    const another_permit_data = get_transfer_permit_data(
      another_packed,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const lpermit = Option.Some<[Key, Signature]>([alice.get_public_key(), sig]);

    await expect_to_fail(async () => {
      await fa2_nft.permit_transfer(another_tps, lpermit, { as: alice })
    }, get_missigned_error(another_permit_data))
  });

  it('Transfer more tokens than owned should fail', async () => {
    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), token_id, new Nat(777777))
      ])]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)
    const lpermit = Option.Some<[Key, Signature]>([alice.get_public_key(), sig]);

    await expect_to_fail(async () => {
      await fa2_nft.permit_transfer(tps, lpermit, { as: alice })
    }, fa2_nft.errors.FA2_INSUFFICIENT_BALANCE)
  });

  it('Transfer tokens with from different signer should fail', async () => {
    const new_token = new Nat(11211)

    await fa2_nft.mint(
      carl.get_address(),       // owner
      new_token,                // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );

    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(carl.get_address(),
      [new transfer_destination(bob.get_address(), new_token, amount)]
    )]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)
    const lpermit = Option.Some<[Key, Signature]>([alice.get_public_key(), sig]);

    const token_owner = await fa2_nft.get_ledger_value(new_token)
    assert(token_owner?.equals(carl.get_address()), "Invalid owner before")

    await expect_to_fail(async () => {
      await fa2_nft.permit_transfer(tps, lpermit, { as: alice })
    }, fa2_nft.errors.SIGNER_NOT_FROM)
  });

  it('Transfer tokens with permit should succeed', async () => {
    const new_token = new Nat(11212)

    await fa2_nft.mint(
      alice.get_address(),      // owner
      new_token,                // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );

    const alice_permit_counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(bob.get_address(), new_token, amount)]
    )]

    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      alice_permit_counter);
    const sig = await alice.sign(permit_data)
    const lpermit = Option.Some<[Key, Signature]>([alice.get_public_key(), sig]);

    const token_owner = await fa2_nft.get_ledger_value(new_token)
    assert(token_owner?.equals(alice.get_address()), "Invalid owner before")

    await fa2_nft.permit_transfer(tps, lpermit, { as: alice })

    const token_owner_after = await fa2_nft.get_ledger_value(new_token)
    assert(token_owner_after?.equals(bob.get_address()), "Invalid owner after")
  });
});

describe('[FA2 NFT] Set metadata', async () => {
  it('Set metadata with empty content should succeed', async () => {
    const metadata_before = await fa2_nft.get_metadata_value("key")
    assert(metadata_before == undefined);

    await fa2_nft.set_metadata("key", Option.Some(new Bytes("")), { as: alice })

    const metadata_after = await fa2_nft.get_metadata_value("key")
    assert(metadata_after?.equals(new Bytes("")));
  });

  it('Set metadata called by not owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.set_metadata("key", Option.Some(new Bytes("")), { as: bob })
    }, fa2_nft.errors.INVALID_CALLER);
  });

  it('Set metadata with valid content should succeed', async () => {
    const data = new Bytes('697066733a2f2f516d617635756142437a4d77377871446f55364d444534743473695855484e4737664a68474c746f79774b35694a');
    const metadata_before = await fa2_nft.get_metadata_value("key")
    assert(metadata_before?.equals(new Bytes("")), "Invalid metadata before");

    await fa2_nft.set_metadata("key", Option.Some(data), { as: alice })

    const metadata_after = await fa2_nft.get_metadata_value("key")
    assert(metadata_after?.equals(data));
  });
});

describe('[FA2 NFT] Set expiry', async () => {

  it('Set global expiry with too big value should fail', async () => {
    await expect_to_fail(async () => {
      await permits.set_expiry(Option.Some(new Nat('999999999999999999999999999999999999999')), Option.None(), { as: alice })
    }, permits.errors.r2);
  });

  it('Set expiry for an existing permit with too big value should fail', async () => {
    const counter = (await permits.get_permits_value(alice.get_address()))?.counter

    const tps = [new transfer_param(alice.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await alice.sign(permit_data)

    await permits.permit(alice.get_public_key(), sig, blake2b(packed_transfer_params), { as: bob })

    await expect_to_fail(async () => {
      await permits.set_expiry(
        Option.Some<Nat>(new Nat('999999999999999999999999999999999999999')),
        Option.Some<Bytes>(permit_data),
        { as: alice }
      )
    }, permits.errors.r2);
  });

  it('Set expiry with 0 (permit get deleted) should succeed', async () => {
    const counter = (await permits.get_permits_value(user2.get_address()))?.counter

    const tps = [new transfer_param(user2.get_address(),
      [new transfer_destination(user1.get_address(), token_id, amount)
      ])]
    const chain_id = await get_chain_id()

    const packed_transfer_params = get_packed_transfer_params(tps)
    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await user2.sign(permit_data)

    await permits.permit(user2.get_public_key(), sig, blake2b(packed_transfer_params), { as: bob })

    const added_permits = await permits.get_permits_value(user2.get_address())
    const permits_count = added_permits?.user_permits.length
    assert(permits_count ? permits_count == 1 : false)

    await permits.set_expiry(
      Option.Some<Nat>(new Nat(0)),
      Option.Some<Bytes>(blake2b(packed_transfer_params)),
      { as: user2 }
    )

    const added_permits_after = await permits.get_permits_value(user2.get_address())
    const permits_count_after = added_permits_after?.user_permits.length
    assert(permits_count_after != undefined ? permits_count_after == 0 : false, "Invalid count after: " + permits_count_after)
  });

  it('Set expiry with a correct value should succeed', async () => {
    const counter = (await permits.get_permits_value(carl.get_address()))?.counter

    const tps = [new transfer_param(carl.get_address(),
      [new transfer_destination(bob.get_address(), token_id, new Nat(11))
      ])]
    const packed_transfer_params = get_packed_transfer_params(tps)
    const chain_id = await get_chain_id()

    const permit_data = get_transfer_permit_data(
      packed_transfer_params,
      permits.get_address(),
      chain_id,
      counter);
    const sig = await carl.sign(permit_data)

    await permits.permit(carl.get_public_key(), sig, blake2b(packed_transfer_params), { as: alice })

    await permits.set_expiry(
      Option.Some(new Nat(8)),
      Option.Some(blake2b(packed_transfer_params)),
      { as: carl }
    )

    const permit_after = await permits.get_permits_value(carl.get_address())
    assert(permit_after?.user_permits[0][1].expiry.get().equals(new Nat(8)))

  });

});

describe('[FA2 NFT] Burn', async () => {
  it('Burn without tokens should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.burn(token_id, { as: alice })
    }, fa2_nft.errors.FA2_NOT_OWNER);
  });

  it('Burn tokens with enough tokens should succeed', async () => {
    const new_token = new Nat(999)

    await fa2_nft.mint(
      alice.get_address(),      // owner
      new_token,                // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );

    const owner = await fa2_nft.get_ledger_value(new_token)
    assert(owner?.equals(alice.get_address()))

    await fa2_nft.burn(new_token, { as: alice })

    const new_owner = await fa2_nft.get_ledger_value(new_token)
    assert(new_owner == undefined)
  });

  it('Re-mint a burnt token', async () => {
    const new_token = new Nat(999)

    await fa2_nft.mint(
      alice.get_address(),      // owner
      new_token,                // token id
      [['', new Bytes('')]],    // metadata
      [                         // royalties
        new part(alice.get_address(), new Nat(1000)),
        new part(bob.get_address(), new Nat(500))
      ], {
      as: alice,
    }
    );
    const owner = await fa2_nft.get_ledger_value(new_token)
    assert(owner?.equals(alice.get_address()))

    await fa2_nft.burn(new_token, { as: alice })

    const new_owner = await fa2_nft.get_ledger_value(new_token)
    assert(new_owner == undefined)
  });
});

describe('[FA2 NFT] Pause', async () => {
  it('Set FA2 on pause should succeed', async () => {
    await fa2_nft.pause({ as: alice });
    const is_paused = await fa2_nft.get_paused()
    assert(is_paused);
  });
  it('Set Permits on pause should succeed', async () => {
    await permits.pause({ as: alice });
    const is_paused = await fa2_nft.get_paused()
    assert(is_paused);
  });

  it('Minting is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      const new_token = new Nat(999)

      await fa2_nft.mint(
        alice.get_address(),      // owner
        new_token,                // token id
        [['', new Bytes('')]],    // metadata
        [                         // royalties
          new part(alice.get_address(), new Nat(1000)),
          new part(bob.get_address(), new Nat(500))
        ], {
        as: alice,
      }
      );
    }, fa2_nft.errors.CONTRACT_PAUSED);
  });

  it('Update operators is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.update_operators([
        new add_operator(new operator_param(alice.get_address(), fa2_nft.get_address(), token_id))
      ], { as: alice })
    }, fa2_nft.errors.CONTRACT_PAUSED);
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
      await permits.permit(alice.get_public_key(), sig, blake2b(packed_transfer_params), { as: alice });
    }, fa2_nft.errors.CONTRACT_PAUSED);
  });

  it('Transfer is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.transfer([new transfer_param(
        user1.get_address(),
        [new transfer_destination(user2.get_address(), token_id, new Nat(1))])],
        { as: user1 });
    }, fa2_nft.errors.CONTRACT_PAUSED);
  });

  it('Set metadata is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.set_metadata("key", Option.Some(new Bytes("")), { as: alice })
    }, fa2_nft.errors.CONTRACT_PAUSED);
  });

  it('Set expiry is not possible when contract is paused should fail', async () => {
    const tps = [new transfer_param(alice.get_address(), [new transfer_destination(bob.get_address(), token_id, amount)])]
    const packed_transfer_params = get_packed_transfer_params(tps)

    await expect_to_fail(async () => {
      await permits.set_expiry(Option.Some(new Nat(0)), Option.Some(packed_transfer_params), { as: alice })
    }, fa2_nft.errors.CONTRACT_PAUSED);
  });

  it('Burn is not possible when contract is paused should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.burn(new Nat(1), { as: alice })
    }, fa2_nft.errors.CONTRACT_PAUSED);
  });

  it('Unpause by not owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.unpause({ as: bob });
    }, fa2_nft.errors.INVALID_CALLER);
  });

  it('Unpause by owner should succeed', async () => {
    await fa2_nft.unpause({ as: alice });
    await permits.unpause({ as: alice });
  });
});

describe('[FA2 NFT] Transfer ownership', async () => {

  it('Transfer ownership when contract is paused should succeed', async () => {
    const owner = await fa2_nft.get_owner()
    assert(owner.equals(alice.get_address()));
    await fa2_nft.declare_ownership(alice.get_address(), { as: alice });
    const new_owner = await fa2_nft.get_owner()
    assert(owner.equals(new_owner));
  });

  it('Transfer ownership as non owner should fail', async () => {
    await expect_to_fail(async () => {
      await fa2_nft.declare_ownership(bob.get_address(), { as: bob });
    }, fa2_nft.errors.INVALID_CALLER);
  });

  it('Transfer ownership as owner should succeed', async () => {
    const owner = await fa2_nft.get_owner()
    assert(owner.equals(alice.get_address()));
    await fa2_nft.declare_ownership(bob.get_address(), { as: alice })
    await fa2_nft.claim_ownership({ as: bob });
    const new_owner = await fa2_nft.get_owner()
    assert(new_owner.equals(bob.get_address()));
  });
});

describe('[FA2 NFT] Balance of', async () => {

  it('Unknown token_id should fail', async () => {
    const fake_token = new Nat(56)
    await expect_to_fail(async () => {
      await fa2_nft.balance_of([new balance_of_request(alice.get_address(), fake_token)], { as: alice })
    }, fa2_nft.errors.FA2_TOKEN_UNDEFINED)
  });

});
