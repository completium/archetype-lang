import { Bytes, Nat, Option, pair_to_mich, string_to_mich } from "@completium/archetype-ts-types";
import { expect_to_fail, get_account, set_mockup, set_mockup_now, set_quiet } from "@completium/experiment-ts";
import assert from 'assert'

import { poll, poll_container } from '../../bindings/contracts/poll/poll'

/* Accounts ---------------------------------------------------------------- */

const alice = get_account('alice');
const bob   = get_account('bob');
const carl  = get_account('carl')

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Now --------------------------------------------------------------------- */

set_mockup_now(new Date(Date.now()))

/* Constants --------------------------------------------------------------- */

const food_hash    = "QmZ8GxAwPvVDEtGxyUmfbB1dtmrdDR6tmMv9HUATaippqU"
const dancer_hash  = "QmbceSQoFzPYAUNnVfmc4juYDm4C4ZN3HrdJu3VfuxNGVR"
const squares_hash = "QmdmFzdsfiAoTF3DaFBuNS6BGYye8q5nZCugrbsf9G3NgJ"

const exists_poll = (c : poll_container, b : Bytes) : boolean => {
  return c.filter(x => { return x[1].ipfs_hash.equals(b) }).length === 1
}

const get_nb_responses = (polls : poll_container, poll_id : Nat, choice_id : Nat) : Nat => {
  return polls.reduce((acc, p) => {
    if (p[0].equals(poll_id)) {
      return p[1].responses.reduce((nb, c) => {
        if (c[0].equals(choice_id)) {
          return c[1]
        } else return nb
      }, new Nat(0))
    } else return acc
  }, new Nat(0))
}

const error_key_exists = (container : string) => {
  return pair_to_mich([string_to_mich("\"KEY_EXISTS\""), string_to_mich(`"${container}"`)])
}

/* Scenario ---------------------------------------------------------------- */

describe('[POLL] Contract deployment', async () => {
  it('Deploy poll contract', async () => {
    await poll.deploy(alice.get_address(), { as: alice })
  });
})

describe("[POLL] 'add_poll' entry", async () => {
  it("add 'Food' poll", async () => {
    const b = Bytes.hex_encode(food_hash)
    await poll.add_poll(Bytes.hex_encode(food_hash), { as: bob });
    const has_poll = await poll.has_poll_to_approve_value(b)
    assert(has_poll)
  })
  it("'add' cannot be called with same hash", async () => {
    const b = Bytes.hex_encode(food_hash)
    await expect_to_fail(async () => {
      await poll.add_poll(b, { as: alice });
    }, error_key_exists("poll_to_approve"))
  })
  it("add 'Dancer' poll", async () => {
    const b = Bytes.hex_encode(dancer_hash)
    await poll.add_poll(b, { as: bob });
    const has_poll = await poll.has_poll_to_approve_value(b)
    assert(has_poll)
  })
  it("add 'Squares' poll", async () => {
    const b = Bytes.hex_encode(squares_hash)
    await poll.add_poll(b, { as: bob });
    const has_poll = await poll.has_poll_to_approve_value(b)
    assert(has_poll)
  })
})
describe("[POLL] 'approve' entry", async () => {
  it("'approve' can only be called by owner", async () => {
    await expect_to_fail(async () => {
      await poll.approve(Bytes.hex_encode(food_hash), { as: bob });
    }, poll.errors.INVALID_CALLER)
  })
  it("approve 'Food' poll", async () => {
    const b = Bytes.hex_encode(food_hash)
    const polls_before = await poll.get_poll()
    assert(!exists_poll(polls_before, b))
    await poll.approve(b, { as: alice });
    const polls_after = await poll.get_poll()
    assert(exists_poll(polls_after, b))
    const has_poll = await poll.has_poll_to_approve_value(b)
    assert(!has_poll)
  })
  it("'approve' cannot be called twice with same hash", async () => {
    const b = Bytes.hex_encode(food_hash)
    await expect_to_fail(async () => {
      await poll.approve(b, { as: alice });
    }, poll.errors.POLL_NOT_FOUND)
  })
  it("approve 'Dancer' poll", async () => {
    const b = Bytes.hex_encode(dancer_hash)
    const polls_before = await poll.get_poll()
    assert(!exists_poll(polls_before, b))
    await poll.approve(b, { as: alice });
    const polls_after = await poll.get_poll()
    assert(exists_poll(polls_after, b))
    const has_poll = await poll.has_poll_to_approve_value(b)
    assert(!has_poll)
  })
})
describe("[POLL] 'disapprove' entry", async () => {
  it("'disapprove' can only be called by owner", async () => {
    await expect_to_fail(async () => {
      await poll.disapprove(Bytes.hex_encode(squares_hash), { as: bob });
    }, poll.errors.INVALID_CALLER)
  })
  it("disapprove 'Squares' poll", async () => {
    const b = Bytes.hex_encode(squares_hash)
    const polls_before = await poll.get_poll()
    assert(!exists_poll(polls_before, b))
    await poll.disapprove(b, { as: alice });
    const polls_after = await poll.get_poll()
    assert(!exists_poll(polls_after, b))
    const has_poll = await poll.has_poll_to_approve_value(b)
    assert(!has_poll)
  })
})
describe("[POLL] 'respond' entry", async () => {
  it("respond to food poll", async () => {
    const poll_id = new Nat(0)
    const choice_id = new Nat(0)
    const polls = await poll.get_poll()
    const nb_responses_before = get_nb_responses(polls, poll_id, choice_id)
    assert(nb_responses_before.equals(new Nat(0)))
    const has_responder_before = await poll.has_responder_value(bob.get_address())
    assert(!has_responder_before)
    await poll.respond(poll_id, choice_id, { as : bob })
    const polls_after = await poll.get_poll()
    const nb_responses_after = get_nb_responses(polls_after, poll_id, choice_id)
    assert(nb_responses_after.equals(new Nat(1)))
    const has_responder_after = await poll.has_responder_value(bob.get_address())
    assert(has_responder_after)
  })
  it("responder cannot respond twice", async () => {
    const poll_id = new Nat(0)
    const choice_id = new Nat(1)
    await expect_to_fail(async () => {
      await poll.respond(poll_id, choice_id, { as : bob })
    }, poll.errors.f1)
  })
  it("'respond' increment number of responses", async () => {
    const poll_id = new Nat(0)
    const choice_id = new Nat(0)
    const polls = await poll.get_poll()
    const nb_responses_before = get_nb_responses(polls, poll_id, choice_id)
    await poll.respond(poll_id, choice_id, { as : carl })
    const polls_after = await poll.get_poll()
    const nb_responses_after = get_nb_responses(polls_after, poll_id, choice_id)
    assert(nb_responses_after.equals(nb_responses_before.plus(new Nat(1))))
  })
})
describe("[POLL] 'remove' entry", async () => {
  it("'remove' can only be called by owner", async () => {
    await expect_to_fail(async () => {
      await poll.remove(new Nat(0), { as : bob })
    }, poll.errors.INVALID_CALLER)
  })
  it("remove food poll", async () => {
    const polls_before = await poll.get_poll()
    assert(exists_poll(polls_before, Bytes.hex_encode(food_hash)))
    await poll.remove(new Nat(0), { as : alice })
    const polls = await poll.get_poll()
    assert(!exists_poll(polls, Bytes.hex_encode(food_hash)))
  })
})
describe('[PAUSABLE] Poll', async () => {
  it('Set poll on pause should succeed', async () => {
    await poll.pause({ as: alice });
    const is_paused = await poll.get_paused()
    assert(is_paused);
  });
  it("'add_poll' should fail", async () => {
    await expect_to_fail(async () => {
      const b = Bytes.hex_encode(squares_hash)
      await poll.add_poll(b, { as: bob })
    }, poll.errors.CONTRACT_PAUSED);
  });
  it("'respond' should fail", async () => {
    const poll_id = new Nat(0)
    const choice_id = new Nat(0)
    await expect_to_fail(async () => {
      await poll.respond(poll_id, choice_id, { as : alice })
    }, poll.errors.CONTRACT_PAUSED);
  });
  it("'set_metadata' should fail", async () => {
    await expect_to_fail(async () => {
      await poll.set_metadata("key", Option.Some(new Bytes("")), { as : alice })
    }, poll.errors.CONTRACT_PAUSED);
  });
  it('Unpause by not owner should fail', async () => {
    await expect_to_fail(async () => {
      await poll.unpause({ as: bob });
    }, poll.errors.INVALID_CALLER);
  });
  it('Unpause by owner should succeed', async () => {
    await poll.unpause({ as: alice });
  });
});
describe('[OWNERSHIP] Poll', async () => {
  it('Transfer ownership when contract is paused should succeed', async () => {
    const owner = await poll.get_owner()
    assert(owner.equals(alice.get_address()));
    await poll.declare_ownership(alice.get_address(), { as: alice });
    const new_owner = await poll.get_owner()
    assert(owner.equals(new_owner));
  });
  it('Transfer ownership as non owner should fail', async () => {
    await expect_to_fail(async () => {
      await poll.declare_ownership(bob.get_address(), { as: bob });
    }, poll.errors.INVALID_CALLER);
  });
  it('Transfer ownership as owner should succeed', async () => {
    const owner = await poll.get_owner()
    assert(owner.equals(alice.get_address()));
    await poll.declare_ownership(bob.get_address(), { as: alice })
    await poll.claim_ownership({ as: bob });
    const new_owner = await poll.get_owner()
    assert(new_owner.equals(bob.get_address()));
  });
});