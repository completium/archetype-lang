import { Address, Bytes, Duration, Micheline, MichelineType, Nat, pair_to_mich, Signature, string_to_mich } from '@completium/archetype-ts-types'
import { Account, delay_mockup_now_by_hour, expect_to_fail, expr_micheline_to_json, get_account, pack, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import assert from 'assert';

/* Contracts */

import { rec_to_sign_approve_feeless, rec_to_sign_approve_feeless_mich_type, rec_to_sign_propose_feeless, rec_to_sign_propose_feeless_mich_type, multisig } from '../../bindings/contracts/multisig/multisig';
import { dummy } from '../../bindings/contracts/multisig/dummy';

/* Accounts ----------------------------------------------------------------- */

const owner = get_account('alice');
const manager1 = get_account('bob');
const manager2 = get_account('carl');
const manager3 = get_account('bootstrap1');


/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

// constants
const MAX_DURATION = new Duration("180d") // 180 days
const MIN_DURATION = new Duration("1h")   // 1 hour
const now = new Date("2022-06-01")

let proposal_id = new Nat(0)

const getCode = (dest: Address, entrypoint: string, typ: string, value: string): Micheline => {
  const input = `{
      DROP;
      NIL operation;
      PUSH address "${dest.toString()}";
      CONTRACT %${entrypoint} ${typ};
      IF_NONE
        { PUSH string "EntryNotFound";
          FAILWITH }
        {  };
      PUSH mutez 0;
      PUSH ${typ} ${value};
      TRANSFER_TOKENS;
      CONS;
    }`;
  return expr_micheline_to_json(input)
}

describe("[Multisig] Deploy", async () => {
  it("Dummy", async () => {
    await dummy.deploy(owner.get_address(), { as: owner })
  });

  it("Multisig", async () => {
    await multisig.deploy(owner.get_address(), new Nat(1), MAX_DURATION, MIN_DURATION, { as: owner })
  });
})

describe("[Multisig] Init", async () => {

  it('Set time', async () => {
    set_mockup_now(now)
  });

  it("Add 3 managers", async () => {
    await multisig.control(manager1.get_address(), true, {
      as: owner
    })
    await multisig.control(manager2.get_address(), true, {
      as: owner
    })
    await multisig.control(manager3.get_address(), true, {
      as: owner
    })
  });

  it("Run", async () => {
    await multisig.run({
      as: owner
    })
  })

})

describe("[Multisig] Change requested value", async () => {

  it("Propose 'request' action by manager1", async () => {
    const code = getCode(multisig.get_address(), "require", "nat", "2");

    const approved_by_caller = true;

    const validity_duration = new Duration("48h")

    await multisig.propose(code, validity_duration, approved_by_caller, {
      as: manager1
    })
  });

  it("Approve by Manager2 and Manager3", async () => {
    await multisig.approve(proposal_id, {
      as: manager2
    });

    await multisig.approve(proposal_id, {
      as: manager3
    })
  });

  it("Execute (by previous contract's owner)", async () => {
    const required_before = await multisig.get_required()
    assert(required_before.to_number() == 1)

    await multisig.execute(proposal_id, {
      as: owner
    });

    proposal_id = new Nat(proposal_id.to_number() + 1)

    const required_after = await multisig.get_required()
    assert(required_after.to_number() == 2)

  });

})

describe("[Multisig] Basic check on Dummy contract", async () => {

  it("Invalid caller", async () => {
    await expect_to_fail(async () => {
      await dummy.process(new Nat(1), {
        as: manager1
      })
    }, dummy.errors.INVALID_CALLER)
  });

  it("Owner can call Dummy's process", async () => {
    const result_before = await dummy.get_result()
    assert(result_before.to_number() == 0)

    await dummy.process(new Nat(1), {
      as: owner
    })
    const result_after = await dummy.get_result()
    assert(result_after.to_number() == 1)
  });

})

describe("[Multisig] Test Multisig", async () => {

  it("Set Multisig as Dummy's owner", async () => {
    await dummy.set_owner(multisig.get_address(), {
      as: owner
    })
  });

  it("Previous owner cannot call Dummy's process", async () => {
    await expect_to_fail(async () => {
      await dummy.process(new Nat(2), {
        as: owner
      })
    }, dummy.errors.INVALID_CALLER)
  });

  it("Previous owner cannot add proposal", async () => {
    const code = getCode(dummy.get_address(), "process", "nat", "1");

    await expect_to_fail(async () => {
      await multisig.propose(code, new Duration("48h"), true, {
        as: owner
      })
    }, multisig.errors.INVALID_CALLER)
  });

  it("Add proposal and approve by Manager1", async () => {
    const code = getCode(dummy.get_address(), "process", "nat", "2");

    await multisig.propose(code, new Duration("48h"), true, {
      as: manager1
    })
  });

  it("Manager 1 cannot execute", async () => {
    await expect_to_fail(async () => {
      await multisig.execute(proposal_id, {
        as: manager1
      })
    }, multisig.errors.r2);
  })

  it("Approve by Manager2 and Manager3", async () => {
    await multisig.approve(proposal_id, {
      as: manager2
    });

    await multisig.approve(proposal_id, {
      as: manager3
    })
  });

  it("Set 'now' beyond expiration date", async () => {
    set_mockup_now(now)
    delay_mockup_now_by_hour(49);
  });

  it("Proposal is expired", async () => {

    await expect_to_fail(async () => {
      await multisig.execute(proposal_id, {
        as: manager1
      })
    }, multisig.errors.r1);

  });

  it("Set 'now' before expiration date", async () => {
    set_mockup_now(now)
    delay_mockup_now_by_hour(47)
  });

  it("Execute (by previous Dummy's owner)", async () => {
    const result_before = await dummy.get_result();
    assert(result_before.to_number() == 1)

    await multisig.execute(proposal_id, {
      as: owner
    });

    proposal_id = new Nat(proposal_id.to_number() + 1)

    const result_after = await dummy.get_result();
    assert(result_after.to_number() == 2)
  });

})

describe("[Multisig] Test Multisig 2", async () => {

  it("Add proposal by Manager1", async () => {
    const code = getCode(dummy.get_address(), "process", "nat", "3");

    await multisig.propose(code, new Duration("48h"), false, {
      as: manager1
    })
  });

  it("Approve by Managers 1, 2 and 3", async () => {
    await multisig.approve(proposal_id, {
      as: manager1
    });

    await multisig.approve(proposal_id, {
      as: manager2
    });

    await multisig.approve(proposal_id, {
      as: manager3
    })
  });

  it("Set 'now' before expiration date", async () => {
    set_mockup_now(now)
    delay_mockup_now_by_hour(47)
  });

  it("Execute (by previous Dummy's owner)", async () => {
    const result_before = await dummy.get_result()
    assert(result_before.to_number() == 2)

    await multisig.execute(proposal_id, {
      as: owner
    });
    proposal_id = new Nat(proposal_id.to_number() + 1)
    const result_after = await dummy.get_result()
    assert(result_after.to_number() == 3)
  });

})

describe("[Multisig] Feeless process (propose, approve)", async () => {

  it("Manager1 proposes and approves (injected by owner)", async () => {
    const validity_duration = new Duration("48h")
    const pkh = manager1.get_address()
    const counter = new Nat(0)
    const entryname = "propose"

    const code = getCode(dummy.get_address(), "process", "nat", "4");

    const tosign = pack(new rec_to_sign_propose_feeless(pkh, counter, entryname, code, validity_duration).to_mich(), rec_to_sign_propose_feeless_mich_type);
    const signature: Signature = await manager1.sign(tosign);

    await multisig.propose_feeless(code, validity_duration, true, manager1.get_public_key(), signature, {
      as: owner
    })

  });

  it("Manager2 approves with INVALID signature", async () => {
    const counter = 0
    const entryname = "approve"
    const pkh = manager2.pkh

    // Build invalid signature
    const dataType = expr_micheline_to_json("(pair address (pair nat (pair string nat)))") as MichelineType;
    const data = expr_micheline_to_json(`(Pair "${pkh}" (Pair ${counter} (Pair "${entryname}" ${proposal_id})))`);

    const tosign = pack(data, dataType);
    const signature: Signature = await manager1.sign(tosign); // signed by manager1 instead of manager2

    await expect_to_fail(async () => {
      await multisig.approve_feeless(proposal_id, manager2.get_public_key(), signature, {
        as: owner
      })
    }, multisig.errors.r10)
  })

  it("Manager2 approves (injected by owner)", async () => {
    await approve_feeless(manager2)
  });

  it("Manager3 approves (injected by owner)", async () => {
    await approve_feeless(manager3)
  });

  const approve_feeless = async (manager: Account) => {
    const counter = new Nat(0)
    const entryname = "approve"

    const tosign = pack(new rec_to_sign_approve_feeless(manager.get_address(), counter, entryname, proposal_id).to_mich(), rec_to_sign_approve_feeless_mich_type);
    const signature = await manager.sign(tosign);

    await multisig.approve_feeless(proposal_id, manager.get_public_key(), signature, {
      as: owner
    })
  }

  it("Execute (by owner)", async () => {
    const storage_before = await dummy.get_result()
    assert(storage_before.to_number() == 3)

    await multisig.execute(proposal_id, {
      as: owner
    });

    proposal_id = new Nat(proposal_id.to_number() + 1)

    const storage_after = await dummy.get_result()
    assert(storage_after.to_number() == 4)
  });

});

describe("[Multisig] Pause / Unpause", async () => {
  it("Manager1 proposes and approves to pause", async () => {
    const code = getCode(multisig.get_address(), "pause", "unit", "Unit");

    await multisig.propose(code, new Duration("48h"), true, {
      as: manager1
    })
  });
  it("Manager2 and Manager3 approve", async () => {
    await multisig.approve(proposal_id, {
      as: manager2
    });
    await multisig.approve(proposal_id, {
      as: manager3
    })
  });
  it("Owner executes", async () => {
    await multisig.execute(proposal_id, {
      as: owner
    });
  });
  it("Manager 1 cannot propose", async () => {
    const code = getCode(multisig.get_address(), "require", "nat", "10");
    await expect_to_fail(async () => {
      await multisig.propose(code, new Duration("48h"), true, {
        as: manager1
      })
    }, multisig.errors.INVALID_STATE)
  });
  it("Managers (2 and 3) approve unpause", async () => {
    await multisig.approve_unpause({
      as: manager2
    });
    await multisig.approve_unpause({
      as: manager3
    })
  });
  it("Owner unpauses", async () => {
    await multisig.unpause({
      as: owner
    });
  });
  it("Manager 1 can now propose", async () => {
    const code = getCode(multisig.get_address(), "set_duration", "(pair nat nat)", `(Pair 60 ${MAX_DURATION.toSecond()})`);

    await multisig.propose(code, new Duration("48h"), true, {
      as: manager1
    })
  });
})