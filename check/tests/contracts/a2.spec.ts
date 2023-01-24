import { Bytes, Key, Nat, Option, Or, pair_to_mich, Signature, string_to_mich } from '@completium/archetype-ts-types'
import { blake2b, call, expect_to_fail, get_account, set_mockup, set_mockup_now, set_quiet } from '@completium/experiment-ts'

import assert from 'assert';

/* Contracts */

import { a2 } from '../../bindings/contracts/a2/a2';
import { a2_storage } from '../../bindings/contracts/a2/a2_storage';


/* Accounts ----------------------------------------------------------------- */

const superUser = get_account('alice');
const whitelister = get_account('bob');
const carl = get_account('carl');
const daniel = get_account('account_0');
const eddy = get_account('account_1');
const list0User1 = get_account('bootstrap1');
const list0User2 = get_account('bootstrap2');
const list1User1 = get_account('bootstrap3');
const list1User2 = get_account('bootstrap4');
const list2User1 = get_account('bootstrap5');
const kevin = get_account('account_2');
const jacky = get_account('account_3');

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Scenarios --------------------------------------------------------------- */

describe('[A2] Contracts deployment', async () => {
  it('`a2_storage` deployment should succeed', async () => {
    await a2_storage.deploy(whitelister.get_address(), { as: whitelister })
  });
  it('`a2` deployment should succeed', async () => {
    await a2.deploy(whitelister.get_address(), a2_storage.get_address(), { as: whitelister })
  });
  it('`a2_storage` initialization should succeed', async () => {
    await a2_storage.add_whitelister(a2.get_address(), { as: whitelister })
  });
});

describe("[A2] Set admin", async () => {
  it("Set admin as non admin should fail", async () => {
    await expect_to_fail(async () => {
      await a2.declare_ownership(whitelister.get_address(), { as: carl })
    }, a2.errors.INVALID_CALLER)
  });

  it("Set admin should succeed", async () => {
    await a2.declare_ownership(whitelister.get_address(), { as: whitelister })
    const owner = await a2.get_owner();
    assert(owner.equals(whitelister.get_address()))
  });
});

describe("[A2] Add super user", async () => {
  it("Add super user in whitelist contract as non admin should fail", async () => {
    await expect_to_fail(async () => {
      await a2.addSuperuser(superUser.get_address(), { as: carl });
    }, a2.errors.INVALID_CALLER)
  });

  it("Add super user in whitelist contract as admin should succeed", async () => {
    await a2.addSuperuser(superUser.get_address(), { as: whitelister });
    const superusers = await a2.get_superusers();
    assert(superusers.length == 1)
    assert(superusers[0].equals(superUser.get_address()))
  });

  it("Add an already existing super user in whitelist contract as admin should succeed", async () => {
    await a2.addSuperuser(superUser.get_address(), { as: whitelister });
    const superusers = await a2.get_superusers();
    assert(superusers.length == 1)
    assert(superusers[0].equals(superUser.get_address()))
  });
});

describe("[A2] Update user", async () => {
  it("Update a non existing user in whitelist contract as non admin should fail", async () => {
    await expect_to_fail(async () => {
      await a2.updateUser(list0User1.get_address(), Option.Some(new Nat(0)), { as: carl });
    }, a2.errors.INVALID_CALLER)
  });

  it("Update a non existing user in whitelist contract as admin should succeed", async () => {
    await a2.updateUser(list0User1.get_address(), Option.Some(new Nat(0)), { as: whitelister });
    const value = await a2_storage.get_users_value(list0User1.get_address());
    assert(value?.equals(new Nat(0)))
  });

  it("Update a non existing user in whitelist contract with no whitelist id as admin should succeed", async () => {
    await a2.updateUser(list0User2.get_address(), Option.None(), { as: whitelister });
    const value = await a2_storage.get_users_value(list0User2.get_address());
    assert(value === undefined)
  });

  it("Update an existing user in whitelist contract with whitelist id as admin should succeed", async () => {
    await a2.updateUser(list0User2.get_address(), Option.Some(new Nat(0)), { as: whitelister });
    const value = await a2_storage.get_users_value(list0User2.get_address());
    assert(value?.equals(new Nat(0)))
  });

  it("Update an existing user in whitelist contract with no whitelist id (to delete it) as admin should succeed", async () => {
    await a2.updateUser(kevin.get_address(), Option.Some(new Nat(0)), { as: whitelister });
    const value_0 = await a2_storage.get_users_value(kevin.get_address());
    assert(value_0?.equals(new Nat(0)))

    await a2.updateUser(kevin.get_address(), Option.None(), { as: whitelister });
    const value_1 = await a2_storage.get_users_value(kevin.get_address());
    assert(value_1 == undefined)
  });
});

describe("[A2] Update users", async () => {

  it("Update non existing users in whitelist contract as non admin should fail", async () => {
    await expect_to_fail(async () => {
      await a2.updateUsers([[list1User1.get_address(), Option.Some(new Nat(1))], [list1User2.get_address(), Option.None()]], { as: carl });
    }, a2.errors.INVALID_CALLER)
  });

  it("Update non existing users in whitelist contract as admin should succeed", async () => {
    await a2.updateUsers([[list1User1.get_address(), Option.Some(new Nat(0))], [list1User2.get_address(), Option.None()]], { as: whitelister });

    const user1 = await a2_storage.get_users_value(list1User1.get_address());
    assert(user1?.equals(new Nat(0)))
    const user2 = await a2_storage.get_users_value(list1User2.get_address());
    assert(user2 === undefined)
  });

  it("Update existing users in whitelist contract as admin should succeed", async () => {
    await a2.updateUsers([[list1User1.get_address(), Option.Some(new Nat(1))], [list1User2.get_address(), Option.Some(new Nat(1))]], { as: whitelister });

    const user1 = await a2_storage.get_users_value(list1User1.get_address());
    assert(user1?.equals(new Nat(1)))
    const user2 = await a2_storage.get_users_value(list1User2.get_address());
    assert(user2?.equals(new Nat(1)))
  });
});

describe("[A2] Update transfer list", async () => {
  it("Update non existing transfer list as non admin should fail", async () => {
    await expect_to_fail(async () => {
      await a2.updateTransferlist(new Nat(0), Option.Some<[boolean, Array<Nat>]>([true, [new Nat(0)]]), { as: carl });
    }, a2.errors.INVALID_CALLER)
  });

  it("Update non existing transfer list as admin should succeed", async () => {
    await a2.updateTransferlist(new Nat(0), Option.Some<[boolean, Array<Nat>]>([false, [new Nat(0), new Nat(2), new Nat(3)]]), { as: whitelister });
    const list = await a2.get_transferlists_value(new Nat(0));
    assert(list?.unrestricted == false);
    assert(list?.allowedTransferlists.length == 3);
    assert(list?.allowedTransferlists[0].equals(new Nat(0)));
    assert(list?.allowedTransferlists[1].equals(new Nat(2)));
    assert(list?.allowedTransferlists[2].equals(new Nat(3)));
  });

  it("Update non existing transfer list as admin with no allowed lists should succeed", async () => {
    await a2.updateTransferlist(new Nat(1), Option.Some<[boolean, Array<Nat>]>([true, []]), { as: whitelister });
    const list = await a2.get_transferlists_value(new Nat(1));
    assert(list?.unrestricted == true);
    assert(list?.allowedTransferlists.length == 0);
  });

  it("Update existing transfer list as admin with no allowed lists should succeed", async () => {
    await a2.updateTransferlist(new Nat(0), Option.Some<[boolean, Array<Nat>]>([true, []]), { as: whitelister });
    const list = await a2.get_transferlists_value(new Nat(0));
    assert(list?.unrestricted == true);
    assert(list?.allowedTransferlists.length == 0);
  });

  it("Update existing transfer list as admin should succeed", async () => {
    await a2.updateTransferlist(new Nat(1), Option.Some<[boolean, Array<Nat>]>([true, [new Nat(0)]]), { as: whitelister });
    const list = await a2.get_transferlists_value(new Nat(1));
    assert(list?.unrestricted == true);
    assert(list?.allowedTransferlists.length == 1);
    assert(list?.allowedTransferlists[0].equals(new Nat(0)));
  });

  it("Update existing transfer list with null to delete it as admin should succeed", async () => {
    await a2.updateTransferlist(new Nat(3), Option.Some<[boolean, Array<Nat>]>([true, [new Nat(0)]]), { as: whitelister });
    const list = await a2.get_transferlists_value(new Nat(3));
    assert(list?.unrestricted == true);
    assert(list?.allowedTransferlists.length == 1);
    assert(list?.allowedTransferlists[0].equals(new Nat(0)));

    await a2.updateTransferlist(new Nat(3), Option.None(), { as: whitelister });
    const list0 = await a2.get_transferlists_value(new Nat(3));
    assert(list0 == undefined);
  });
});

describe("[A2] Remove super user", async () => {

  it("Remove super user in whitelist contract as non admin should fail", async () => {
    await expect_to_fail(async () => {
      await a2.removeSuperuser(superUser.get_address(), { as: carl });
    }, a2.errors.INVALID_CALLER)
  });

  it("Remove non existing super user from whitelist contract should succeed", async () => {
    const superusers_before = await a2.get_superusers();
    assert(superusers_before.length == 1)
    assert(superusers_before[0].equals(superUser.get_address()))
    await a2.removeSuperuser(carl.get_address(), { as: whitelister });
    const superusers_after = await a2.get_superusers();
    assert(superusers_after.length == 1)
    assert(superusers_after[0].equals(superUser.get_address()))
  });

  it("Remove existing super user from whitelist contract should succeed", async () => {
    const superusers_before = await a2.get_superusers();
    assert(superusers_before.length == 1)
    assert(superusers_before[0].equals(superUser.get_address()))
    await a2.removeSuperuser(superUser.get_address(), { as: whitelister });
    const superusers_after = await a2.get_superusers();
    assert(superusers_after.length == 0)
    await a2.addSuperuser(superUser.get_address(), { as: whitelister });
    const superusers_final = await a2.get_superusers();
    assert(superusers_final.length == 1)
    assert(superusers_final[0].equals(superUser.get_address()))
  });
});

describe("[A2] Assert receivers", async () => {
  it("Set up users for assert receivers tests", async () => {
    await a2.updateTransferlist(new Nat(2), Option.Some<[boolean, Array<Nat>]>([false, []]), { as: whitelister });
    await a2.updateUsers([
      [list2User1.get_address(), Option.Some(new Nat(2))],
      [carl.get_address(), Option.Some(new Nat(2))],
      [list1User1.get_address(), Option.None()]
    ], { as: whitelister });
  });

  it("Assert receivers with only restricted users should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertReceivers([list2User1.get_address(), carl.get_address()], {
        as: whitelister
      });
    }, a2.errors.USER_RESTRICTED)
  });

  it("Assert receivers with restricted and non restricted users should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertReceivers([carl.get_address(), list1User1.get_address()], { as: whitelister });
    }, a2.errors.USER_RESTRICTED)
  });

  it("Assert receivers with unknown users should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertReceivers([whitelister.get_address()], { as: whitelister });
    }, a2.errors.USER_RESTRICTED)
  });

  it("Assert receivers with users without allowed list should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertReceivers([list1User1.get_address()], { as: whitelister });
    }, a2.errors.USER_RESTRICTED)
  });

  it("Assert receivers with unrestricted users should succeed", async () => {
    await a2.assertReceivers([list0User2.get_address(), list0User1.get_address()], { as: whitelister });
  });
});

describe("[A2] Assert transfers", async () => {
  it("Assert transfers [FROM: restriced, TO: restriced] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[carl.get_address(), [list2User1.get_address()]]], { as: whitelister });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: not whitelisted, TO: not whitelisted] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[daniel.get_address(), [eddy.get_address()]]], { as: whitelister });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: restricted, TO: not whitelisted] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[carl.get_address(), [eddy.get_address()]]], { as: whitelister });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: not whitelisted, TO: restricted] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[eddy.get_address(), [carl.get_address()]]], { as: whitelister });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: whitelisted unrestricted, TO: restricted] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[list0User1.get_address(), [carl.get_address()]]], { as: whitelister });
    }, string_to_mich("\"TO_RESTRICTED\""))
  });

  it("Assert transfers [FROM: whitelisted unrestricted, TO: not whitelisted] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[list0User1.get_address(), [eddy.get_address()]]], { as: whitelister });
    }, string_to_mich("\"TO_RESTRICTED\""))
  });

  it("Assert transfers [FROM: whitelisted unrestricted, TO: not in FROM allowed list] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[list1User2.get_address(), [list1User2.get_address()]]], { as: whitelister });
    }, string_to_mich("\"TO_NOT_ALLOWED\""))
  });

  it("Assert transfers [FROM: whitelisted unrestricted, TO: in FROM allowed list] should succeed", async () => {
    await a2.assertTransfers([[list1User2.get_address(), [list0User2.get_address()]]], { as: whitelister });
  });

  it("Assert transfers [FROM: not whitelisted, TO: not whitelisted, SENDER: SUPERUSER] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[jacky.get_address(), [eddy.get_address()]]], { as: superUser });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: whitelisted, TO: not whitelisted, SENDER: SUPERUSER] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[list1User2.get_address(), [jacky.get_address()]]], { as: superUser });
    }, string_to_mich("\"TO_RESTRICTED\""))
  });

  it("Assert transfers [FROM: restricted, TO: not whitelisted, SENDER: SUPERUSER] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[carl.get_address(), [jacky.get_address()]]], { as: superUser });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: not whitelisted, TO: restricted, SENDER: SUPERUSER] should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[jacky.get_address(), [carl.get_address()]]], { as: superUser });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: unrestricted, TO: not in FROM allowed list, SENDER: SUPERUSER] should succeed", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[carl.get_address(), [list1User2.get_address()]]], { as: superUser });
    }, string_to_mich("\"FROM_RESTRICTED\""))
  });

  it("Assert transfers [FROM: unrestricted, TO: restricted, SENDER: SUPERUSER] should succeed", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransfers([[list1User2.get_address(), [carl.get_address()]]], { as: superUser });
    }, string_to_mich("\"TO_RESTRICTED\""))
  });

  it("Assert transfers [FROM: whitelisted unrestricted, TO: in FROM allowed list, , SENDER: SUPERUSER] should succeed", async () => {
    await a2.assertTransfers([[list1User2.get_address(), [list0User2.get_address()]]], { as: superUser });
  });
});

describe("[A2] Assert transfer list", async () => {
  it("Assert transfer list with non existing from transfer list should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransferlist(new Nat(123), new Nat(1), { as: whitelister });
    }, a2.errors.FROM_TRANSFERLIST_NOT_FOUND)
  });

  it("Assert transfer list with non existing to transfer list should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransferlist(new Nat(1), new Nat(123), { as: whitelister });
    }, a2.errors.TO_TRANSFERLIST_NOT_FOUND)
  });

  it("Assert transfer list with restricted existing from transfer list should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransferlist(new Nat(2), new Nat(1), { as: whitelister });
    }, a2.errors.FROM_INVALID_UNRESTRICTED_STATE)
  });

  it("Assert transfer list with restricted existing to transfer list should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransferlist(new Nat(1), new Nat(2), { as: whitelister });
    }, a2.errors.TO_INVALID_UNRESTRICTED_STATE)
  });

  it("Assert transfer list with to transfer list not in from allowed lists should fail", async () => {
    await expect_to_fail(async () => {
      await a2.assertTransferlist(new Nat(1), new Nat(1), { as: whitelister });
    }, a2.errors.TO_TRANSFERLIST_NOT_FOUND_IN_FROM)
  });

  it("Assert transfer list with to transfer list  in from allowed lists should succeed", async () => {
    await a2.assertTransferlist(new Nat(1), new Nat(0), { as: whitelister });
  });
});
