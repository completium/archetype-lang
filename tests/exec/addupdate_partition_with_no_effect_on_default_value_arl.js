const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let contract;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy addupdate_partition_with_no_effect_on_default_value", async () => {
  it("addupdate_partition_with_no_effect_on_default_value", async () => {
    [contract, _] = await deploy('./tests/passed/addupdate_partition_with_no_effect_on_default_value.arl', {
      // as: admin.pkh
    });
  });
});


describe("Check addupdate_partition_with_no_effect_on_default_value", async () => {

  async function checkStorage(prefix, expectedV, expectedC) {
    const storage = await contract.getStorage();

    const item = storage.o_asset.get("oid");
    assert(item.v.toNumber() == expectedV, `Expected after ${prefix} v: ${expectedV} when value is ${item.v.toNumber()}`)
    assert(item.c.toNumber() == expectedC, `Expected after ${prefix} c: ${expectedC} when value is ${item.c.toNumber()}`)
  }

  it("init addupdate_partition_with_no_effect_on_default_value", async () => {
    await contract.init({})
    await checkStorage("init", 0, 1);
  });

  it("exec addupdate_partition_with_no_effect_on_default_value", async () => {
    await contract.exec({})
    await checkStorage("exec", 0, 1);
  });

  it("exec2 addupdate_partition_with_no_effect_on_default_value", async () => {
    await contract.exec2({})
    await checkStorage("exec2", 1, 1);
  });

});
