const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let contract;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy addupdate_with_no_effect_on_default_value", async () => {
  it("addupdate_with_no_effect_on_default_value", async () => {
    [contract, _] = await deploy('./tests/passed/addupdate_with_no_effect_on_default_value.arl', {
      // as: admin.pkh
    });
  });
});


describe("Check addupdate_with_no_effect_on_default_value", async () => {

  async function checkStorage(prefix, expectedV, expectedC) {
    const storage = await contract.getStorage();

    const item = storage.my_asset.get("id0");
    assert(item.v.toNumber() == expectedV, `Expected after ${prefix} v: ${expectedV} when value is ${item.v.toNumber()}`)
    assert(item.c.toNumber() == expectedC, `Expected after ${prefix} c: ${expectedC} when value is ${item.c.toNumber()}`)
  }

  it("exec addupdate_with_no_effect_on_default_value", async () => {
    await contract.exec({})
    await checkStorage("exec", 0, 1);
  });

  it("exec2 addupdate_with_no_effect_on_default_value", async () => {
    await contract.exec2({})
    await checkStorage("exec2", 1, 1);
  });

});
