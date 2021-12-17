const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let contract;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy", async () => {
  it("select_partition", async () => {
    [contract, _] = await deploy('./tests/passed/select_partition.arl', {
      // as: admin.pkh
    });
  });
});


describe("Check", async () => {
  it("init", async () => {
    await contract.init({ });
  });

  // it("check iteration", async () => {
  //   await contract.init({ })
  // });

  it("check removeif with predicate", async () => {
    await contract.exec({ })

    const storage = await contract.getStorage();
    assert(storage.o_asset.get("tutu").toNumber() == 2)

    const bm_my_asset = storage.my_asset;
    const res = await getValueFromBigMap(bm_my_asset, {"string" : "toto"}, {prim : "string"});
    assert(jsonMichelineToExpr(res[0]) == '"tutu"')
    assert(res.length === 1)
  });

});
