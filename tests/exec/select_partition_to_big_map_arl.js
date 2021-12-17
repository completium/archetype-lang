const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let contract;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy", async () => {
  it("select_partition", async () => {
    [contract, _] = await deploy('./tests/passed/select_partition_big_map.arl', {
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

    const bm_o_asset = storage.o_asset;
    const res0 = await getValueFromBigMap(bm_o_asset, {"string" : "tutu"}, {prim : "string"});
    assert(jsonMichelineToExpr(res0) == '2')

    const bm_my_asset = storage.my_asset;
    const res1 = await getValueFromBigMap(bm_my_asset, {"string" : "toto"}, {prim : "string"});
    assert(jsonMichelineToExpr(res1[0]) == '"tutu"')
    assert(res1.length === 1)
  });

});
