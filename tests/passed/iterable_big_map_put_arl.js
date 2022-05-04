const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let contract;

setEndpoint('mockup');
setAccount('alice');
setQuiet(true);

describe("Deploy", async () => {
  it("iterable_big_map_put_arl", async () => {
    [contract, _] = await deploy('./tests/passed/iterable_big_map_put.arl', {
      // as: admin.pkh
    });
  });
});


describe("Check", async () => {
  it("check", async () => {
    const storage_before = await contract.getStorage();
    const bm = storage_before;

    const res_bm_before_0 = await getValueFromBigMap(bm, {"string" : "mystr0"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_before_0) === '0');

    const res_bm_before_1 = await getValueFromBigMap(bm, {"string" : "mystr1"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_before_1) === '1');

    const res_bm_before_2 = await getValueFromBigMap(bm, {"string" : "mystr2"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_before_2) === '2');

    const res_bm_before_3 = await getValueFromBigMap(bm, {"string" : "mystr3"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_before_3) === '3');

    await contract.exec({});

    const res_bm_after_0 = await getValueFromBigMap(bm, {"string" : "mystr0"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_after_0) === '0');

    const res_bm_after_1 = await getValueFromBigMap(bm, {"string" : "mystr1"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_after_1) === '1');

    const res_bm_after_2 = await getValueFromBigMap(bm, {"string" : "mystr2"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_after_2) === '2');

    const res_bm_after_3 = await getValueFromBigMap(bm, {"string" : "mystr3"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_after_3) === '3');

    const res_bm_after_4 = await getValueFromBigMap(bm, {"string" : "mystr4"}, {prim : "string"});
    assert(jsonMichelineToExpr(res_bm_after_4) === '4');
  })
});
