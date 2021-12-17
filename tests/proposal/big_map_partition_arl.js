const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let contract;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy", async () => {
  it("big_map_partition", async () => {
    [contract, _] = await deploy('./tests/proposal/big_map_partition.arl', {
      // as: admin.pkh
    });
  });
});


describe("Check", async () => {
  it("init", async () => {
    // Check if
    // entry init() {
    // a.add({0; [{2; "toto"}; {4; "tata"}; {5; "titi"}]});
    // a.add({1; [{3; "tutu"}]});
    await contract.init({ });
    const storage = await contract.getStorage();
    const bm_a = storage.a;
    const bm_b = storage.b;

    const res_bm_a_0 = await getValueFromBigMap(bm_a, {"int" : "0"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_a_0) === '{2; 4; 5}');

    const res_bm_a_1 = await getValueFromBigMap(bm_a, {"int" : "1"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_a_1) === '{3}');

    const res_bm_b_2 = await getValueFromBigMap(bm_b, {"int" : "2"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_b_2) === '"toto"');

    const res_bm_b_3 = await getValueFromBigMap(bm_b, {"int" : "3"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_b_3) === '"tutu"');

    const res_bm_b_4 = await getValueFromBigMap(bm_b, {"int" : "4"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_b_4) === '"tata"');

    const res_bm_b_5 = await getValueFromBigMap(bm_b, {"int" : "5"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_b_5) === '"titi"');
  });

  // it("check iteration", async () => {
  //   await contract.init({ })
  // });

  it("check removeif with predicate", async () => {
    await contract.removeEven({ })

    const storage = await contract.getStorage();
    const bm_a = storage.a;
    const bm_b = storage.b;

    const res_bm_a_0 = await getValueFromBigMap(bm_a, {"int" : "0"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_a_0) === '{5}');

    const res_bm_a_1 = await getValueFromBigMap(bm_a, {"int" : "1"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_a_1) === '{3}');

    const res_bm_b_2 = await getValueFromBigMap(bm_b, {"int" : "2"}, {prim : "nat"});
    assert(res_bm_b_2 === null);

    const res_bm_b_3 = await getValueFromBigMap(bm_b, {"int" : "3"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_b_3) === '"tutu"');

    const res_bm_b_4 = await getValueFromBigMap(bm_b, {"int" : "4"}, {prim : "nat"});
    assert(res_bm_b_4 === null);

    const res_bm_b_5 = await getValueFromBigMap(bm_b, {"int" : "5"}, {prim : "nat"});
    assert(jsonMichelineToExpr(res_bm_b_5) === '"titi"');

  });

});
