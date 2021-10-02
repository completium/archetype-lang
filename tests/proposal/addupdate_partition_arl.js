const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

const prefix_path = "/home/guillaume/archetype/archetype-lang/tests/proposal"

let contract;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy", async () => {
  it("addupdate_desuggared_partition", async () => {
    [contract, _] = await deploy(prefix_path + '/addupdate_partition.arl', {
      // as: admin.pkh
    });
  });
});


describe("Check", async () => {
  it("exec", async () => {
    let storage = await contract.getStorage();

    assert(storage.o_asset.has("oid") == false)
    assert(storage.my_asset.get("id0").col.length == 0)

    await contract.exec({ })

    storage = await contract.getStorage();
    assert(storage.o_asset.get("oid").toNumber() == 0)
    assert(storage.my_asset.get("id0").col.length == 1)
    assert(storage.my_asset.get("id0").col[0] === "oid")
  });

});
