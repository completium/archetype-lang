const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let view_0;
let view_args_0;
let view_args_1;
let view_args_storage_0;
let view_args_storage_1;
let view_asset;
let view_storage_0;
let view_storage_1;
let view_storage_2;
let view_storage_3;
let view_storage_4;
let view_storage_5;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy", async () => {
  it("view_0", async () => {
    [view_0, _] = await deploy('./tests/passed/view_0.arl', {
      force_tezos_client: true
    });
  });
  it("view_args_0", async () => {
    [view_args_0, _] = await deploy('./tests/passed/view_args_0.arl', {
      force_tezos_client: true
    });
  });
  it("view_args_1", async () => {
    [view_args_1, _] = await deploy('./tests/passed/view_args_1.arl', {
      force_tezos_client: true
    });
  });
  it("view_args_storage_0", async () => {
    [view_args_storage_0, _] = await deploy('./tests/passed/view_args_storage_0.arl', {
      force_tezos_client: true
    });
  });
  it("view_args_storage_1", async () => {
    [view_args_storage_1, _] = await deploy('./tests/passed/view_args_storage_1.arl', {
      force_tezos_client: true
    });
  });
  it("view_asset", async () => {
    [view_asset, _] = await deploy('./tests/passed/view_asset.arl', {
      force_tezos_client: true
    });
  });
  it("view_storage_0", async () => {
    [view_storage_0, _] = await deploy('./tests/passed/view_storage_0.arl', {
      force_tezos_client: true
    });
  });
  it("view_storage_1", async () => {
    [view_storage_1, _] = await deploy('./tests/passed/view_storage_1.arl', {
      force_tezos_client: true
    });
  });
  it("view_storage_2", async () => {
    [view_storage_2, _] = await deploy('./tests/passed/view_storage_2.arl', {
      force_tezos_client: true
    });
  });
  it("view_storage_3", async () => {
    [view_storage_3, _] = await deploy('./tests/passed/view_storage_3.arl', {
      force_tezos_client: true
    });
  });
  it("view_storage_4", async () => {
    [view_storage_4, _] = await deploy('./tests/passed/view_storage_4.arl', {
      force_tezos_client: true
    });
  });
  it("view_storage_5", async () => {
    [view_storage_5, _] = await deploy('./tests/passed/view_storage_5.arl', {
      force_tezos_client: true
    });
  });
});

describe("Check", async () => {

  async function getStorage(contract) {
    await contract.exec({});
    const storage = await contract.getStorage();
    return storage;
  }

  function check(value, expected) {
    assert(value === expected, `Expected: ${expected} when value is ${value}`)
  }

  it("Check view_0", async () => {
    const storage = await getStorage(view_0);
    check(storage.toNumber(), 2);
  });

  it("Check view_args_0", async () => {
    const storage = await getStorage(view_args_0);
    check(storage.toNumber(), 2);
  });

  it("Check view_args_1", async () => {
    const storage = await getStorage(view_args_1);
    check(storage.toNumber(), 8);
  });

  it("Check view_args_storage_0", async () => {
    const storage = await getStorage(view_args_storage_0);
    check(storage.res.toNumber(), 4);
  });

  it("Check view_args_storage_1", async () => {
    const storage = await getStorage(view_args_storage_1);
    check(storage.res.toNumber(), 21);
  });

  it("Check view_asset", async () => {
    const storage = await getStorage(view_asset);
    check(storage.res, "value");
  });

  it("Check view_storage_0", async () => {
    const storage = await getStorage(view_storage_0);
    check(storage.res.toNumber(), 2);
  });

  it("Check view_storage_1", async () => {
    const storage = await getStorage(view_storage_1);
    check(storage.res.toNumber(), 1);
  });

  it("Check view_storage_2", async () => {
    const storage = await getStorage(view_storage_2);
    check(storage.res.toNumber(), 3);
  });

  it("Check view_storage_3", async () => {
    const storage = await getStorage(view_storage_3);
    check(storage.res.toNumber(), 5);
  });

  it("Check view_storage_4", async () => {
    const storage = await getStorage(view_storage_4);
    check(storage.res.toNumber(), 3);
  });

  it("Check view_storage_5", async () => {
    const storage = await getStorage(view_storage_5);
    check(storage.res.toNumber(), 5);
  });

});
