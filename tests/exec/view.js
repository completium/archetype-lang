const { deploy, getValueFromBigMap, setAccount, setEndpoint, setQuiet, jsonMichelineToExpr } = require('@completium/completium-cli');
const assert = require('assert');

let view_args_several_args_and_storage;
let view_args_several_args;
let view_args_several_stovars_1;
let view_args_several_stovars_2;
let view_args_several_stovars_3;
let view_args_several_stovars_4;
let view_args_several_stovars_5;
let view_args_storage_no_no;
let view_args_storage_yes_no;
let view_args_storage_no_yes;
let view_args_storage_yes_yes;
let view_asset;

setEndpoint('mockup');
setAccount('guillaume');
setQuiet(true);

describe("Deploy", async () => {
  it("view_args_storage_no_no", async () => {
    [view_args_storage_no_no, _] = await deploy('./tests/passed/view_args_storage_no_no.arl', {
      force_tezos_client: true
    });
  });
  it("view_args_storage_yes_no", async () => {
    [view_args_storage_yes_no, _] = await deploy('./tests/passed/view_args_storage_yes_no.arl', {
      force_tezos_client: true
    });
  });
  it("view_args_storage_no_yes", async () => {
    [view_args_storage_no_yes, _] = await deploy('./tests/passed/view_args_storage_no_yes.arl', {
      force_tezos_client: true
    });
  });
  // it("view_args_storage_yes_yes", async () => {
  //   [view_args_storage_yes_yes, _] = await deploy('./tests/passed/view_args_storage_yes_yes.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_stovars_1", async () => {
  //   [view_args_several_stovars_1, _] = await deploy('./tests/passed/view_args_several_stovars_1.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_stovars_2", async () => {
  //   [view_args_several_stovars_2, _] = await deploy('./tests/passed/view_args_several_stovars_2.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_stovars_3", async () => {
  //   [view_args_several_stovars_3, _] = await deploy('./tests/passed/view_args_several_stovars_3.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_stovars_4", async () => {
  //   [view_args_several_stovars_4, _] = await deploy('./tests/passed/view_args_several_stovars_4.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_stovars_5", async () => {
  //   [view_args_several_stovars_5, _] = await deploy('./tests/passed/view_args_several_stovars_5.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_args", async () => {
  //   [view_args_several_args, _] = await deploy('./tests/passed/view_args_several_args.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_args_several_args_and_storage", async () => {
  //   [view_args_several_args_and_storage, _] = await deploy('./tests/passed/view_args_several_args_and_storage.arl', {
  //     force_tezos_client: true
  //   });
  // });
  // it("view_asset", async () => {
  //   [view_asset, _] = await deploy('./tests/passed/view_asset.arl', {
  //     force_tezos_client: true
  //   });
  // });
});

describe("Check", async () => {

  async function testRes(contract, expected) {
    await contract.exec({});
    const storage = await contract.getStorage();
    const res = storage.toNumber();
    assert(res == expected, `Expected: ${expected} when result is ${res}`)
  }

  it("Check view_args_storage_no_no", async () => {
    await testRes(view_args_storage_no_no, 2)
  });

  it("Check view_args_storage_yes_no", async () => {
    await testRes(view_args_storage_yes_no, 2)
  });

  it("Check view_args_storage_no_yes", async () => {
    await testRes(view_args_storage_no_yes, 2)
  });

  // it("Check view_args_storage_no_no", async () => {
  //   await view_args_storage_no_no.exec({});
  //   const storage = await view_args_storage_no_no.getStorage();
  //   assert(storage.toNumber() == 2, "")
  // });

  // it("Check view_args_several", async () => {
  //   await view_args_several_args.exec({
  //     args: {
  //       n: 1,
  //       s: "toto",
  //       b: "0x123456"
  //     }
  //   });
  //   const storage = await view_args_several_args.getStorage();
  //   assert(storage.toNumber() == 8, "")
  // });

});
