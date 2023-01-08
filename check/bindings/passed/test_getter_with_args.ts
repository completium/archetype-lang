import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const getSum_arg_to_mich = (a: att.Nat, b: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        a.to_mich(),
        b.to_mich()
    ]);
}
export const deploy_getSum_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("getSum", att.prim_annot_to_mich_type("nat", []), params);
};
export class Test_getter_with_args {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    getSum_callback_address: string | undefined;
    get_address(): att.Address {
        if (undefined != this.address) {
            return new att.Address(this.address);
        }
        throw new Error("Contract not initialised");
    }
    async get_balance(): Promise<att.Tez> {
        if (null != this.address) {
            return await ex.get_balance(new att.Address(this.address));
        }
        throw new Error("Contract not initialised");
    }
    async deploy(params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/passed/test_getter_with_args.arl", {}, params)).address;
        this.address = address;
        this.getSum_callback_address = (await deploy_getSum_callback(params)).address;
    }
    async getSum(a: att.Nat, b: att.Nat, params: Partial<ex.Parameters>): Promise<att.Nat> {
        if (this.address != undefined) {
            if (this.getSum_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.getSum_callback_address), "callback");
                await ex.call(this.address, "getSum", att.getter_args_to_mich(getSum_arg_to_mich(a, b), entrypoint), params);
                return await ex.get_callback_value<att.Nat>(this.getSum_callback_address, x => { return att.Nat.from_mich(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_getter_with_args = new Test_getter_with_args();
