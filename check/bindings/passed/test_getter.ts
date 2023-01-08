import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const getN_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export const deploy_getN_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("getN", att.prim_annot_to_mich_type("nat", []), params);
};
export class Test_getter {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    getN_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/test_getter.arl", {}, params)).address;
        this.address = address;
        this.getN_callback_address = (await deploy_getN_callback(params)).address;
    }
    async getN(params: Partial<ex.Parameters>): Promise<att.Nat> {
        if (this.address != undefined) {
            if (this.getN_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.getN_callback_address), "callback");
                await ex.call(this.address, "getN", att.getter_args_to_mich(getN_arg_to_mich(), entrypoint), params);
                return await ex.get_callback_value<att.Nat>(this.getN_callback_address, x => { return att.Nat.from_mich(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_n(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_getter = new Test_getter();
