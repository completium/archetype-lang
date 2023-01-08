import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const getArg_arg_to_mich = (s: string): att.Micheline => {
    return att.string_to_mich(s);
}
export const deploy_getArg_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("getArg", att.prim_annot_to_mich_type("string", []), params);
};
export class Test_getter_with_arg {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    getArg_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/test_getter_with_arg.arl", {}, params)).address;
        this.address = address;
        this.getArg_callback_address = (await deploy_getArg_callback(params)).address;
    }
    async getArg(s: string, params: Partial<ex.Parameters>): Promise<string> {
        if (this.address != undefined) {
            if (this.getArg_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.getArg_callback_address), "callback");
                await ex.call(this.address, "getArg", att.getter_args_to_mich(getArg_arg_to_mich(s), entrypoint), params);
                return await ex.get_callback_value<string>(this.getArg_callback_address, x => { return att.mich_to_string(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_getter_with_arg = new Test_getter_with_arg();
