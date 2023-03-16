import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const callback_arg_to_mich = (k: att.Nat): att.Micheline => {
    return k.to_mich();
}
const exec_arg_to_mich = (a: att.Address): att.Micheline => {
    return a.to_mich();
}
const getN_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export const deploy_getN_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("getN", att.prim_annot_to_mich_type("nat", []), params);
};
export class Test_caller_getter {
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
        const address = (await ex.deploy("../tests/passed/test_caller_getter.arl", {}, params)).address;
        this.address = address;
        this.getN_callback_address = (await deploy_getN_callback(params)).address;
    }
    async callback(k: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(k: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
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
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_ENTRY: att.string_to_mich("\"INVALID_ENTRY\"")
    };
}
export const test_caller_getter = new Test_caller_getter();
