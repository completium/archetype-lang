import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const set_res_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
const exec_arg_to_mich = (a: att.Address): att.Micheline => {
    return a.to_mich();
}
export class Import_arl_entrypoint_use {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
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
        const address = (await ex.deploy("../tests/passed/import_arl_entrypoint_use.arl", {}, params)).address;
        this.address = address;
    }
    async set_res(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_res", set_res_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_res_param(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_res", set_res_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a), params);
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
    errors = {};
}
export const import_arl_entrypoint_use = new Import_arl_entrypoint_use();
