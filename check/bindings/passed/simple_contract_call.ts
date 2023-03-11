import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const an_entry_arg_to_mich = (s: string): att.Micheline => {
    return att.string_to_mich(s);
}
const exec_arg_to_mich = (contract_i: att.Address): att.Micheline => {
    return contract_i.to_mich();
}
export class Simple_contract_call {
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
        const address = (await ex.deploy("../tests/passed/simple_contract_call.arl", {}, params)).address;
        this.address = address;
    }
    async an_entry(s: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "an_entry", an_entry_arg_to_mich(s), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(contract_i: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(contract_i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_an_entry_param(s: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "an_entry", an_entry_arg_to_mich(s), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(contract_i: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(contract_i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const simple_contract_call = new Simple_contract_call();
