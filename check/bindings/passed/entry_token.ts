import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const getBalance_arg_to_mich = (cb: att.Entrypoint): att.Micheline => {
    return cb.to_mich();
}
const setBalance_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
export class Entry_token {
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
        const address = (await ex.deploy("../tests/passed/entry_token.arl", {}, params)).address;
        this.address = address;
    }
    async getBalance(cb: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "getBalance", getBalance_arg_to_mich(cb), params);
        }
        throw new Error("Contract not initialised");
    }
    async setBalance(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setBalance", setBalance_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_getBalance_param(cb: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "getBalance", getBalance_arg_to_mich(cb), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setBalance_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setBalance", setBalance_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_nbtokens(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const entry_token = new Entry_token();
