import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const incr_arg_to_mich = (v: att.Int): att.Micheline => {
    return v.to_mich();
}
const decr_arg_to_mich = (v: att.Int): att.Micheline => {
    return v.to_mich();
}
const reset_arg_to_mich = (v: att.Int): att.Micheline => {
    return v.to_mich();
}
export class Counter {
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
        const address = (await ex.deploy("../tests/passed/counter.arl", {}, params)).address;
        this.address = address;
    }
    async incr(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "incr", incr_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async decr(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "decr", decr_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async reset(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "reset", reset_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_incr_param(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "incr", incr_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_decr_param(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "decr", decr_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_reset_param(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "reset", reset_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_value(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const counter = new Counter();
