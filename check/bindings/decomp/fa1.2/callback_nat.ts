import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const callback_arg_to_mich = (i: att.Nat): att.Micheline => {
    return i.to_mich();
}
const reset_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Callback_nat {
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
        const address = (await ex.deploy("./archetype/decomp/fa1.2/callback_nat.arl", {}, params)).address;
        this.address = address;
    }
    async callback(i: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async reset(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "reset", reset_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(i: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_reset_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "reset", reset_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Option<att.Nat>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich(storage, x => { return att.Nat.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const callback_nat = new Callback_nat();
