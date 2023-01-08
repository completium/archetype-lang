import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const assign_arg_to_mich = (v: att.Int): att.Micheline => {
    return v.to_mich();
}
export class Simple_arg_int {
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
        const address = (await ex.deploy("../tests/passed/simple_arg_int.arl", {}, params)).address;
        this.address = address;
    }
    async assign(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "assign", assign_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_assign_param(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "assign", assign_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_n(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const simple_arg_int = new Simple_arg_int();
