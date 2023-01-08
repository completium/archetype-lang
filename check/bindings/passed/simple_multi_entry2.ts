import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const e1_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const e2_arg_to_mich = (a: att.Nat, b: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        a.to_mich(),
        b.to_mich()
    ]);
}
const e3_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Simple_multi_entry2 {
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
        const address = (await ex.deploy("../tests/passed/simple_multi_entry2.arl", {}, params)).address;
        this.address = address;
    }
    async e1(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e1", e1_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async e2(a: att.Nat, b: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e2", e2_arg_to_mich(a, b), params);
        }
        throw new Error("Contract not initialised");
    }
    async e3(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e3", e3_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e1_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e1", e1_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e2_param(a: att.Nat, b: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e2", e2_arg_to_mich(a, b), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e3_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e3", e3_arg_to_mich(), params);
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
export const simple_multi_entry2 = new Simple_multi_entry2();
