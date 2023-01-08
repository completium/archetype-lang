import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const setN_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const view_get_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Test_view {
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
        const address = (await ex.deploy("../tests/passed/test_view.arl", {}, params)).address;
        this.address = address;
    }
    async setN(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setN", setN_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setN_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setN", setN_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async view_get(params: Partial<ex.Parameters>): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "get", view_get_arg_to_mich(), params);
            return mich.value ? att.Nat.from_mich(mich.value) : undefined;
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
export const test_view = new Test_view();
