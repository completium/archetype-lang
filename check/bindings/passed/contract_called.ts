import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const set_value_arg_to_mich = (n: att.Int): att.Micheline => {
    return n.to_mich();
}
const add_value_arg_to_mich = (a: att.Int, b: att.Int): att.Micheline => {
    return att.pair_to_mich([
        a.to_mich(),
        b.to_mich()
    ]);
}
export class Contract_called {
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
        const address = (await ex.deploy("../tests/passed/contract_called.arl", {}, params)).address;
        this.address = address;
    }
    async set_value(n: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_value", set_value_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async add_value(a: att.Int, b: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "add_value", add_value_arg_to_mich(a, b), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_value_param(n: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_value", set_value_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_add_value_param(a: att.Int, b: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "add_value", add_value_arg_to_mich(a, b), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_v(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const contract_called = new Contract_called();
