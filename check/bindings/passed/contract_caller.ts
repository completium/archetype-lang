import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const update_value_arg_to_mich = (n: att.Int, c: att.Address): att.Micheline => {
    return att.pair_to_mich([
        n.to_mich(),
        c.to_mich()
    ]);
}
const add_one_arg_to_mich = (n: att.Int, c: att.Address): att.Micheline => {
    return att.pair_to_mich([
        n.to_mich(),
        c.to_mich()
    ]);
}
export class Contract_caller {
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
        const address = (await ex.deploy("../tests/passed/contract_caller.arl", {}, params)).address;
        this.address = address;
    }
    async update_value(n: att.Int, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update_value", update_value_arg_to_mich(n, c), params);
        }
        throw new Error("Contract not initialised");
    }
    async add_one(n: att.Int, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "add_one", add_one_arg_to_mich(n, c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_value_param(n: att.Int, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update_value", update_value_arg_to_mich(n, c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_add_one_param(n: att.Int, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "add_one", add_one_arg_to_mich(n, c), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const contract_caller = new Contract_caller();
