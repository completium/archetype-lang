import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (v: att.Int, c: att.Address): att.Micheline => {
    return att.pair_to_mich([
        v.to_mich(),
        c.to_mich()
    ]);
}
export class Counter_proxy {
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
        const address = (await ex.deploy("../tests/passed/counter_proxy.arl", {}, params)).address;
        this.address = address;
    }
    async exec(v: att.Int, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(v, c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(v: att.Int, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(v, c), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const counter_proxy = new Counter_proxy();
