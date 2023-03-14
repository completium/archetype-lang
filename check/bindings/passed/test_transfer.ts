import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const f_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
const exec_arg_to_mich = (a: att.Address, c: att.Address): att.Micheline => {
    return att.pair_to_mich([
        a.to_mich(),
        c.to_mich()
    ]);
}
export class Test_transfer {
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
        const address = (await ex.deploy("../tests/passed/test_transfer.arl", {}, params)).address;
        this.address = address;
    }
    async f(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "f", f_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(a: att.Address, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a, c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_f_param(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "f", f_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: att.Address, c: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a, c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        NOT_FOUND: att.string_to_mich("\"NOT_FOUND\"")
    };
}
export const test_transfer = new Test_transfer();
