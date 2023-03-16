import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const myaction1_arg_to_mich = (a: att.Address, t: att.Tez): att.Micheline => {
    return att.pair_to_mich([
        a.to_mich(),
        t.to_mich()
    ]);
}
const myaction2_arg_to_mich = (s: string): att.Micheline => {
    return att.string_to_mich(s);
}
const pay_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const exec_arg_to_mich = (a: att.Address): att.Micheline => {
    return a.to_mich();
}
export class Test_contract {
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
        const address = (await ex.deploy("../tests/passed/test_contract.arl", {}, params)).address;
        this.address = address;
    }
    async myaction1(a: att.Address, t: att.Tez, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "myaction1", myaction1_arg_to_mich(a, t), params);
        }
        throw new Error("Contract not initialised");
    }
    async myaction2(s: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "myaction2", myaction2_arg_to_mich(s), params);
        }
        throw new Error("Contract not initialised");
    }
    async pay(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "pay", pay_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_myaction1_param(a: att.Address, t: att.Tez, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "myaction1", myaction1_arg_to_mich(a, t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_myaction2_param(s: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "myaction2", myaction2_arg_to_mich(s), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_pay_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "pay", pay_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_contract = new Test_contract();
