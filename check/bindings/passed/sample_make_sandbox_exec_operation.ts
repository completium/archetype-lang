import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (l: att.Micheline): att.Micheline => {
    return (x => x)(l);
}
export class Sample_make_sandbox_exec_operation {
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
        const address = (await ex.deploy("../tests/passed/sample_make_sandbox_exec_operation.arl", {}, params)).address;
        this.address = address;
    }
    async exec(l: att.Micheline, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(l), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(l: att.Micheline, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(l), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        ERROR: att.string_to_mich("\"ERROR\"")
    };
}
export const sample_make_sandbox_exec_operation = new Sample_make_sandbox_exec_operation();
