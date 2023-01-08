import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (x: att.Int): att.Micheline => {
    return x.to_mich();
}
export class Init_lambda {
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
        const address = (await ex.deploy("../tests/passed/init_lambda.arl", {}, params)).address;
        this.address = address;
    }
    async exec(x: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(x), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(x: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(x), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_f(): Promise<att.Micheline> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return (storage as att.Mpair).args[1];
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const init_lambda = new Init_lambda();
