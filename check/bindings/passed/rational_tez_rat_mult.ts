import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (dest: att.Address): att.Micheline => {
    return dest.to_mich();
}
export class Rational_tez_rat_mult {
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
        const address = (await ex.deploy("../tests/passed/rational_tez_rat_mult.arl", {}, params)).address;
        this.address = address;
    }
    async exec(dest: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(dest), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(dest: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(dest), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const rational_tez_rat_mult = new Rational_tez_rat_mult();
