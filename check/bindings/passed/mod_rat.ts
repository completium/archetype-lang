import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (i: att.Rational, j: att.Rational): att.Micheline => {
    return att.pair_to_mich([
        i.to_mich(),
        j.to_mich()
    ]);
}
export class Mod_rat {
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
        const address = (await ex.deploy("../tests/passed/mod_rat.arl", {}, params)).address;
        this.address = address;
    }
    async exec(i: att.Rational, j: att.Rational, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(i, j), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(i: att.Rational, j: att.Rational, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(i, j), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const mod_rat = new Mod_rat();
