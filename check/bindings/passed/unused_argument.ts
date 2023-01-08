import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const e_arg_to_mich = (s: string, i: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        att.string_to_mich(s),
        i.to_mich()
    ]);
}
export class Unused_argument {
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
        const address = (await ex.deploy("../tests/passed/unused_argument.arl", {}, params)).address;
        this.address = address;
    }
    async e(s: string, i: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e", e_arg_to_mich(s, i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e_param(s: string, i: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e", e_arg_to_mich(s, i), params);
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
export const unused_argument = new Unused_argument();
