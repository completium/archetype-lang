import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const myasset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export type myasset_container = Array<string>;
export const myasset_container_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []);
const exec_arg_to_mich = (k: string): att.Micheline => {
    return att.string_to_mich(k);
}
export class Shadow_field {
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
        const address = (await ex.deploy("../tests/passed/shadow_field.arl", {}, params)).address;
        this.address = address;
    }
    async exec(k: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(k: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_myasset(): Promise<myasset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.mich_to_string(x), att.Int.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        f1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"f1\"")])
    };
}
export const shadow_field = new Shadow_field();
