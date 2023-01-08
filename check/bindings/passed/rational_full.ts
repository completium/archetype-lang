import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const anasset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const anasset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", []),
    att.prim_annot_to_mich_type("nat", [])
], []);
export type anasset_container = Array<[
    att.Address,
    att.Rational
]>;
export const anasset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", []),
    att.prim_annot_to_mich_type("nat", [])
], []), []);
const exec_arg_to_mich = (r3: att.Rational): att.Micheline => {
    return r3.to_mich();
}
export class Rational_full {
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
        const address = (await ex.deploy("../tests/passed/rational_full.arl", {}, params)).address;
        this.address = address;
    }
    async exec(r3: att.Rational, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(r3), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(r3: att.Rational, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(r3), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_anasset(): Promise<anasset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Address.from_mich(x), att.Rational.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        KO: att.string_to_mich("\"ko\"")
    };
}
export const rational_full = new Rational_full();
