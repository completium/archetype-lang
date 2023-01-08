import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const u_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const u_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", []),
    att.prim_annot_to_mich_type("nat", [])
], []);
export type u_container = Array<[
    att.Address,
    att.Rational
]>;
export const u_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", []),
    att.prim_annot_to_mich_type("nat", [])
], []), []);
const start_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Rational_in_formula {
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
        const address = (await ex.deploy("../tests/passed/rational_in_formula.arl", {}, params)).address;
        this.address = address;
    }
    async start(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "start", start_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_start_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "start", start_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_u(): Promise<u_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Address.from_mich(x), att.Rational.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const rational_in_formula = new Rational_in_formula();
