import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const o_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("int", []);
export const o_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("int", []);
export const my_asset_value_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []);
export type o_asset_container = Array<[
    string,
    att.Int
]>;
export type my_asset_container = Array<[
    att.Int,
    Array<string>
]>;
export const o_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("int", []), []);
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), []);
const add_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Effect_add_asset_with_complex_partition {
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
        const address = (await ex.deploy("../tests/passed/effect_add_asset_with_complex_partition.arl", {}, params)).address;
        this.address = address;
    }
    async add(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "add", add_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_add_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "add", add_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_o_asset(): Promise<o_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.mich_to_string(x), att.Int.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[1], (x, y) => [att.Int.from_mich(x), att.mich_to_list(y, x => { return att.mich_to_string(x); })]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const effect_add_asset_with_complex_partition = new Effect_add_asset_with_complex_partition();
