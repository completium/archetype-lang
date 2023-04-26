import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const import_arl_asset_def__my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("bytes", []);
export class my_asset_value implements att.ArchetypeType {
    constructor(public v: boolean, public k: att.Int) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.bool_to_mich(this.v), this.k.to_mich()]);
    }
    equals(v: my_asset_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_asset_value {
        return new my_asset_value(att.mich_to_bool((input as att.Mpair).args[0]), att.Int.from_mich((input as att.Mpair).args[1]));
    }
}
export const import_arl_asset_def__my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const my_asset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bool", ["%v"]),
    att.prim_annot_to_mich_type("int", ["%k"])
], []);
export type import_arl_asset_def__my_asset_container = Array<[
    att.Nat,
    string
]>;
export type my_asset_container = Array<[
    att.Bytes,
    my_asset_value
]>;
export const import_arl_asset_def__my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("string", []), []);
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("bytes", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bool", ["%v"]),
    att.prim_annot_to_mich_type("int", ["%k"])
], []), []);
const exec_imported_arg_to_mich = (i: string): att.Micheline => {
    return att.string_to_mich(i);
}
const exec_top_arg_to_mich = (i: my_asset_value): att.Micheline => {
    return i.to_mich();
}
export class Import_arl_asset_value_use_arg_collide {
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
        const address = (await ex.deploy("../tests/passed/import_arl_asset_value_use_arg_collide.arl", {}, params)).address;
        this.address = address;
    }
    async exec_imported(i: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec_imported", exec_imported_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec_top(i: my_asset_value, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec_top", exec_top_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_imported_param(i: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec_imported", exec_imported_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_top_param(i: my_asset_value, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec_top", exec_top_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.Bytes.from_mich(x), my_asset_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_imported(): Promise<att.Option<string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[1], x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_res_top(): Promise<att.Option<my_asset_value>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[2], x => { return my_asset_value.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const import_arl_asset_value_use_arg_collide = new Import_arl_asset_value_use_arg_collide();
