import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_simple_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("int", []);
export const my_asset_all_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export class my_asset_all_value implements att.ArchetypeType {
    constructor(public k: att.Int, public b: boolean) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.k.to_mich(), att.bool_to_mich(this.b)]);
    }
    equals(v: my_asset_all_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_asset_all_value {
        return new my_asset_all_value(att.Int.from_mich((input as att.Mpair).args[0]), att.mich_to_bool((input as att.Mpair).args[1]));
    }
}
export const my_asset_simple_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("int", []);
export const my_asset_value_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []);
export const my_asset_all_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%k"]),
    att.prim_annot_to_mich_type("bool", ["%b"])
], []);
export type my_asset_simple_container = Array<[
    string,
    att.Int
]>;
export type my_asset_container = Array<[
    att.Int,
    Array<string>
]>;
export type my_asset_all_container = Array<[
    att.Address,
    my_asset_all_value
]>;
export const my_asset_simple_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("int", []), []);
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), []);
export const my_asset_all_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%k"]),
    att.prim_annot_to_mich_type("bool", ["%b"])
], []), []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Test_asset {
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
        const address = (await ex.deploy("../tests/passed/test_asset.arl", {}, params)).address;
        this.address = address;
    }
    async exec(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset_simple(): Promise<my_asset_simple_container> {
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
    async get_my_asset_all(): Promise<my_asset_all_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[2], (x, y) => [att.Address.from_mich(x), my_asset_all_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        KO: att.string_to_mich("\"ko\"")
    };
}
export const test_asset = new Test_asset();
