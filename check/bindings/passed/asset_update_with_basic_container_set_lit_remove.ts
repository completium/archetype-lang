import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export class my_asset_value implements att.ArchetypeType {
    constructor(public v0: att.Nat, public c: Array<att.Nat>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.v0.to_mich(), att.list_to_mich(this.c, x => {
                return x.to_mich();
            })]);
    }
    equals(v: my_asset_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_asset_value {
        return new my_asset_value(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }));
    }
}
export const my_asset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%v0"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%c"])
], []);
export type my_asset_container = Array<[
    string,
    my_asset_value
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%v0"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%c"])
], []), []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Asset_update_with_basic_container_set_lit_remove {
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
        const address = (await ex.deploy("../tests/passed/asset_update_with_basic_container_set_lit_remove.arl", {}, params)).address;
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
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.mich_to_string(x), my_asset_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const asset_update_with_basic_container_set_lit_remove = new Asset_update_with_basic_container_set_lit_remove();
