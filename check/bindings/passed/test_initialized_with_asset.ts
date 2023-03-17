import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const o_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export class my_asset_value implements att.ArchetypeType {
    constructor(public value: att.Int, public myaggregate: Array<att.Nat>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.value.to_mich(), att.list_to_mich(this.myaggregate, x => {
                return x.to_mich();
            })]);
    }
    equals(v: my_asset_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_asset_value {
        return new my_asset_value(att.Int.from_mich((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }));
    }
}
export const my_asset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%value"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%myaggregate"])
], []);
export type o_asset_container = Array<att.Nat>;
export type my_asset_container = Array<[
    string,
    my_asset_value
]>;
export const o_asset_container_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), []);
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%value"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%myaggregate"])
], []), []);
export class Test_initialized_with_asset {
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
        const address = (await ex.deploy("../tests/passed/test_initialized_with_asset.arl", {}, params)).address;
        this.address = address;
    }
    async get_o_asset(): Promise<o_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[0], x => { return att.Nat.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[1], (x, y) => [att.mich_to_string(x), my_asset_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_initialized_with_asset = new Test_initialized_with_asset();
