import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export class my_asset_value implements att.ArchetypeType {
    constructor(public r1: att.Rational, public r2: att.Rational, public r3: att.Rational, public r4: att.Rational) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.r1.to_mich(), this.r2.to_mich(), this.r3.to_mich(), this.r4.to_mich()]);
    }
    equals(v: my_asset_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_asset_value {
        return new my_asset_value(att.Rational.from_mich((input as att.Mpair).args[0]), att.Rational.from_mich((input as att.Mpair).args[1]), att.Rational.from_mich((input as att.Mpair).args[2]), att.Rational.from_mich(att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(3, 5))));
    }
}
export const my_asset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r1"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r2"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r3"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r4"])
], []);
export type my_asset_container = Array<[
    att.Nat,
    my_asset_value
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r1"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r2"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r3"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%r4"])
], []), []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Op_assign_rat_update_asset {
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
        const address = (await ex.deploy("../tests/passed/op_assign_rat_update_asset.arl", {}, params)).address;
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
            return att.mich_to_map(storage, (x, y) => [att.Nat.from_mich(x), my_asset_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const op_assign_rat_update_asset = new Op_assign_rat_update_asset();
