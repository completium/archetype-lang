import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public x: boolean, public y: Array<att.Nat>, public z: Array<att.Nat>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.bool_to_mich(this.x), att.list_to_mich(this.y, x => {
                return x.to_mich();
            }), att.list_to_mich(this.z, x => {
                return x.to_mich();
            })]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.mich_to_bool((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }), att.mich_to_list((input as att.Mpair).args[2], x => { return att.Nat.from_mich(x); }));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bool", ["%x"]),
    att.list_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%y"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%z"])
], []);
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export class my_asset_value implements att.ArchetypeType {
    constructor(public b: boolean, public c: Array<att.Nat>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.bool_to_mich(this.b), att.list_to_mich(this.c, x => {
                return x.to_mich();
            })]);
    }
    equals(v: my_asset_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_asset_value {
        return new my_asset_value(att.mich_to_bool((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }));
    }
}
export const my_asset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bool", ["%b"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%c"])
], []);
export type my_asset_container = Array<[
    att.Nat,
    my_asset_value
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bool", ["%b"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%c"])
], []), []);
const updateTransferlist_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Add_update_record {
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
        const address = (await ex.deploy("../tests/passed/add_update_record.arl", {}, params)).address;
        this.address = address;
    }
    async updateTransferlist(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "updateTransferlist", updateTransferlist_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_updateTransferlist_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "updateTransferlist", updateTransferlist_arg_to_mich(), params);
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
export const add_update_record = new Add_update_record();
