import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const updatableMap_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export class updatableMap_value implements att.ArchetypeType {
    constructor(public value1: att.Nat, public value2: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.value1.to_mich(), att.string_to_mich(this.value2)]);
    }
    equals(v: updatableMap_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): updatableMap_value {
        return new updatableMap_value(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_string((input as att.Mpair).args[1]));
    }
}
export const updatableMap_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%value1"]),
    att.prim_annot_to_mich_type("string", ["%value2"])
], []);
export type updatableMap_container = Array<[
    att.Nat,
    updatableMap_value
]>;
export const updatableMap_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%value1"]),
    att.prim_annot_to_mich_type("string", ["%value2"])
], []), []);
const update_arg_to_mich = (idToChange: att.Nat, newValue1: att.Nat, newValue2: string): att.Micheline => {
    return att.pair_to_mich([
        idToChange.to_mich(),
        newValue1.to_mich(),
        att.string_to_mich(newValue2)
    ]);
}
export class Simple_add_update_asset {
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
        const address = (await ex.deploy("../tests/passed/simple_add_update_asset.arl", {}, params)).address;
        this.address = address;
    }
    async update(idToChange: att.Nat, newValue1: att.Nat, newValue2: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update", update_arg_to_mich(idToChange, newValue1, newValue2), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_param(idToChange: att.Nat, newValue1: att.Nat, newValue2: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update", update_arg_to_mich(idToChange, newValue1, newValue2), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_updatableMap(): Promise<updatableMap_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Nat.from_mich(x), updatableMap_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const simple_add_update_asset = new Simple_add_update_asset();
