import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const abc_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export class abc_value implements att.ArchetypeType {
    constructor(public b: string, public c: att.Bytes) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.b), this.c.to_mich()]);
    }
    equals(v: abc_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): abc_value {
        return new abc_value(att.mich_to_string((input as att.Mpair).args[0]), att.Bytes.from_mich((input as att.Mpair).args[1]));
    }
}
export const abc_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%b"]),
    att.prim_annot_to_mich_type("bytes", ["%c"])
], []);
export type abc_container = Array<[
    att.Nat,
    abc_value
]>;
export const abc_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%b"]),
    att.prim_annot_to_mich_type("bytes", ["%c"])
], []), []);
const get_value_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Asset_types_get {
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
        const address = (await ex.deploy("../tests/passed/asset_types_get.arl", {}, params)).address;
        this.address = address;
    }
    async get_value(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "get_value", get_value_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_get_value_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "get_value", get_value_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_abc(): Promise<abc_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Nat.from_mich(x), abc_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_VALUE: att.string_to_mich("\"INVALID_VALUE\"")
    };
}
export const asset_types_get = new Asset_types_get();
