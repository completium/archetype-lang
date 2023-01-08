import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum my_enum_types {
    First = "First",
    Second = "Second",
    Third = "Third"
}
export abstract class my_enum extends att.Enum<my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class First extends my_enum {
    constructor() {
        super(my_enum_types.First);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Second extends my_enum {
    constructor() {
        super(my_enum_types.Second);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Third extends my_enum {
    constructor() {
        super(my_enum_types.Third);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_my_enum = (m: any): my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new First();
        case 1: return new Second();
        case 2: return new Third();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const my_asset_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%val"]),
    att.prim_annot_to_mich_type("int", ["%state_my_asset"])
], []);
export type my_asset_container = Array<[
    string,
    att.Int
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%val"]),
    att.prim_annot_to_mich_type("int", ["%state_my_asset"])
], []), []);
const exec_arg_to_mich = (v: att.Int, a: string): att.Micheline => {
    return att.pair_to_mich([
        v.to_mich(),
        att.string_to_mich(a)
    ]);
}
export class Contract_transition_on_asset {
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
        const address = (await ex.deploy("../tests/passed/contract_transition_on_asset.arl", {}, params)).address;
        this.address = address;
    }
    async exec(v: att.Int, a: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(v, a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(v: att.Int, a: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(v, a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.mich_to_string(x), att.Int.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_STATE: att.string_to_mich("\"INVALID_STATE\"")
    };
}
export const contract_transition_on_asset = new Contract_transition_on_asset();
