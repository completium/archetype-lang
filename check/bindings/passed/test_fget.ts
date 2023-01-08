import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const mile_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const owner_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export class mile_value implements att.ArchetypeType {
    constructor(public expiration: Date, public quantity: att.Int) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.date_to_mich(this.expiration), this.quantity.to_mich()]);
    }
    equals(v: mile_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): mile_value {
        return new mile_value(att.mich_to_date((input as att.Mpair).args[0]), att.Int.from_mich((input as att.Mpair).args[1]));
    }
}
export const mile_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%expiration"]),
    att.prim_annot_to_mich_type("int", ["%quantity"])
], []);
export const owner_value_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []);
export type mile_container = Array<[
    string,
    mile_value
]>;
export type owner_container = Array<[
    att.Address,
    Array<string>
]>;
export const mile_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%expiration"]),
    att.prim_annot_to_mich_type("int", ["%quantity"])
], []), []);
export const owner_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), []);
const exec_arg_to_mich = (ow: att.Address): att.Micheline => {
    return ow.to_mich();
}
export class Test_fget {
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
        const address = (await ex.deploy("../tests/passed/test_fget.arl", {}, params)).address;
        this.address = address;
    }
    async exec(ow: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(ow), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(ow: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(ow), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_mile(): Promise<mile_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.mich_to_string(x), mile_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_owner(): Promise<owner_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[1], (x, y) => [att.Address.from_mich(x), att.mich_to_list(y, x => { return att.mich_to_string(x); })]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        KO: att.string_to_mich("\"ko\"")
    };
}
export const test_fget = new Test_fget();
