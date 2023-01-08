import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public v0: string, public v1: att.Nat, public v2: att.Int, public v3: boolean) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.v0), this.v1.to_mich(), this.v2.to_mich(), att.bool_to_mich(this.v3)]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.mich_to_string((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.Int.from_mich((input as att.Mpair).args[2]), att.mich_to_bool((input as att.Mpair).args[3]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%v0"]),
    att.prim_annot_to_mich_type("nat", ["%v1"]),
    att.prim_annot_to_mich_type("int", ["%v2"]),
    att.prim_annot_to_mich_type("bool", ["%v3"])
], []);
const setv0_arg_to_mich = (v: string): att.Micheline => {
    return att.string_to_mich(v);
}
const setv1_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const setv2_arg_to_mich = (v: att.Int): att.Micheline => {
    return v.to_mich();
}
const setv3_arg_to_mich = (v: boolean): att.Micheline => {
    return att.bool_to_mich(v);
}
export class Test_record_assign_full {
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
        const address = (await ex.deploy("../tests/passed/test_record_assign_full.arl", {}, params)).address;
        this.address = address;
    }
    async setv0(v: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setv0", setv0_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async setv1(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setv1", setv1_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async setv2(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setv2", setv2_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async setv3(v: boolean, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setv3", setv3_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setv0_param(v: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setv0", setv0_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setv1_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setv1", setv1_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setv2_param(v: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setv2", setv2_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setv3_param(v: boolean, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setv3", setv3_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<my_record> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_record_assign_full = new Test_record_assign_full();
