import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public a: string, public b: att.Nat, public c: att.Bytes, public d: att.Int, public e: string, public f: att.Nat, public g: att.Bytes, public h: att.Int) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.a), this.b.to_mich(), this.c.to_mich(), this.d.to_mich(), att.string_to_mich(this.e), this.f.to_mich(), this.g.to_mich(), this.h.to_mich()]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.mich_to_string((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.Bytes.from_mich((input as att.Mpair).args[2]), att.Int.from_mich((input as att.Mpair).args[3]), att.mich_to_string((input as att.Mpair).args[4]), att.Nat.from_mich((input as att.Mpair).args[5]), att.Bytes.from_mich((input as att.Mpair).args[6]), att.Int.from_mich((input as att.Mpair).args[7]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%a"]),
    att.prim_annot_to_mich_type("nat", ["%b"]),
    att.prim_annot_to_mich_type("bytes", ["%c"]),
    att.prim_annot_to_mich_type("int", ["%d"]),
    att.prim_annot_to_mich_type("string", ["%e"]),
    att.prim_annot_to_mich_type("nat", ["%f"]),
    att.prim_annot_to_mich_type("bytes", ["%g"]),
    att.prim_annot_to_mich_type("int", ["%h"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Record_access {
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
        const address = (await ex.deploy("../tests/passed/record_access.arl", {}, params)).address;
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
    async get_r(): Promise<my_record> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const record_access = new Record_access();
