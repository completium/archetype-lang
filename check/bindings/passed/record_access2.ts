import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record2 implements att.ArchetypeType {
    constructor(public x: string, public y: att.Nat, public z: att.Bytes) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.x), this.y.to_mich(), this.z.to_mich()]);
    }
    equals(v: my_record2): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record2 {
        return new my_record2(att.mich_to_string((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.Bytes.from_mich((input as att.Mpair).args[2]));
    }
}
export class my_record3 implements att.ArchetypeType {
    constructor(public alpha: string, public beta: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.alpha), this.beta.to_mich()]);
    }
    equals(v: my_record3): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record3 {
        return new my_record3(att.mich_to_string((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export class my_record implements att.ArchetypeType {
    constructor(public a: my_record3, public b: my_record3, public c: att.Bytes, public d: att.Int, public e: string, public f: my_record2, public g: att.Bytes, public h: my_record3) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.a.to_mich(), this.b.to_mich(), this.c.to_mich(), this.d.to_mich(), att.string_to_mich(this.e), this.f.to_mich(), this.g.to_mich(), this.h.to_mich()]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(my_record3.from_mich((input as att.Mpair).args[0]), my_record3.from_mich((input as att.Mpair).args[1]), att.Bytes.from_mich((input as att.Mpair).args[2]), att.Int.from_mich((input as att.Mpair).args[3]), att.mich_to_string((input as att.Mpair).args[4]), my_record2.from_mich((input as att.Mpair).args[5]), att.Bytes.from_mich((input as att.Mpair).args[6]), my_record3.from_mich(att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(7, 9))));
    }
}
export const my_record2_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%x"]),
    att.prim_annot_to_mich_type("nat", ["%y"]),
    att.prim_annot_to_mich_type("bytes", ["%z"])
], []);
export const my_record3_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%alpha"]),
    att.prim_annot_to_mich_type("nat", ["%beta"])
], []);
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", ["%alpha"]),
        att.prim_annot_to_mich_type("nat", ["%beta"])
    ], ["%a"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", ["%alpha"]),
        att.prim_annot_to_mich_type("nat", ["%beta"])
    ], ["%b"]),
    att.prim_annot_to_mich_type("bytes", ["%c"]),
    att.prim_annot_to_mich_type("int", ["%d"]),
    att.prim_annot_to_mich_type("string", ["%e"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", ["%x"]),
        att.prim_annot_to_mich_type("nat", ["%y"]),
        att.prim_annot_to_mich_type("bytes", ["%z"])
    ], ["%f"]),
    att.prim_annot_to_mich_type("bytes", ["%g"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", ["%alpha"]),
        att.prim_annot_to_mich_type("nat", ["%beta"])
    ], ["%h"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Record_access2 {
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
        const address = (await ex.deploy("../tests/passed/record_access2.arl", {}, params)).address;
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
    async get_res_a(): Promise<my_record3> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record3.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_b(): Promise<my_record3> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record3.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_c(): Promise<att.Bytes> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bytes.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_d(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_e(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[4]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_f(): Promise<my_record2> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record2.from_mich((storage as att.Mpair).args[5]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_g(): Promise<att.Bytes> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bytes.from_mich((storage as att.Mpair).args[6]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_h(): Promise<my_record3> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record3.from_mich((storage as att.Mpair).args[7]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_a_alpha(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[8]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_a_beta(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[9]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_b_alpha(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[10]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_b_beta(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[11]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_f_x(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[12]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_f_y(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[13]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_f_z(): Promise<att.Bytes> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bytes.from_mich((storage as att.Mpair).args[14]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_h_alpha(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[15]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_h_beta(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[16]);
        }
        throw new Error("Contract not initialised");
    }
    async get_r(): Promise<my_record> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record.from_mich(att.pair_to_mich((storage as att.Mpair as att.Mpair).args.slice(17, 26)));
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const record_access2 = new Record_access2();
