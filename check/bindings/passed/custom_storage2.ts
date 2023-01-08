import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public a: att.Int, public b: att.Nat, public c: att.Address, public d: att.Int, public e: att.Nat, public f: att.Address, public g: att.Int, public h: att.Nat, public i: att.Address, public j: att.Int, public k: att.Nat, public l: att.Address) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.pair_to_mich([this.a.to_mich(), this.b.to_mich()]), att.pair_to_mich([att.pair_to_mich([this.c.to_mich(), this.d.to_mich()]), att.pair_to_mich([this.e.to_mich(), this.f.to_mich(), this.g.to_mich(), this.h.to_mich()]), this.i.to_mich()]), att.pair_to_mich([this.j.to_mich(), this.k.to_mich()]), this.l.to_mich()]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.Int.from_mich(((input as att.Mpair).args[0] as att.Mpair).args[0]), att.Nat.from_mich(((input as att.Mpair).args[0] as att.Mpair).args[1]), att.Address.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[0] as att.Mpair).args[0]), att.Int.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[0] as att.Mpair).args[1]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[0]), att.Address.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[1]), att.Int.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[2]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[3]), att.Address.from_mich(((input as att.Mpair).args[1] as att.Mpair).args[2]), att.Int.from_mich(((input as att.Mpair).args[2] as att.Mpair).args[0]), att.Nat.from_mich(((input as att.Mpair).args[2] as att.Mpair).args[1]), att.Address.from_mich((input as att.Mpair).args[3]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%m"]),
        att.prim_annot_to_mich_type("nat", ["%n"])
    ], []),
    att.pair_array_to_mich_type([
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("address", ["%p"]),
            att.prim_annot_to_mich_type("int", ["%q"])
        ], []),
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("nat", ["%r"]),
            att.prim_annot_to_mich_type("address", ["%s"]),
            att.prim_annot_to_mich_type("int", ["%t"]),
            att.prim_annot_to_mich_type("nat", ["%u"])
        ], []),
        att.prim_annot_to_mich_type("address", ["%v"])
    ], []),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%x"]),
        att.prim_annot_to_mich_type("nat", ["%y"])
    ], []),
    att.prim_annot_to_mich_type("address", ["%z"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Custom_storage2 {
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
        const address = (await ex.deploy("../tests/passed/custom_storage2.arl", {}, params)).address;
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
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_r(): Promise<my_record> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return my_record.from_mich(att.pair_to_mich((storage as att.Mpair as att.Mpair).args.slice(1, 5)));
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const custom_storage2 = new Custom_storage2();
