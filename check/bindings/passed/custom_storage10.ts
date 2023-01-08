import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public fa: att.Nat, public fb: att.Nat, public fc: att.Nat, public fd: att.Nat, public fe: att.Nat, public ff: att.Nat, public fg: att.Nat, public fh: att.Nat, public fi: att.Nat, public fj: att.Nat, public fk: att.Nat, public fl: att.Nat, public fm: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.pair_to_mich([this.fa.to_mich(), this.fb.to_mich()]), att.pair_to_mich([att.pair_to_mich([this.fc.to_mich(), this.fd.to_mich()]), att.pair_to_mich([this.fe.to_mich(), this.ff.to_mich(), this.fg.to_mich(), this.fh.to_mich()]), this.fi.to_mich()]), this.fj.to_mich(), att.pair_to_mich([this.fk.to_mich(), this.fl.to_mich()]), this.fm.to_mich()]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.Nat.from_mich(((input as att.Mpair).args[0] as att.Mpair).args[0]), att.Nat.from_mich(((input as att.Mpair).args[0] as att.Mpair).args[1]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[0] as att.Mpair).args[0]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[0] as att.Mpair).args[1]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[0]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[1]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[2]), att.Nat.from_mich((((input as att.Mpair).args[1] as att.Mpair).args[1] as att.Mpair).args[3]), att.Nat.from_mich(((input as att.Mpair).args[1] as att.Mpair).args[2]), att.Nat.from_mich((input as att.Mpair).args[2]), att.Nat.from_mich(((input as att.Mpair).args[3] as att.Mpair).args[0]), att.Nat.from_mich(((input as att.Mpair).args[3] as att.Mpair).args[1]), att.Nat.from_mich((input as att.Mpair).args[4]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%a"]),
        att.prim_annot_to_mich_type("nat", ["%b"])
    ], []),
    att.pair_array_to_mich_type([
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("nat", ["%c"]),
            att.prim_annot_to_mich_type("nat", ["%d"])
        ], []),
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("nat", ["%e"]),
            att.prim_annot_to_mich_type("nat", ["%f"]),
            att.prim_annot_to_mich_type("nat", ["%g"]),
            att.prim_annot_to_mich_type("nat", ["%h"])
        ], []),
        att.prim_annot_to_mich_type("nat", ["%i"])
    ], []),
    att.prim_annot_to_mich_type("nat", ["%j"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%k"]),
        att.prim_annot_to_mich_type("nat", ["%l"])
    ], []),
    att.prim_annot_to_mich_type("nat", ["%m"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Custom_storage10 {
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
        const address = (await ex.deploy("../tests/passed/custom_storage10.arl", {}, params)).address;
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
            return my_record.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const custom_storage10 = new Custom_storage10();
