import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public id: string, public v: att.Nat, public a: att.Int) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.id), this.v.to_mich(), this.a.to_mich()]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.mich_to_string((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.Int.from_mich((input as att.Mpair).args[2]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%id"]),
    att.prim_annot_to_mich_type("nat", ["%v"]),
    att.prim_annot_to_mich_type("int", ["%a"])
], []);
const exec_arg_to_mich = (i: att.Int): att.Micheline => {
    return i.to_mich();
}
export class Test_record_assign_4 {
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
        const address = (await ex.deploy("../tests/passed/test_record_assign_4.arl", {}, params)).address;
        this.address = address;
    }
    async exec(i: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(i: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(i), params);
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
export const test_record_assign_4 = new Test_record_assign_4();
