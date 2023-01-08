import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public m: Array<[
        att.Nat,
        string
    ]>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.list_to_mich(this.m, x => {
            const x_key = x[0];
            const x_value = x[1];
            return att.elt_to_mich(x_key.to_mich(), att.string_to_mich(x_value));
        });
    }
    equals(v: my_record): boolean {
        return JSON.stringify(this.m) == JSON.stringify(v.m);
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.mich_to_map(input, (x, y) => [att.Nat.from_mich(x), att.mich_to_string(y)]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("string", []), []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Expr_instr_rec_1_0 {
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
        const address = (await ex.deploy("../tests/passed/expr_instr_rec_1_0.arl", {}, params)).address;
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
export const expr_instr_rec_1_0 = new Expr_instr_rec_1_0();
