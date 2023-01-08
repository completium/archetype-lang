import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_arg implements att.ArchetypeType {
    constructor(public a: att.Int, public b: att.Nat, public c: att.Address) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.pair_to_mich([this.a.to_mich(), this.to_mich()]), this.b.to_mich()]);
    }
    equals(v: my_arg): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_arg {
        return new my_arg(att.Int.from_mich(input), att.Nat.from_mich(input), att.Address.from_mich(input));
    }
}
export const my_arg_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%x"]),
        att.prim_annot_to_mich_type("nat", [])
    ], []),
    att.prim_annot_to_mich_type("address", ["%z"])
], []);
const exec_arg_to_mich = (p: my_arg): att.Micheline => {
    return p.to_mich();
}
export class Custom_args_with_record {
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
        const address = (await ex.deploy("../tests/passed/custom_args_with_record.arl", {}, params)).address;
        this.address = address;
    }
    async exec(p: my_arg, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(p: my_arg, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const custom_args_with_record = new Custom_args_with_record();
