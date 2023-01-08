import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class rt implements att.ArchetypeType {
    constructor(public s: Array<att.Nat>, public l: Array<att.Nat>, public m: Array<[
        att.Nat,
        att.Nat
    ]>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.list_to_mich(this.s, x => {
                return x.to_mich();
            }), att.list_to_mich(this.l, x => {
                return x.to_mich();
            }), att.list_to_mich(this.m, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), x_value.to_mich());
            })]);
    }
    equals(v: rt): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): rt {
        return new rt(att.mich_to_list((input as att.Mpair).args[0], x => { return att.Nat.from_mich(x); }), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }), att.mich_to_map((input as att.Mpair).args[2], (x, y) => [att.Nat.from_mich(x), att.Nat.from_mich(y)]));
    }
}
export const rt_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%s"]),
    att.list_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%l"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("nat", []), ["%m"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Variable_in_container {
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
        const address = (await ex.deploy("../tests/passed/variable_in_container.arl", {}, params)).address;
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
    async get_v(): Promise<rt> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return rt.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const variable_in_container = new Variable_in_container();
