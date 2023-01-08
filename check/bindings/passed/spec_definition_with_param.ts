import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const myasset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("int", []);
export const myasset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export type myasset_container = Array<[
    att.Int,
    att.Nat
]>;
export const myasset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), []);
const exec_arg_to_mich = (p: att.Nat): att.Micheline => {
    return p.to_mich();
}
export class Spec_definition_with_param {
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
        const address = (await ex.deploy("../tests/passed/spec_definition_with_param.arl", {}, params)).address;
        this.address = address;
    }
    async exec(p: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(p: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_myasset(): Promise<myasset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Int.from_mich(x), att.Nat.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const spec_definition_with_param = new Spec_definition_with_param();
