import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const myasset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const myasset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export type myasset_container = Array<[
    string,
    att.Nat
]>;
export const myasset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("nat", []), []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Get_in_require_failif {
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
        const address = (await ex.deploy("../tests/passed/get_in_require_failif.arl", {}, params)).address;
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
    async get_myasset(): Promise<myasset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.mich_to_string(x), att.Nat.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        f1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"f1\"")]),
        r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r1\"")])
    };
}
export const get_in_require_failif = new Get_in_require_failif();
