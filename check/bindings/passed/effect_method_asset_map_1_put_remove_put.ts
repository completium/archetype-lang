import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export type my_asset_container = Array<[
    att.Nat,
    string
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("string", []), []);
const exec_arg_to_mich = (av: string): att.Micheline => {
    return att.string_to_mich(av);
}
export class Effect_method_asset_map_1_put_remove_put {
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
        const address = (await ex.deploy("../tests/passed/effect_method_asset_map_1_put_remove_put.arl", {}, params)).address;
        this.address = address;
    }
    async exec(av: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(av), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(av: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(av), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Nat.from_mich(x), att.mich_to_string(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const effect_method_asset_map_1_put_remove_put = new Effect_method_asset_map_1_put_remove_put();
