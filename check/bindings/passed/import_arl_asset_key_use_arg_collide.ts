import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const import_arl_asset_def__my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const import_arl_asset_def__my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export type import_arl_asset_def__my_asset_container = Array<[
    att.Nat,
    string
]>;
export const import_arl_asset_def__my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("string", []), []);
export class Import_arl_asset_key_use_arg_collide {
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
        const address = (await ex.deploy("../tests/passed/import_arl_asset_key_use_arg_collide.arl", {}, params)).address;
        this.address = address;
    }
    errors = {};
}
export const import_arl_asset_key_use_arg_collide = new Import_arl_asset_key_use_arg_collide();
