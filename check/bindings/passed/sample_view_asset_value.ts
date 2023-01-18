import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export type my_asset_container = Array<[
    string,
    att.Nat
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("nat", []), []);
const view_getPrice_arg_to_mich = (c: string): att.Micheline => {
    return att.string_to_mich(c);
}
export class Sample_view_asset_value {
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
        const address = (await ex.deploy("../tests/passed/sample_view_asset_value.arl", {}, params)).address;
        this.address = address;
    }
    async view_getPrice(c: string, params: Partial<ex.Parameters>): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "getPrice", view_getPrice_arg_to_mich(c), params);
            return mich.value ? att.Nat.from_mich(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset_value(key: string): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), att.string_to_mich(key), my_asset_key_mich_type);
            if (data != undefined) {
                return att.Nat.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_my_asset_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), att.string_to_mich(key), my_asset_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        ERROR: att.string_to_mich("\"ERROR\"")
    };
}
export const sample_view_asset_value = new Sample_view_asset_value();
