import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const o_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const o_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const my_asset_value_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []);
export type o_asset_container = Array<[
    string,
    att.Nat
]>;
export type my_asset_container = Array<[
    string,
    Array<string>
]>;
export const o_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("nat", []), []);
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("string", []), att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), []);
const init_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Select_partition_to_big_map {
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
        const address = (await ex.deploy("../tests/passed/select_partition_to_big_map.arl", {}, params)).address;
        this.address = address;
    }
    async init(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "init", init_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_init_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "init", init_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_o_asset_value(key: string): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[0]).toString()), att.string_to_mich(key), o_asset_key_mich_type);
            if (data != undefined) {
                return att.Nat.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_o_asset_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[0]).toString()), att.string_to_mich(key), o_asset_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset_value(key: string): Promise<Array<string> | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), my_asset_key_mich_type);
            if (data != undefined) {
                return att.mich_to_list(data, x => { return att.mich_to_string(x); });
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), my_asset_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<Array<string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[2], x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const select_partition_to_big_map = new Select_partition_to_big_map();
