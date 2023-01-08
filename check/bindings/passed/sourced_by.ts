import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export type my_asset_container = Array<[
    att.Address,
    string
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.prim_annot_to_mich_type("string", []), []);
const e0_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e1_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e2_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Sourced_by {
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
        const address = (await ex.deploy("../tests/passed/sourced_by.arl", {}, params)).address;
        this.address = address;
    }
    async e0(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e0", e0_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e1(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e1", e1_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e2(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e2", e2_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e0_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e0", e0_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e1_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e1", e1_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e2_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e2", e2_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map(storage, (x, y) => [att.Address.from_mich(x), att.mich_to_string(y)]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_SOURCE: att.string_to_mich("\"INVALID_SOURCE\"")
    };
}
export const sourced_by = new Sourced_by();
