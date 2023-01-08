import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const set_arg_to_mich = (i: att.Nat, s: string): att.Micheline => {
    return att.pair_to_mich([
        i.to_mich(),
        att.string_to_mich(s)
    ]);
}
export class View_offchain_nat {
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
        const address = (await ex.deploy("../tests/passed/view_offchain_nat.arl", {}, params)).address;
        this.address = address;
    }
    async set(i: att.Nat, s: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set", set_arg_to_mich(i, s), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_param(i: att.Nat, s: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set", set_arg_to_mich(i, s), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_bm_value(key: att.Nat): Promise<string | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[0]).toString()), key.to_mich(), att.prim_annot_to_mich_type("nat", []));
            if (data != undefined) {
                return att.mich_to_string(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_bm_value(key: att.Nat): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[0]).toString()), key.to_mich(), att.prim_annot_to_mich_type("nat", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_metadata_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return att.Bytes.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_metadata_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const view_offchain_nat = new View_offchain_nat();
