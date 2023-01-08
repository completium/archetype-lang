import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum states {
    One = 1,
    Two
}
export const mich_to_state = (m: any): states => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return states.One;
        case 1: return states.Two;
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export type my_asset_container = Array<[
    att.Nat,
    string
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("string", []), []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const view_getN_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const view_getMyasset0_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const view_getMyasset_arg_to_mich = (i: att.Nat): att.Micheline => {
    return i.to_mich();
}
export class Test_view {
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
        const address = (await ex.deploy("../tests/passed/test_view.arl", {}, params)).address;
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
    async view_getN(params: Partial<ex.Parameters>): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "getN", view_getN_arg_to_mich(), params);
            return mich.value ? att.Nat.from_mich(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async view_getMyasset0(params: Partial<ex.Parameters>): Promise<string | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "getMyasset0", view_getMyasset0_arg_to_mich(), params);
            return mich.value ? att.mich_to_string(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async view_getMyasset(i: att.Nat, params: Partial<ex.Parameters>): Promise<string | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "getMyasset", view_getMyasset_arg_to_mich(i), params);
            return mich.value ? att.mich_to_string(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async get_my_asset(): Promise<my_asset_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.Nat.from_mich(x), att.mich_to_string(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_n(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_r(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_state(): Promise<states> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const state = (storage as att.Mpair).args[3];
            switch (att.Int.from_mich(state).to_number()) {
                case 0: return states.One;
                case 1: return states.Two;
            }
        }
        return states.One;
    }
    errors = {
        NOT_FOUND: att.string_to_mich("\"not_found\"")
    };
}
export const test_view = new Test_view();
