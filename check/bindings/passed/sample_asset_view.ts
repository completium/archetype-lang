import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const my_asset_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const my_asset_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export type my_asset_container = Array<[
    att.Nat,
    string
]>;
export const my_asset_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("string", []), []);
const exec_arg_to_mich = (i: Array<att.Nat>): att.Micheline => {
    return att.list_to_mich(i, x => {
        return x.to_mich();
    });
}
const my_getter_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const view_my_view_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export const deploy_my_getter_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("my_getter", att.list_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), []), params);
};
export class Sample_asset_view {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    my_getter_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/sample_asset_view.arl", {}, params)).address;
        this.address = address;
        this.my_getter_callback_address = (await deploy_my_getter_callback(params)).address;
    }
    async exec(i: Array<att.Nat>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(i: Array<att.Nat>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async my_getter(params: Partial<ex.Parameters>): Promise<Array<att.Nat>> {
        if (this.address != undefined) {
            if (this.my_getter_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.my_getter_callback_address), "callback");
                await ex.call(this.address, "my_getter", att.getter_args_to_mich(my_getter_arg_to_mich(), entrypoint), params);
                return await ex.get_callback_value<Array<att.Nat>>(this.my_getter_callback_address, x => { return att.mich_to_list(x, x => { return att.Nat.from_mich(x); }); });
            }
        }
        throw new Error("Contract not initialised");
    }
    async view_my_view(params: Partial<ex.Parameters>): Promise<Array<att.Nat> | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "my_view", view_my_view_arg_to_mich(), params);
            return mich.value ? att.mich_to_list(mich.value, x => { return att.Nat.from_mich(x); }) : undefined;
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
    async get_res(): Promise<Array<att.Nat>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[1], x => { return att.Nat.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const sample_asset_view = new Sample_asset_view();
