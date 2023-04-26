import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const set_res_imported_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
const set_res_top_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
const callback_arg_to_mich = (n: att.Nat, s: string, e: att.Entrypoint): att.Micheline => {
    return att.pair_to_mich([
        n.to_mich(),
        att.string_to_mich(s),
        e.to_mich()
    ]);
}
const exec_arg_to_mich = (a: att.Address): att.Micheline => {
    return a.to_mich();
}
export class Import_arl_entrypoint_use_collide {
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
        const address = (await ex.deploy("../tests/passed/import_arl_entrypoint_use_collide.arl", {}, params)).address;
        this.address = address;
    }
    async set_res_imported(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_res_imported", set_res_imported_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_res_top(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_res_top", set_res_top_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async callback(n: att.Nat, s: string, e: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(n, s, e), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_res_imported_param(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_res_imported", set_res_imported_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_res_top_param(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_res_top", set_res_top_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(n: att.Nat, s: string, e: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(n, s, e), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_imported(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res_top(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const import_arl_entrypoint_use_collide = new Import_arl_entrypoint_use_collide();
