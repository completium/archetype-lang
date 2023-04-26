import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const callback_arg_to_mich = (n: att.Nat, e: att.Entrypoint): att.Micheline => {
    return att.pair_to_mich([
        n.to_mich(),
        e.to_mich()
    ]);
}
export class Import_arl_entrypoint_def {
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
        const address = (await ex.deploy("../tests/passed/import_arl_entrypoint_def.arl", {}, params)).address;
        this.address = address;
    }
    async callback(n: att.Nat, e: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(n, e), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(n: att.Nat, e: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(n, e), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const import_arl_entrypoint_def = new Import_arl_entrypoint_def();
