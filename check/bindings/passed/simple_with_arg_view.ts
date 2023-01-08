import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const assign_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const getValue_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export const deploy_getValue_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("getValue", att.prim_annot_to_mich_type("nat", []), params);
};
export class Simple_with_arg_view {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    getValue_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/simple_with_arg_view.arl", {}, params)).address;
        this.address = address;
        this.getValue_callback_address = (await deploy_getValue_callback(params)).address;
    }
    async assign(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "assign", assign_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_assign_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "assign", assign_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async getValue(params: Partial<ex.Parameters>): Promise<att.Nat> {
        if (this.address != undefined) {
            if (this.getValue_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.getValue_callback_address), "callback");
                await ex.call(this.address, "getValue", att.getter_args_to_mich(getValue_arg_to_mich(), entrypoint), params);
                return await ex.get_callback_value<att.Nat>(this.getValue_callback_address, x => { return att.Nat.from_mich(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_n(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const simple_with_arg_view = new Simple_with_arg_view();
