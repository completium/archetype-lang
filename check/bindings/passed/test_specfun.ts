import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const anentry_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
const agetter_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export const deploy_agetter_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("agetter", att.prim_annot_to_mich_type("nat", []), params);
};
export class Test_specfun {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    agetter_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/test_specfun.arl", {}, params)).address;
        this.address = address;
        this.agetter_callback_address = (await deploy_agetter_callback(params)).address;
    }
    async anentry(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "anentry", anentry_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_anentry_param(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "anentry", anentry_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async agetter(params: Partial<ex.Parameters>): Promise<att.Nat> {
        if (this.address != undefined) {
            if (this.agetter_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.agetter_callback_address), "callback");
                await ex.call(this.address, "agetter", att.getter_args_to_mich(agetter_arg_to_mich(), entrypoint), params);
                return await ex.get_callback_value<att.Nat>(this.agetter_callback_address, x => { return att.Nat.from_mich(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_specfun = new Test_specfun();
