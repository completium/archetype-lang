import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const get_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export const deploy_get_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("get", att.prim_annot_to_mich_type("nat", []), params);
};
export class Getter_called_by {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    get_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/getter_called_by.arl", {}, params)).address;
        this.address = address;
        this.get_callback_address = (await deploy_get_callback(params)).address;
    }
    async get(params: Partial<ex.Parameters>): Promise<att.Nat> {
        if (this.address != undefined) {
            if (this.get_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.get_callback_address), "callback");
                await ex.call(this.address, "get", att.getter_args_to_mich(get_arg_to_mich(), entrypoint), params);
                return await ex.get_callback_value<att.Nat>(this.get_callback_address, x => { return att.Nat.from_mich(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\"")
    };
}
export const getter_called_by = new Getter_called_by();
