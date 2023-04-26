import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const view_my_view_arg_to_mich = (s: string): att.Micheline => {
    return att.string_to_mich(s);
}
export class Import_arl_view_def {
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
        const address = (await ex.deploy("../tests/passed/import_arl_view_def.arl", {}, params)).address;
        this.address = address;
    }
    async view_my_view(s: string, params: Partial<ex.Parameters>): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "my_view", view_my_view_arg_to_mich(s), params);
            return mich.value ? att.Nat.from_mich(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const import_arl_view_def = new Import_arl_view_def();
