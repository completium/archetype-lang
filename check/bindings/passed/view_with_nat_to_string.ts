import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const view_my_view_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
export class View_with_nat_to_string {
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
        const address = (await ex.deploy("../tests/passed/view_with_nat_to_string.arl", {}, params)).address;
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
    async view_my_view(n: att.Nat, params: Partial<ex.Parameters>): Promise<string | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "my_view", view_my_view_arg_to_mich(n), params);
            return mich.value ? att.mich_to_string(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Option<string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich(storage, x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const view_with_nat_to_string = new View_with_nat_to_string();
