import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const default_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class List_list {
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
        const address = (await ex.deploy("../tests/passed/list_list.arl", {}, params)).address;
        this.address = address;
    }
    async default(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "default", default_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_default_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "default", default_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_l(): Promise<Array<Array<att.Nat>>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list(storage, x => { return att.mich_to_list(x, x => { return att.Nat.from_mich(x); }); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const list_list = new List_list();
