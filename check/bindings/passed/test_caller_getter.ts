import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const callback_arg_to_mich = (k: att.Nat): att.Micheline => {
    return k.to_mich();
}
const main_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Test_caller_getter {
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
        const address = (await ex.deploy("../tests/passed/test_caller_getter.arl", {}, params)).address;
        this.address = address;
    }
    async callback(k: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async main(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "main", main_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(k: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_main_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "main", main_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_n(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_ENTRY: att.string_to_mich("\"INVALID_ENTRY\"")
    };
}
export const test_caller_getter = new Test_caller_getter();
