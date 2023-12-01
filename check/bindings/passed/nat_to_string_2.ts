import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const callback_arg_to_mich = (babal: att.Nat): att.Micheline => {
    return babal.to_mich();
}
export class Nat_to_string_2 {
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
        const address = (await ex.deploy("../tests/passed/nat_to_string_2.arl", {}, params)).address;
        this.address = address;
    }
    async callback(babal: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(babal), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(babal: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(babal), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_s1(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_s2(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_bal(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const nat_to_string_2 = new Nat_to_string_2();
