import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const balanceInquiry_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Entry_inspector {
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
        const address = (await ex.deploy("../tests/passed/entry_inspector.arl", {}, params)).address;
        this.address = address;
    }
    async balanceInquiry(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "balanceInquiry", balanceInquiry_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_balanceInquiry_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "balanceInquiry", balanceInquiry_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_total(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_token(): Promise<att.Address> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Address.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        NOT_FOUND: att.string_to_mich("\"NOT_FOUND\"")
    };
}
export const entry_inspector = new Entry_inspector();
