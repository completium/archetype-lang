import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const update_value_arg_to_mich = (p: [
    string,
    Date,
    att.Nat
]): att.Micheline => {
    return att.pair_to_mich([att.string_to_mich(p[0]), att.date_to_mich(p[1]), p[2].to_mich()]);
}
const exec_arg_to_mich = (addr: att.Address): att.Micheline => {
    return addr.to_mich();
}
export class Test_oracle {
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
        const address = (await ex.deploy("../tests/passed/test_oracle.arl", {}, params)).address;
        this.address = address;
    }
    async update_value(p: [
        string,
        Date,
        att.Nat
    ], params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update_value", update_value_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(addr: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(addr), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_value_param(p: [
        string,
        Date,
        att.Nat
    ], params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update_value", update_value_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(addr: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(addr), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_v(): Promise<[
        string,
        Date,
        att.Nat
    ]> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return (p => {
                return [att.mich_to_string((p as att.Mpair).args[0]), att.mich_to_date((p as att.Mpair).args[1]), att.Nat.from_mich((p as att.Mpair).args[2])];
            })(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_oracle = new Test_oracle();
