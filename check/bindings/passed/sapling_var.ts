import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (t: att.Sapling_transaction): att.Micheline => {
    return t.to_mich();
}
export class Sapling_var {
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
        const address = (await ex.deploy("../tests/passed/sapling_var.arl", {}, params)).address;
        this.address = address;
    }
    async exec(t: att.Sapling_transaction, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(t: att.Sapling_transaction, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_s(): Promise<att.Sapling_state> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Sapling_state.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_n(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        BAD_TRANSACTION: att.string_to_mich("\"BAD TRANSACTION\"")
    };
}
export const sapling_var = new Sapling_var();
