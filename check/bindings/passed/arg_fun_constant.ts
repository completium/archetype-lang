import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const manage_transfers_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Arg_fun_constant {
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
        const address = (await ex.deploy("../tests/passed/arg_fun_constant.arl", {}, params)).address;
        this.address = address;
    }
    async manage_transfers(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "manage_transfers", manage_transfers_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_manage_transfers_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "manage_transfers", manage_transfers_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        s0: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"s0\"")])
    };
}
export const arg_fun_constant = new Arg_fun_constant();
