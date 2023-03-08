import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (a: att.Address): att.Micheline => {
    return a.to_mich();
}
export class Expr_fun_address_to_contract_unit {
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
        const address = (await ex.deploy("../tests/passed/expr_fun_address_to_contract_unit.arl", {}, params)).address;
        this.address = address;
    }
    async exec(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        ERROR: att.string_to_mich("\"ERROR\"")
    };
}
export const expr_fun_address_to_contract_unit = new Expr_fun_address_to_contract_unit();
