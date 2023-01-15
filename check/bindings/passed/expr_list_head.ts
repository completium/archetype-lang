import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (l: Array<string>, n: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        att.list_to_mich(l, x => {
            return att.string_to_mich(x);
        }),
        n.to_mich()
    ]);
}
export class Expr_list_head {
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
        const address = (await ex.deploy("../tests/passed/expr_list_head.arl", {}, params)).address;
        this.address = address;
    }
    async exec(l: Array<string>, n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(l, n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(l: Array<string>, n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(l, n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<Array<string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list(storage, x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const expr_list_head = new Expr_list_head();
