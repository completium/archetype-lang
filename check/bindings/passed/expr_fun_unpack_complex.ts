import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (b: att.Bytes): att.Micheline => {
    return b.to_mich();
}
export class Expr_fun_unpack_complex {
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
        const address = (await ex.deploy("../tests/passed/expr_fun_unpack_complex.arl", {}, params)).address;
        this.address = address;
    }
    async exec(b: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(b), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(b: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(b), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Option<[
        att.Nat,
        string,
        att.Chain_id
    ]>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich(storage, x => { return (p => {
                return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1]), att.Chain_id.from_mich((p as att.Mpair).args[2])];
            })(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const expr_fun_unpack_complex = new Expr_fun_unpack_complex();
