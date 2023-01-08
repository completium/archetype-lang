import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const e0_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const put_arg_to_mich = (m: Array<[
    string,
    att.Nat
]>, k: string, v: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        att.list_to_mich(m, x => {
            const x_key = x[0];
            const x_value = x[1];
            return att.elt_to_mich(att.string_to_mich(x_key), x_value.to_mich());
        }),
        att.string_to_mich(k),
        v.to_mich()
    ]);
}
const e2_arg_to_mich = (l: Array<att.Nat>): att.Micheline => {
    return att.list_to_mich(l, x => {
        return x.to_mich();
    });
}
export class Parameter_expr_map {
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
        const address = (await ex.deploy("../tests/passed/parameter_expr_map.arl", {}, params)).address;
        this.address = address;
    }
    async e0(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e0", e0_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async put(m: Array<[
        string,
        att.Nat
    ]>, k: string, v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "put", put_arg_to_mich(m, k, v), params);
        }
        throw new Error("Contract not initialised");
    }
    async e2(l: Array<att.Nat>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e2", e2_arg_to_mich(l), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e0_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e0", e0_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_put_param(m: Array<[
        string,
        att.Nat
    ]>, k: string, v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "put", put_arg_to_mich(m, k, v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e2_param(l: Array<att.Nat>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e2", e2_arg_to_mich(l), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_smap(): Promise<Array<[
        string,
        att.Nat
    ]>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.mich_to_string(x), att.Nat.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_slist(): Promise<Array<att.Nat>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[1], x => { return att.Nat.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const parameter_expr_map = new Parameter_expr_map();
