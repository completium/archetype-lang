import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const get_arg_to_mich = (s: string): att.Micheline => {
    return att.string_to_mich(s);
}
export const deploy_get_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("get", att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", []),
        att.prim_annot_to_mich_type("timestamp", []),
        att.prim_annot_to_mich_type("nat", [])
    ], []), params);
};
export class Test_oracle_called {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    get_callback_address: string | undefined;
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
        const address = (await ex.deploy("../tests/passed/test_oracle_called.arl", {}, params)).address;
        this.address = address;
        this.get_callback_address = (await deploy_get_callback(params)).address;
    }
    async get(s: string, params: Partial<ex.Parameters>): Promise<[
        string,
        Date,
        att.Nat
    ]> {
        if (this.address != undefined) {
            if (this.get_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.get_callback_address), "callback");
                await ex.call(this.address, "get", att.getter_args_to_mich(get_arg_to_mich(s), entrypoint), params);
                return await ex.get_callback_value<[
                    string,
                    Date,
                    att.Nat
                ]>(this.get_callback_address, x => { return (p => {
                    return [att.mich_to_string((p as att.Mpair).args[0]), att.mich_to_date((p as att.Mpair).args[1]), att.Nat.from_mich((p as att.Mpair).args[2])];
                })(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_oracle_called = new Test_oracle_called();
