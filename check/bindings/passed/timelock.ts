import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (k: att.Chest_key, c: att.Chest, t: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        k.to_mich(),
        c.to_mich(),
        t.to_mich()
    ]);
}
export class Timelock {
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
        const address = (await ex.deploy("../tests/passed/timelock.arl", {}, params)).address;
        this.address = address;
    }
    async exec(k: att.Chest_key, c: att.Chest, t: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(k, c, t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(k: att.Chest_key, c: att.Chest, t: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(k, c, t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Bytes> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bytes.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const timelock = new Timelock();
