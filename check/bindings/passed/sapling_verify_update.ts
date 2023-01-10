import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const init_arg_to_mich = (s: att.Sapling_state, t: att.Sapling_transaction): att.Micheline => {
    return att.pair_to_mich([
        s.to_mich(),
        t.to_mich()
    ]);
}
export class Sapling_verify_update {
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
        const address = (await ex.deploy("../tests/passed/sapling_verify_update.arl", {}, params)).address;
        this.address = address;
    }
    async init(s: att.Sapling_state, t: att.Sapling_transaction, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "init", init_arg_to_mich(s, t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_init_param(s: att.Sapling_state, t: att.Sapling_transaction, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "init", init_arg_to_mich(s, t), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<att.Option<[
        att.Bytes,
        att.Int,
        att.Sapling_state
    ]>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich(storage, x => { return (p => {
                return [att.Bytes.from_mich((p as att.Mpair).args[0]), att.Int.from_mich((p as att.Mpair).args[1]), att.Sapling_state.from_mich((p as att.Mpair).args[2])];
            })(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const sapling_verify_update = new Sapling_verify_update();
