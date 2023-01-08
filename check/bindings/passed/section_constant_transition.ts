import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum states {
    First = 1,
    Second,
    Third
}
export const mich_to_state = (m: any): states => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return states.First;
        case 1: return states.Second;
        case 2: return states.Third;
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Section_constant_transition {
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
        const address = (await ex.deploy("../tests/passed/section_constant_transition.arl", {}, params)).address;
        this.address = address;
    }
    async exec(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res2(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_res3(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_state(): Promise<states> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const state = (storage as att.Mpair).args[2];
            switch (att.Int.from_mich(state).to_number()) {
                case 0: return states.First;
                case 1: return states.Second;
                case 2: return states.Third;
            }
        }
        return states.First;
    }
    errors = {
        INVALID_STATE: att.string_to_mich("\"INVALID_STATE\""),
        r2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r2\"")]),
        r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r1\"")]),
        ERROR: att.string_to_mich("\"ERROR\"")
    };
}
export const section_constant_transition = new Section_constant_transition();
