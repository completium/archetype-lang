import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum states {
    Init = 1,
    InProgress,
    Completed
}
export const mich_to_state = (m: any): states => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return states.Init;
        case 1: return states.InProgress;
        case 2: return states.Completed;
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const ident_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Test {
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
        const address = (await ex.deploy("../tests/passed/test.arl", {}, params)).address;
        this.address = address;
    }
    async ident(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "ident", ident_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_ident_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "ident", ident_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_state(): Promise<states> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const state = storage;
            switch (att.Int.from_mich(state).to_number()) {
                case 0: return states.Init;
                case 1: return states.InProgress;
                case 2: return states.Completed;
            }
        }
        return states.Init;
    }
    errors = {
        INVALID_STATE: att.string_to_mich("\"INVALID_STATE\"")
    };
}
export const test = new Test();
