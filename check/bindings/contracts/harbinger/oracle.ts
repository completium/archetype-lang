import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum states {
    Running = 1,
    Revoked
}
export const mich_to_state = (m: any): states => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return states.Running;
        case 1: return states.Revoked;
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const oracleData_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export class oracleData_value implements att.ArchetypeType {
    constructor(public start: Date, public end: Date, public open: att.Nat, public high: att.Nat, public low: att.Nat, public close: att.Nat, public volume: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.date_to_mich(this.start), att.date_to_mich(this.end), this.open.to_mich(), this.high.to_mich(), this.low.to_mich(), this.close.to_mich(), this.volume.to_mich()]);
    }
    equals(v: oracleData_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): oracleData_value {
        return new oracleData_value(att.mich_to_date((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]), att.Nat.from_mich((input as att.Mpair).args[2]), att.Nat.from_mich((input as att.Mpair).args[3]), att.Nat.from_mich((input as att.Mpair).args[4]), att.Nat.from_mich((input as att.Mpair).args[5]), att.Nat.from_mich((input as att.Mpair).args[6]));
    }
}
export const oracleData_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%start"]),
    att.prim_annot_to_mich_type("timestamp", ["%end"]),
    att.prim_annot_to_mich_type("nat", ["%open"]),
    att.prim_annot_to_mich_type("nat", ["%high"]),
    att.prim_annot_to_mich_type("nat", ["%low"]),
    att.prim_annot_to_mich_type("nat", ["%close"]),
    att.prim_annot_to_mich_type("nat", ["%volume"])
], []);
export type oracleData_container = Array<[
    string,
    oracleData_value
]>;
export const oracleData_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%start"]),
    att.prim_annot_to_mich_type("timestamp", ["%end"]),
    att.prim_annot_to_mich_type("nat", ["%open"]),
    att.prim_annot_to_mich_type("nat", ["%high"]),
    att.prim_annot_to_mich_type("nat", ["%low"]),
    att.prim_annot_to_mich_type("nat", ["%close"]),
    att.prim_annot_to_mich_type("nat", ["%volume"])
], []), []);
const update_arg_to_mich = (upm: Array<[
    string,
    [
        att.Signature,
        oracleData_value
    ]
]>): att.Micheline => {
    return att.list_to_mich(upm, x => {
        const x_key = x[0];
        const x_value = x[1];
        return att.elt_to_mich(att.string_to_mich(x_key), att.pair_to_mich([x_value[0].to_mich(), x_value[1].to_mich()]));
    });
}
const push_arg_to_mich = (normalizer: att.Entrypoint): att.Micheline => {
    return normalizer.to_mich();
}
const revoke_arg_to_mich = (sig: att.Signature): att.Micheline => {
    return sig.to_mich();
}
const view_getPrice_arg_to_mich = (c: string): att.Micheline => {
    return att.string_to_mich(c);
}
export class Oracle {
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
    async deploy(publickey: att.Key, init_data: Array<[
        string,
        [
            Date,
            Date,
            att.Nat,
            att.Nat,
            att.Nat,
            att.Nat,
            att.Nat
        ]
    ]>, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/harbinger/oracle.arl", {
            publickey: publickey.to_mich(),
            init_data: att.list_to_mich(init_data, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(att.string_to_mich(x_key), att.pair_to_mich([att.date_to_mich(x_value[0]), att.date_to_mich(x_value[1]), x_value[2].to_mich(), x_value[3].to_mich(), x_value[4].to_mich(), x_value[5].to_mich(), x_value[6].to_mich()]));
            })
        }, params)).address;
        this.address = address;
    }
    async update(upm: Array<[
        string,
        [
            att.Signature,
            oracleData_value
        ]
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update", update_arg_to_mich(upm), params);
        }
        throw new Error("Contract not initialised");
    }
    async push(normalizer: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "push", push_arg_to_mich(normalizer), params);
        }
        throw new Error("Contract not initialised");
    }
    async revoke(sig: att.Signature, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "revoke", revoke_arg_to_mich(sig), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_param(upm: Array<[
        string,
        [
            att.Signature,
            oracleData_value
        ]
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update", update_arg_to_mich(upm), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_push_param(normalizer: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "push", push_arg_to_mich(normalizer), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_revoke_param(sig: att.Signature, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "revoke", revoke_arg_to_mich(sig), params);
        }
        throw new Error("Contract not initialised");
    }
    async view_getPrice(c: string, params: Partial<ex.Parameters>): Promise<oracleData_value | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "getPrice", view_getPrice_arg_to_mich(c), params);
            return mich.value ? oracleData_value.from_mich(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async get_publickey(): Promise<att.Key> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Key.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_oracleData_value(key: string): Promise<oracleData_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), oracleData_key_mich_type);
            if (data != undefined) {
                return oracleData_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_oracleData_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), oracleData_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_state(): Promise<states> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const state = (storage as att.Mpair).args[2];
            switch (att.Int.from_mich(state).to_number()) {
                case 0: return states.Running;
                case 1: return states.Revoked;
            }
        }
        return states.Running;
    }
    errors = {
        INVALID_STATE: att.string_to_mich("\"INVALID_STATE\""),
        r0: att.string_to_mich("\"bad sig\""),
        BAD_SIG: att.string_to_mich("\"bad sig\""),
        REVOKED: att.string_to_mich("\"revoked\""),
        BAD_REQUEST: att.string_to_mich("\"bad request\"")
    };
}
export const oracle = new Oracle();
