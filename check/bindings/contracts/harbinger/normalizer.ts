import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class queue implements att.ArchetypeType {
    constructor(public first: att.Int, public last: att.Int, public sum: att.Nat, public saved: Array<[
        att.Int,
        att.Nat
    ]>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.first.to_mich(), this.last.to_mich(), this.sum.to_mich(), att.list_to_mich(this.saved, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), x_value.to_mich());
            })]);
    }
    equals(v: queue): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): queue {
        return new queue(att.Int.from_mich((input as att.Mpair).args[0]), att.Int.from_mich((input as att.Mpair).args[1]), att.Nat.from_mich((input as att.Mpair).args[2]), att.mich_to_map((input as att.Mpair).args[3], (x, y) => [att.Int.from_mich(x), att.Nat.from_mich(y)]));
    }
}
export class update_param implements att.ArchetypeType {
    constructor(public start: Date, public end: Date, public open: att.Nat, public high: att.Nat, public low: att.Nat, public close: att.Nat, public volume: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.date_to_mich(this.start), att.date_to_mich(this.end), this.open.to_mich(), this.high.to_mich(), this.low.to_mich(), this.close.to_mich(), this.volume.to_mich()]);
    }
    equals(v: update_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): update_param {
        return new update_param(att.mich_to_date((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]), att.Nat.from_mich((input as att.Mpair).args[2]), att.Nat.from_mich((input as att.Mpair).args[3]), att.Nat.from_mich((input as att.Mpair).args[4]), att.Nat.from_mich((input as att.Mpair).args[5]), att.Nat.from_mich((input as att.Mpair).args[6]));
    }
}
export const queue_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%first"]),
    att.prim_annot_to_mich_type("int", ["%last"]),
    att.prim_annot_to_mich_type("nat", ["%sum"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), ["%saved"])
], []);
export const update_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%start"]),
    att.prim_annot_to_mich_type("timestamp", ["%end"]),
    att.prim_annot_to_mich_type("nat", ["%open"]),
    att.prim_annot_to_mich_type("nat", ["%high"]),
    att.prim_annot_to_mich_type("nat", ["%low"]),
    att.prim_annot_to_mich_type("nat", ["%close"]),
    att.prim_annot_to_mich_type("nat", ["%volume"])
], []);
export const assetMap_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export class assetMap_value implements att.ArchetypeType {
    constructor(public computedPrice: att.Nat, public lastUpdateTime: Date, public prices: queue, public volumes: queue) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.computedPrice.to_mich(), att.date_to_mich(this.lastUpdateTime), this.prices.to_mich(), this.volumes.to_mich()]);
    }
    equals(v: assetMap_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): assetMap_value {
        return new assetMap_value(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]), queue.from_mich((input as att.Mpair).args[2]), queue.from_mich(att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(3, 7))));
    }
}
export const assetMap_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%computedPrice"]),
    att.prim_annot_to_mich_type("timestamp", ["%lastUpdateTime"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%first"]),
        att.prim_annot_to_mich_type("int", ["%last"]),
        att.prim_annot_to_mich_type("nat", ["%sum"]),
        att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), ["%saved"])
    ], ["%prices"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%first"]),
        att.prim_annot_to_mich_type("int", ["%last"]),
        att.prim_annot_to_mich_type("nat", ["%sum"]),
        att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), ["%saved"])
    ], ["%volumes"])
], []);
export type assetMap_container = Array<[
    string,
    assetMap_value
]>;
export const assetMap_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%computedPrice"]),
    att.prim_annot_to_mich_type("timestamp", ["%lastUpdateTime"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%first"]),
        att.prim_annot_to_mich_type("int", ["%last"]),
        att.prim_annot_to_mich_type("nat", ["%sum"]),
        att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), ["%saved"])
    ], ["%prices"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%first"]),
        att.prim_annot_to_mich_type("int", ["%last"]),
        att.prim_annot_to_mich_type("nat", ["%sum"]),
        att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), ["%saved"])
    ], ["%volumes"])
], []), []);
const update_arg_to_mich = (upm: Array<[
    string,
    update_param
]>): att.Micheline => {
    return att.list_to_mich(upm, x => {
        const x_key = x[0];
        const x_value = x[1];
        return att.elt_to_mich(att.string_to_mich(x_key), x_value.to_mich());
    });
}
const get_arg_to_mich = (requestedAsset: string): att.Micheline => {
    return att.string_to_mich(requestedAsset);
}
const view_getPrice_arg_to_mich = (requestedAsset: string): att.Micheline => {
    return att.string_to_mich(requestedAsset);
}
export const deploy_get_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("get", att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", []),
        att.prim_annot_to_mich_type("timestamp", []),
        att.prim_annot_to_mich_type("nat", [])
    ], []), params);
};
export class Normalizer {
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
    async deploy(assetCodes: Array<string>, oracleContract: att.Address, numDataPoints: att.Nat, empty_queue: queue, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/harbinger/normalizer.arl", {
            assetCodes: att.list_to_mich(assetCodes, x => {
                return att.string_to_mich(x);
            }),
            oracleContract: oracleContract.to_mich(),
            numDataPoints: numDataPoints.to_mich(),
            empty_queue: empty_queue.to_mich()
        }, params)).address;
        this.address = address;
        this.get_callback_address = (await deploy_get_callback(params)).address;
    }
    async update(upm: Array<[
        string,
        update_param
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update", update_arg_to_mich(upm), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_param(upm: Array<[
        string,
        update_param
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update", update_arg_to_mich(upm), params);
        }
        throw new Error("Contract not initialised");
    }
    async get(requestedAsset: string, params: Partial<ex.Parameters>): Promise<[
        string,
        Date,
        att.Nat
    ]> {
        if (this.address != undefined) {
            if (this.get_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.get_callback_address), "callback");
                await ex.call(this.address, "get", att.getter_args_to_mich(get_arg_to_mich(requestedAsset), entrypoint), params);
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
    async view_getPrice(requestedAsset: string, params: Partial<ex.Parameters>): Promise<[
        Date,
        att.Nat
    ] | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "getPrice", view_getPrice_arg_to_mich(requestedAsset), params);
            return mich.value ? (p => {
                return [att.mich_to_date((p as att.Mpair).args[0]), att.Nat.from_mich((p as att.Mpair).args[1])];
            })(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async get_assetCodes(): Promise<Array<string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[0], x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_oracleContract(): Promise<att.Address> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Address.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_numDataPoints(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_assetMap_value(key: string): Promise<assetMap_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[3]).toString()), att.string_to_mich(key), assetMap_key_mich_type);
            if (data != undefined) {
                return assetMap_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_assetMap_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[3]).toString()), att.string_to_mich(key), assetMap_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        OPTION_IS_NONE: att.string_to_mich("\"OPTION_IS_NONE\""),
        BAD_SENDER: att.string_to_mich("\"bad sender\""),
        BAD_REQUEST: att.string_to_mich("\"bad request\""),
        INVALID_SUM: att.string_to_mich("\"invalid sum\"")
    };
}
export const normalizer = new Normalizer();
