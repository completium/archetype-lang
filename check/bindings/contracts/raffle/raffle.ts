import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum states {
    Created = 1,
    Initialised,
    Transferred
}
export const mich_to_state = (m: any): states => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return states.Created;
        case 1: return states.Initialised;
        case 2: return states.Transferred;
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export class r_settings implements att.ArchetypeType {
    constructor(public open_buy: Date, public close_buy: Date, public chest_time: att.Nat, public reveal_fee: att.Rational) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.date_to_mich(this.open_buy), att.date_to_mich(this.close_buy), this.chest_time.to_mich(), this.reveal_fee.to_mich()]);
    }
    equals(v: r_settings): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): r_settings {
        return new r_settings(att.mich_to_date((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]), att.Nat.from_mich((input as att.Mpair).args[2]), att.Rational.from_mich(att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(3, 5))));
    }
}
export const r_settings_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%open_buy"]),
    att.prim_annot_to_mich_type("timestamp", ["%close_buy"]),
    att.prim_annot_to_mich_type("nat", ["%chest_time"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", []),
        att.prim_annot_to_mich_type("nat", [])
    ], ["%reveal_fee"])
], []);
export const player_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export class player_value implements att.ArchetypeType {
    constructor(public locked_raffle_key: att.Chest, public revealed: boolean) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.locked_raffle_key.to_mich(), att.bool_to_mich(this.revealed)]);
    }
    equals(v: player_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): player_value {
        return new player_value(att.Chest.from_mich((input as att.Mpair).args[0]), att.mich_to_bool((input as att.Mpair).args[1]));
    }
}
export const player_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("chest", ["%locked_raffle_key"]),
    att.prim_annot_to_mich_type("bool", ["%revealed"])
], []);
export type player_container = Array<[
    att.Address,
    player_value
]>;
export const player_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("chest", ["%locked_raffle_key"]),
    att.prim_annot_to_mich_type("bool", ["%revealed"])
], []), []);
const initialise_arg_to_mich = (ob: Date, cb: Date, t: att.Nat, rf: att.Rational): att.Micheline => {
    return att.pair_to_mich([
        att.date_to_mich(ob),
        att.date_to_mich(cb),
        t.to_mich(),
        rf.to_mich()
    ]);
}
const buy_arg_to_mich = (lrk: att.Chest): att.Micheline => {
    return lrk.to_mich();
}
const reveal_arg_to_mich = (addr: att.Address, k: att.Chest_key): att.Micheline => {
    return att.pair_to_mich([
        addr.to_mich(),
        k.to_mich()
    ]);
}
const transfer_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Raffle {
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
    async deploy(owner: att.Address, jackpot: att.Tez, ticket_price: att.Tez, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/raffle/raffle.arl", {
            owner: owner.to_mich(),
            jackpot: jackpot.to_mich(),
            ticket_price: ticket_price.to_mich()
        }, params)).address;
        this.address = address;
    }
    async initialise(ob: Date, cb: Date, t: att.Nat, rf: att.Rational, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "initialise", initialise_arg_to_mich(ob, cb, t, rf), params);
        }
        throw new Error("Contract not initialised");
    }
    async buy(lrk: att.Chest, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "buy", buy_arg_to_mich(lrk), params);
        }
        throw new Error("Contract not initialised");
    }
    async reveal(addr: att.Address, k: att.Chest_key, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "reveal", reveal_arg_to_mich(addr, k), params);
        }
        throw new Error("Contract not initialised");
    }
    async transfer(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "transfer", transfer_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_initialise_param(ob: Date, cb: Date, t: att.Nat, rf: att.Rational, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "initialise", initialise_arg_to_mich(ob, cb, t, rf), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_buy_param(lrk: att.Chest, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "buy", buy_arg_to_mich(lrk), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_reveal_param(addr: att.Address, k: att.Chest_key, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "reveal", reveal_arg_to_mich(addr, k), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_transfer_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "transfer", transfer_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_owner(): Promise<att.Address> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Address.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_jackpot(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ticket_price(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_o_settings(): Promise<att.Option<r_settings>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[3], x => { return r_settings.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_player(): Promise<player_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[4], (x, y) => [att.Address.from_mich(x), player_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_raffle_key(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[5]);
        }
        throw new Error("Contract not initialised");
    }
    async get_state(): Promise<states> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const state = (storage as att.Mpair).args[6];
            switch (att.Int.from_mich(state).to_number()) {
                case 0: return states.Created;
                case 1: return states.Initialised;
                case 2: return states.Transferred;
            }
        }
        return states.Created;
    }
    errors = {
        INVALID_STATE: att.string_to_mich("\"INVALID_STATE\""),
        INTERNAL_ERROR: att.string_to_mich("\"INTERNAL_ERROR\""),
        r7: att.string_to_mich("\"EXISTS_NOT_REVEALED\""),
        INVALID_CHEST_KEY: att.string_to_mich("\"INVALID_CHEST_KEY\""),
        r6: att.string_to_mich("\"PLAYER_ALREADY_REVEALED\""),
        r5: att.string_to_mich("\"RAFFLE_OPEN\""),
        PLAYER_NOT_FOUND: att.string_to_mich("\"PLAYER_NOT_FOUND\""),
        SETTINGS_NOT_INITIALIZED: att.string_to_mich("\"SETTINGS_NOT_INITIALIZED\""),
        r4: att.string_to_mich("\"RAFFLE_CLOSED\""),
        r3: att.string_to_mich("\"INVALID_TICKET_PRICE\""),
        r2: att.string_to_mich("\"INVALID_AMOUNT\""),
        r1: att.string_to_mich("\"INVALID_REVEAL_FEE\""),
        r0: att.string_to_mich("\"INVALID_OPEN_CLOSE_BUY\""),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\"")
    };
}
export const raffle = new Raffle();
