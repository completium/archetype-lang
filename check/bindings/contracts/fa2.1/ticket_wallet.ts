import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class import_ticket_param implements att.ArchetypeType {
    constructor(public itp_to: att.Address, public itp_tickets: att.Ticket<[
        att.Nat,
        att.Option<att.Bytes>
    ]>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.itp_to.to_mich(), this.itp_tickets.to_mich((x => { return att.pair_to_mich([x[0].to_mich(), x[1].to_mich((x => { return x.to_mich(); }))]); }))]);
    }
    equals(v: import_ticket_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): import_ticket_param {
        return new import_ticket_param(att.Address.from_mich((input as att.Mpair).args[0]), att.Ticket.from_mich(att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 4)), x => { return (p => {
            return [att.Nat.from_mich((p as att.Mpair).args[0]), att.Option.from_mich((p as att.Mpair).args[1], x => { return att.Bytes.from_mich(x); })];
        })(x); }));
    }
}
export const import_ticket_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%to_"]),
    att.ticket_annot_to_mich_type(att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", []),
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("bytes", []), [])
    ], []), ["%tickets_to_import"])
], []);
const callback_arg_to_mich = (input: att.Ticket<[
    att.Nat,
    att.Option<att.Bytes>
]>): att.Micheline => {
    return input.to_mich((x => { return att.pair_to_mich([x[0].to_mich(), x[1].to_mich((x => { return x.to_mich(); }))]); }));
}
const transfer_ticket_arg_to_mich = (fa2_1: att.Address, to_: att.Address): att.Micheline => {
    return att.pair_to_mich([
        fa2_1.to_mich(),
        to_.to_mich()
    ]);
}
export class Ticket_wallet {
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
        const address = (await ex.deploy("../tests/contracts/fa2.1/ticket_wallet.arl", {}, params)).address;
        this.address = address;
    }
    async callback(input: att.Ticket<[
        att.Nat,
        att.Option<att.Bytes>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "callback", callback_arg_to_mich(input), params);
        }
        throw new Error("Contract not initialised");
    }
    async transfer_ticket(fa2_1: att.Address, to_: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "transfer_ticket", transfer_ticket_arg_to_mich(fa2_1, to_), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_callback_param(input: att.Ticket<[
        att.Nat,
        att.Option<att.Bytes>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "callback", callback_arg_to_mich(input), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_transfer_ticket_param(fa2_1: att.Address, to_: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "transfer_ticket", transfer_ticket_arg_to_mich(fa2_1, to_), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_ticket(): Promise<att.Option<att.Ticket<[
        att.Nat,
        att.Option<att.Bytes>
    ]>>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[0], x => { return att.Ticket.from_mich(x, x => { return (p => {
                return [att.Nat.from_mich((p as att.Mpair).args[0]), att.Option.from_mich((p as att.Mpair).args[1], x => { return att.Bytes.from_mich(x); })];
            })(x); }); });
        }
        throw new Error("Contract not initialised");
    }
    async get_metadata_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return att.Bytes.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_metadata_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const ticket_wallet = new Ticket_wallet();
