import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public u: att.Option<att.Ticket<string>>, public n: att.Bytes, public v: [
        att.Option<string>,
        att.Ticket<string>,
        att.Option<att.Address>
    ], public s: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.u.to_mich((x => { return x.to_mich((x => { return att.string_to_mich(x); })); })), this.n.to_mich(), att.pair_to_mich([this.v[0].to_mich((x => { return att.string_to_mich(x); })), this.v[1].to_mich((x => { return att.string_to_mich(x); })), this.v[2].to_mich((x => { return x.to_mich(); }))]), att.string_to_mich(this.s)]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.Option.from_mich((input as att.Mpair).args[0], x => { return att.Ticket.from_mich(x, x => { return att.mich_to_string(x); }); }), att.Bytes.from_mich((input as att.Mpair).args[1]), (p => {
            return [att.Option.from_mich((p as att.Mpair).args[0], x => { return att.mich_to_string(x); }), att.Ticket.from_mich((p as att.Mpair).args[1], x => { return att.mich_to_string(x); }), att.Option.from_mich((p as att.Mpair).args[2], x => { return att.Address.from_mich(x); })];
        })((input as att.Mpair).args[2]), att.mich_to_string((input as att.Mpair).args[3]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.option_annot_to_mich_type(att.ticket_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), ["%u"]),
    att.prim_annot_to_mich_type("bytes", ["%n"]),
    att.pair_array_to_mich_type([
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []),
        att.ticket_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []),
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), [])
    ], ["%v"]),
    att.prim_annot_to_mich_type("string", ["%s"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Ticket_read_ticket_record_list {
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
        const address = (await ex.deploy("../tests/passed/ticket_read_ticket_record_list.arl", {}, params)).address;
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
    async get_res(): Promise<[
        att.Address,
        string,
        att.Nat
    ]> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return (p => {
                return [att.Address.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1]), att.Nat.from_mich((p as att.Mpair).args[2])];
            })(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        ERROR: att.string_to_mich("\"ERROR\"")
    };
}
export const ticket_read_ticket_record_list = new Ticket_read_ticket_record_list();
