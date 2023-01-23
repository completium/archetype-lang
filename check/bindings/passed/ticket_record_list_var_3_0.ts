import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class my_record implements att.ArchetypeType {
    constructor(public t: att.Option<att.Ticket<string>>, public n: att.Bytes, public s: string, public v: [
        att.Nat,
        string,
        att.Option<att.Address>
    ]) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.t.to_mich((x => { return x.to_mich((x => { return att.string_to_mich(x); })); })), this.n.to_mich(), att.string_to_mich(this.s), att.pair_to_mich([this.v[0].to_mich(), att.string_to_mich(this.v[1]), this.v[2].to_mich((x => { return x.to_mich(); }))])]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.Option.from_mich((input as att.Mpair).args[0], x => { return att.Ticket.from_mich(x, x => { return att.mich_to_string(x); }); }), att.Bytes.from_mich((input as att.Mpair).args[1]), att.mich_to_string((input as att.Mpair).args[2]), (p => {
            return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1]), att.Option.from_mich((p as att.Mpair).args[2], x => { return att.Address.from_mich(x); })];
        })(att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(3, 6))));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.option_annot_to_mich_type(att.ticket_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), ["%t"]),
    att.prim_annot_to_mich_type("bytes", ["%n"]),
    att.prim_annot_to_mich_type("string", ["%s"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", []),
        att.prim_annot_to_mich_type("string", []),
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), [])
    ], ["%v"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Ticket_record_list_var_3_0 {
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
        const address = (await ex.deploy("../tests/passed/ticket_record_list_var_3_0.arl", {}, params)).address;
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
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const ticket_record_list_var_3_0 = new Ticket_record_list_var_3_0();
