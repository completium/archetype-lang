import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
import * as el from "@completium/event-listener";
export class ev implements att.ArchetypeType {
    constructor(public u: att.Unit, public b: boolean, public i: att.Int, public n: att.Nat, public r: att.Rational, public date_: Date, public du: att.Duration, public str: string, public addr: att.Address, public cur: att.Tez, public sig: att.Signature, public k: att.Key, public kh: att.Key_hash, public byt: att.Bytes, public cid: att.Chain_id, public s: Array<att.Nat>, public l: Array<att.Nat>, public m: Array<[
        att.Nat,
        string
    ]>, public o: att.Option<att.Nat>, public tu: [
        att.Nat,
        string
    ], public oal: att.Or<att.Nat, string>, public oar: att.Or<att.Nat, string>, public rr: rtype, public ee: my_enum, public eee: eaparam) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.unit_to_mich(), att.bool_to_mich(this.b), this.i.to_mich(), this.n.to_mich(), this.r.to_mich(), att.date_to_mich(this.date_), this.du.to_mich(), att.string_to_mich(this.str), this.addr.to_mich(), this.cur.to_mich(), this.sig.to_mich(), this.k.to_mich(), this.kh.to_mich(), this.byt.to_mich(), this.cid.to_mich(), att.list_to_mich(this.s, x => {
                return x.to_mich();
            }), att.list_to_mich(this.l, x => {
                return x.to_mich();
            }), att.list_to_mich(this.m, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), att.string_to_mich(x_value));
            }), this.o.to_mich((x => { return x.to_mich(); })), att.pair_to_mich([this.tu[0].to_mich(), att.string_to_mich(this.tu[1])]), this.oal.to_mich((x => { return x.to_mich(); }), (x => { return att.string_to_mich(x); })), this.oar.to_mich((x => { return x.to_mich(); }), (x => { return att.string_to_mich(x); })), this.rr.to_mich(), this.ee.to_mich(), this.eee.to_mich()]);
    }
    equals(v: ev): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): ev {
        return new ev(new att.Unit(), att.mich_to_bool((input as att.Mpair).args[1]), att.Int.from_mich((input as att.Mpair).args[2]), att.Nat.from_mich((input as att.Mpair).args[3]), att.Rational.from_mich((input as att.Mpair).args[4]), att.mich_to_date((input as att.Mpair).args[5]), att.Duration.from_mich((input as att.Mpair).args[6]), att.mich_to_string((input as att.Mpair).args[7]), att.Address.from_mich((input as att.Mpair).args[8]), att.Tez.from_mich((input as att.Mpair).args[9]), att.Signature.from_mich((input as att.Mpair).args[10]), att.Key.from_mich((input as att.Mpair).args[11]), att.Key_hash.from_mich((input as att.Mpair).args[12]), att.Bytes.from_mich((input as att.Mpair).args[13]), att.Chain_id.from_mich((input as att.Mpair).args[14]), att.mich_to_list((input as att.Mpair).args[15], x => { return att.Nat.from_mich(x); }), att.mich_to_list((input as att.Mpair).args[16], x => { return att.Nat.from_mich(x); }), att.mich_to_map((input as att.Mpair).args[17], (x, y) => [att.Nat.from_mich(x), att.mich_to_string(y)]), att.Option.from_mich((input as att.Mpair).args[18], x => { return att.Nat.from_mich(x); }), (p => {
            return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1])];
        })((input as att.Mpair).args[19]), att.Or.from_mich((input as att.Mpair).args[20], x => { return att.Nat.from_mich(x); }, x => { return att.mich_to_string(x); }), att.Or.from_mich((input as att.Mpair).args[21], x => { return att.Nat.from_mich(x); }, x => { return att.mich_to_string(x); }), rtype.from_mich((input as att.Mpair).args[22]), mich_to_my_enum((input as att.Mpair).args[23]), mich_to_eaparam((input as att.Mpair).args[24]));
    }
}
export enum my_enum_types {
    aaa = "aaa",
    bbb = "bbb"
}
export abstract class my_enum extends att.Enum<my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class aaa extends my_enum {
    constructor() {
        super(my_enum_types.aaa);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class bbb extends my_enum {
    constructor() {
        super(my_enum_types.bbb);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export enum eaparam_types {
    xxx = "xxx",
    yyy = "yyy"
}
export abstract class eaparam extends att.Enum<eaparam_types> {
    abstract to_mich(): att.Micheline;
    equals(v: eaparam): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class xxx extends eaparam {
    constructor(private content: att.Nat) {
        super(eaparam_types.xxx);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class yyy extends eaparam {
    constructor(private content: string) {
        super(eaparam_types.yyy);
    }
    to_mich() { return att.right_to_mich(att.string_to_mich(this.content)); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_my_enum = (m: any): my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new aaa();
        case 1: return new bbb();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const mich_to_eaparam = (m: att.Micheline): eaparam => {
    if ((m as att.Msingle).prim == "Left") {
        return new xxx(att.Nat.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new yyy(att.mich_to_string((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_eaparam : invalid micheline");
};
export class rtype implements att.ArchetypeType {
    constructor(public f1: att.Nat, public f2: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.f1.to_mich(), att.string_to_mich(this.f2)]);
    }
    equals(v: rtype): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): rtype {
        return new rtype(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_string((input as att.Mpair).args[1]));
    }
}
export const rtype_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%f1"]),
    att.prim_annot_to_mich_type("string", ["%f2"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Event_all {
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
        const address = (await ex.deploy("../tests/passed/event_all.arl", {}, params)).address;
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
    register_ev(ep: el.EventProcessor<ev>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "ev"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return ev.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const event_all = new Event_all();
