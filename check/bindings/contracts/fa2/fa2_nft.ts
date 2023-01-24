import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum update_op_types {
    add_operator = "add_operator",
    remove_operator = "remove_operator"
}
export abstract class update_op extends att.Enum<update_op_types> {
    abstract to_mich(): att.Micheline;
    equals(v: update_op): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class add_operator extends update_op {
    constructor(private content: operator_param) {
        super(update_op_types.add_operator);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class remove_operator extends update_op {
    constructor(private content: operator_param) {
        super(update_op_types.remove_operator);
    }
    to_mich() { return att.right_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export enum update_for_all_op_types {
    add_for_all = "add_for_all",
    remove_for_all = "remove_for_all"
}
export abstract class update_for_all_op extends att.Enum<update_for_all_op_types> {
    abstract to_mich(): att.Micheline;
    equals(v: update_for_all_op): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class add_for_all extends update_for_all_op {
    constructor(private content: att.Address) {
        super(update_for_all_op_types.add_for_all);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class remove_for_all extends update_for_all_op {
    constructor(private content: att.Address) {
        super(update_for_all_op_types.remove_for_all);
    }
    to_mich() { return att.right_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_update_op = (m: att.Micheline): update_op => {
    if ((m as att.Msingle).prim == "Left") {
        return new add_operator(operator_param.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new remove_operator(operator_param.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_update_op : invalid micheline");
};
export const mich_to_update_for_all_op = (m: att.Micheline): update_for_all_op => {
    if ((m as att.Msingle).prim == "Left") {
        return new add_for_all(att.Address.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new remove_for_all(att.Address.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_update_for_all_op : invalid micheline");
};
export class transfer_destination implements att.ArchetypeType {
    constructor(public to_dest: att.Address, public token_id_dest: att.Nat, public token_amount_dest: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.to_dest.to_mich(), att.pair_to_mich([this.token_id_dest.to_mich(), this.token_amount_dest.to_mich()])]);
    }
    equals(v: transfer_destination): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): transfer_destination {
        return new transfer_destination(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export class transfer_param implements att.ArchetypeType {
    constructor(public tp_from: att.Address, public tp_txs: Array<transfer_destination>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.tp_from.to_mich(), att.list_to_mich(this.tp_txs, x => {
                return x.to_mich();
            })]);
    }
    equals(v: transfer_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): transfer_param {
        return new transfer_param(att.Address.from_mich((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return transfer_destination.from_mich(x); }));
    }
}
export class part implements att.ArchetypeType {
    constructor(public part_account: att.Address, public part_value: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.part_account.to_mich(), this.part_value.to_mich()]);
    }
    equals(v: part): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): part {
        return new part(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export class operator_param implements att.ArchetypeType {
    constructor(public opp_owner: att.Address, public opp_operator: att.Address, public opp_token_id: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.opp_owner.to_mich(), att.pair_to_mich([this.opp_operator.to_mich(), this.opp_token_id.to_mich()])]);
    }
    equals(v: operator_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): operator_param {
        return new operator_param(att.Address.from_mich((input as att.Mpair).args[0]), att.Address.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export class gasless_param implements att.ArchetypeType {
    constructor(public transfer_params: Array<transfer_param>, public user_pk: att.Key, public user_sig: att.Signature) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.list_to_mich(this.transfer_params, x => {
                return x.to_mich();
            }), this.user_pk.to_mich(), this.user_sig.to_mich()]);
    }
    equals(v: gasless_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): gasless_param {
        return new gasless_param(att.mich_to_list((input as att.Mpair).args[0], x => { return transfer_param.from_mich(x); }), att.Key.from_mich((input as att.Mpair).args[1]), att.Signature.from_mich((input as att.Mpair).args[2]));
    }
}
export class balance_of_request implements att.ArchetypeType {
    constructor(public bo_owner: att.Address, public btoken_id: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.bo_owner.to_mich(), this.btoken_id.to_mich()]);
    }
    equals(v: balance_of_request): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): balance_of_request {
        return new balance_of_request(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export class balance_of_response implements att.ArchetypeType {
    constructor(public request: balance_of_request, public balance_: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.request.to_mich(), this.balance_.to_mich()]);
    }
    equals(v: balance_of_response): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): balance_of_response {
        return new balance_of_response(balance_of_request.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export const transfer_destination_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%to_"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%token_id"]),
        att.prim_annot_to_mich_type("nat", ["%amount"])
    ], [])
], []);
export const transfer_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%from_"]),
    att.list_annot_to_mich_type(att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%to_"]),
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("nat", ["%token_id"]),
            att.prim_annot_to_mich_type("nat", ["%amount"])
        ], [])
    ], []), ["%txs"])
], []);
export const part_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%part_account"]),
    att.prim_annot_to_mich_type("nat", ["%part_value"])
], []);
export const operator_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%owner"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%operator"]),
        att.prim_annot_to_mich_type("nat", ["%token_id"])
    ], [])
], []);
export const gasless_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.list_annot_to_mich_type(att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%from_"]),
        att.list_annot_to_mich_type(att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("address", ["%to_"]),
            att.pair_array_to_mich_type([
                att.prim_annot_to_mich_type("nat", ["%token_id"]),
                att.prim_annot_to_mich_type("nat", ["%amount"])
            ], [])
        ], []), ["%txs"])
    ], []), ["%transfer_params"]),
    att.prim_annot_to_mich_type("key", ["%user_pk"]),
    att.prim_annot_to_mich_type("signature", ["%user_sig"])
], []);
export const balance_of_request_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%owner"]),
    att.prim_annot_to_mich_type("nat", ["%token_id"])
], []);
export const balance_of_response_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%owner"]),
        att.prim_annot_to_mich_type("nat", ["%token_id"])
    ], ["%request"]),
    att.prim_annot_to_mich_type("nat", ["%balance"])
], []);
export class operator_key implements att.ArchetypeType {
    constructor(public oaddr: att.Address, public otoken: att.Nat, public oowner: att.Address) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.oaddr.to_mich(), att.pair_to_mich([this.otoken.to_mich(), this.oowner.to_mich()])]);
    }
    equals(v: operator_key): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): operator_key {
        return new operator_key(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Address.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export class operator_for_all_key implements att.ArchetypeType {
    constructor(public fa_oaddr: att.Address, public fa_oowner: att.Address) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.fa_oaddr.to_mich(), this.fa_oowner.to_mich()]);
    }
    equals(v: operator_for_all_key): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): operator_for_all_key {
        return new operator_for_all_key(att.Address.from_mich((input as att.Mpair).args[0]), att.Address.from_mich((input as att.Mpair).args[1]));
    }
}
export const token_metadata_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const ledger_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const royalties_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const operator_key_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%oaddr"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%otoken"]),
        att.prim_annot_to_mich_type("address", ["%oowner"])
    ], [])
], []);
export const operator_for_all_key_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%fa_oaddr"]),
    att.prim_annot_to_mich_type("address", ["%fa_oowner"])
], []);
export class token_metadata_value implements att.ArchetypeType {
    constructor(public token_id: att.Nat, public token_info: Array<[
        string,
        att.Bytes
    ]>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.token_id.to_mich(), att.list_to_mich(this.token_info, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(att.string_to_mich(x_key), x_value.to_mich());
            })]);
    }
    equals(v: token_metadata_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): token_metadata_value {
        return new token_metadata_value(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_map((input as att.Mpair).args[1], (x, y) => [att.mich_to_string(x), att.Bytes.from_mich(y)]));
    }
}
export class operator_value implements att.ArchetypeType {
    constructor() { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.unit_to_mich();
    }
    equals(v: operator_value): boolean {
        return true;
    }
    static from_mich(input: att.Micheline): operator_value {
        return new operator_value();
    }
}
export class operator_for_all_value implements att.ArchetypeType {
    constructor() { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.unit_to_mich();
    }
    equals(v: operator_for_all_value): boolean {
        return true;
    }
    static from_mich(input: att.Micheline): operator_for_all_value {
        return new operator_for_all_value();
    }
}
export const token_metadata_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%token_id"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("bytes", []), ["%token_info"])
], []);
export const ledger_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const royalties_value_mich_type: att.MichelineType = att.list_annot_to_mich_type(att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%part_account"]),
    att.prim_annot_to_mich_type("nat", ["%part_value"])
], []), []);
export const operator_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("unit", []);
export const operator_for_all_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("unit", []);
export type token_metadata_container = Array<[
    att.Nat,
    token_metadata_value
]>;
export type ledger_container = Array<[
    att.Nat,
    att.Address
]>;
export type royalties_container = Array<[
    att.Nat,
    Array<part>
]>;
export type operator_container = Array<[
    operator_key,
    operator_value
]>;
export type operator_for_all_container = Array<[
    operator_for_all_key,
    operator_for_all_value
]>;
export const token_metadata_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%token_id"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("bytes", []), ["%token_info"])
], []), []);
export const ledger_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("address", []), []);
export const royalties_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.list_annot_to_mich_type(att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%part_account"]),
    att.prim_annot_to_mich_type("nat", ["%part_value"])
], []), []), []);
export const operator_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%oaddr"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%otoken"]),
        att.prim_annot_to_mich_type("address", ["%oowner"])
    ], [])
], []), att.prim_annot_to_mich_type("unit", []), []);
export const operator_for_all_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%fa_oaddr"]),
    att.prim_annot_to_mich_type("address", ["%fa_oowner"])
], []), att.prim_annot_to_mich_type("unit", []), []);
const declare_ownership_arg_to_mich = (candidate: att.Address): att.Micheline => {
    return candidate.to_mich();
}
const claim_ownership_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const pause_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const unpause_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const set_metadata_arg_to_mich = (k: string, d: att.Option<att.Bytes>): att.Micheline => {
    return att.pair_to_mich([
        att.string_to_mich(k),
        d.to_mich((x => { return x.to_mich(); }))
    ]);
}
const set_token_metadata_arg_to_mich = (tid: att.Nat, tdata: Array<[
    string,
    att.Bytes
]>): att.Micheline => {
    return att.pair_to_mich([
        tid.to_mich(),
        att.list_to_mich(tdata, x => {
            const x_key = x[0];
            const x_value = x[1];
            return att.elt_to_mich(att.string_to_mich(x_key), x_value.to_mich());
        })
    ]);
}
const set_permits_arg_to_mich = (p: att.Address): att.Micheline => {
    return p.to_mich();
}
const update_operators_arg_to_mich = (upl: Array<update_op>): att.Micheline => {
    return att.list_to_mich(upl, x => {
        return x.to_mich();
    });
}
const update_operators_for_all_arg_to_mich = (upl: Array<update_for_all_op>): att.Micheline => {
    return att.list_to_mich(upl, x => {
        return x.to_mich();
    });
}
const do_transfer_arg_to_mich = (txs: Array<transfer_param>): att.Micheline => {
    return att.list_to_mich(txs, x => {
        return x.to_mich();
    });
}
const transfer_gasless_arg_to_mich = (batch: Array<gasless_param>): att.Micheline => {
    return att.list_to_mich(batch, x => {
        return x.to_mich();
    });
}
const transfer_arg_to_mich = (txs: Array<transfer_param>): att.Micheline => {
    return att.list_to_mich(txs, x => {
        return x.to_mich();
    });
}
const mint_arg_to_mich = (tow: att.Address, tid: att.Nat, tmd: Array<[
    string,
    att.Bytes
]>, roy: Array<part>): att.Micheline => {
    return att.pair_to_mich([
        tow.to_mich(),
        tid.to_mich(),
        att.list_to_mich(tmd, x => {
            const x_key = x[0];
            const x_value = x[1];
            return att.elt_to_mich(att.string_to_mich(x_key), x_value.to_mich());
        }),
        att.list_to_mich(roy, x => {
            return x.to_mich();
        })
    ]);
}
const burn_arg_to_mich = (tid: att.Nat): att.Micheline => {
    return tid.to_mich();
}
const permit_transfer_arg_to_mich = (txs: Array<transfer_param>, permit: att.Option<[
    att.Key,
    att.Signature
]>): att.Micheline => {
    return att.pair_to_mich([
        att.list_to_mich(txs, x => {
            return x.to_mich();
        }),
        permit.to_mich((x => { return att.pair_to_mich([x[0].to_mich(), x[1].to_mich()]); }))
    ]);
}
const balance_of_arg_to_mich = (requests: Array<balance_of_request>): att.Micheline => {
    return att.list_to_mich(requests, x => {
        return x.to_mich();
    });
}
const view_get_royalties_arg_to_mich = (tokenId: att.Nat): att.Micheline => {
    return tokenId.to_mich();
}
export const deploy_balance_of_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("balance_of", att.list_annot_to_mich_type(att.pair_array_to_mich_type([
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("address", ["%owner"]),
            att.prim_annot_to_mich_type("nat", ["%token_id"])
        ], ["%request"]),
        att.prim_annot_to_mich_type("nat", ["%balance"])
    ], []), []), params);
};
export class Fa2_nft {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    balance_of_callback_address: string | undefined;
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
    async deploy(owner: att.Address, permits: att.Address, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/fa2/fa2_nft.arl", {
            owner: owner.to_mich(),
            permits: permits.to_mich()
        }, params)).address;
        this.address = address;
        this.balance_of_callback_address = (await deploy_balance_of_callback(params)).address;
    }
    async declare_ownership(candidate: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "declare_ownership", declare_ownership_arg_to_mich(candidate), params);
        }
        throw new Error("Contract not initialised");
    }
    async claim_ownership(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "claim_ownership", claim_ownership_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async pause(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "pause", pause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async unpause(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "unpause", unpause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_metadata(k: string, d: att.Option<att.Bytes>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_metadata", set_metadata_arg_to_mich(k, d), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_token_metadata(tid: att.Nat, tdata: Array<[
        string,
        att.Bytes
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_token_metadata", set_token_metadata_arg_to_mich(tid, tdata), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_permits(p: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_permits", set_permits_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async update_operators(upl: Array<update_op>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update_operators", update_operators_arg_to_mich(upl), params);
        }
        throw new Error("Contract not initialised");
    }
    async update_operators_for_all(upl: Array<update_for_all_op>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "update_operators_for_all", update_operators_for_all_arg_to_mich(upl), params);
        }
        throw new Error("Contract not initialised");
    }
    async do_transfer(txs: Array<transfer_param>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "do_transfer", do_transfer_arg_to_mich(txs), params);
        }
        throw new Error("Contract not initialised");
    }
    async transfer_gasless(batch: Array<gasless_param>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "transfer_gasless", transfer_gasless_arg_to_mich(batch), params);
        }
        throw new Error("Contract not initialised");
    }
    async transfer(txs: Array<transfer_param>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "transfer", transfer_arg_to_mich(txs), params);
        }
        throw new Error("Contract not initialised");
    }
    async mint(tow: att.Address, tid: att.Nat, tmd: Array<[
        string,
        att.Bytes
    ]>, roy: Array<part>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "mint", mint_arg_to_mich(tow, tid, tmd, roy), params);
        }
        throw new Error("Contract not initialised");
    }
    async burn(tid: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "burn", burn_arg_to_mich(tid), params);
        }
        throw new Error("Contract not initialised");
    }
    async permit_transfer(txs: Array<transfer_param>, permit: att.Option<[
        att.Key,
        att.Signature
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "permit_transfer", permit_transfer_arg_to_mich(txs, permit), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_declare_ownership_param(candidate: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "declare_ownership", declare_ownership_arg_to_mich(candidate), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_claim_ownership_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "claim_ownership", claim_ownership_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_pause_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "pause", pause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_unpause_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "unpause", unpause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_metadata_param(k: string, d: att.Option<att.Bytes>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_metadata", set_metadata_arg_to_mich(k, d), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_token_metadata_param(tid: att.Nat, tdata: Array<[
        string,
        att.Bytes
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_token_metadata", set_token_metadata_arg_to_mich(tid, tdata), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_permits_param(p: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_permits", set_permits_arg_to_mich(p), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_operators_param(upl: Array<update_op>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update_operators", update_operators_arg_to_mich(upl), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_update_operators_for_all_param(upl: Array<update_for_all_op>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "update_operators_for_all", update_operators_for_all_arg_to_mich(upl), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_do_transfer_param(txs: Array<transfer_param>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "do_transfer", do_transfer_arg_to_mich(txs), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_transfer_gasless_param(batch: Array<gasless_param>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "transfer_gasless", transfer_gasless_arg_to_mich(batch), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_transfer_param(txs: Array<transfer_param>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "transfer", transfer_arg_to_mich(txs), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_mint_param(tow: att.Address, tid: att.Nat, tmd: Array<[
        string,
        att.Bytes
    ]>, roy: Array<part>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "mint", mint_arg_to_mich(tow, tid, tmd, roy), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_burn_param(tid: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "burn", burn_arg_to_mich(tid), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_permit_transfer_param(txs: Array<transfer_param>, permit: att.Option<[
        att.Key,
        att.Signature
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "permit_transfer", permit_transfer_arg_to_mich(txs, permit), params);
        }
        throw new Error("Contract not initialised");
    }
    async balance_of(requests: Array<balance_of_request>, params: Partial<ex.Parameters>): Promise<Array<balance_of_response>> {
        if (this.address != undefined) {
            if (this.balance_of_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.balance_of_callback_address), "callback");
                await ex.call(this.address, "balance_of", att.getter_args_to_mich(balance_of_arg_to_mich(requests), entrypoint), params);
                return await ex.get_callback_value<Array<balance_of_response>>(this.balance_of_callback_address, x => { return att.mich_to_list(x, x => { return balance_of_response.from_mich(x); }); });
            }
        }
        throw new Error("Contract not initialised");
    }
    async view_get_royalties(tokenId: att.Nat, params: Partial<ex.Parameters>): Promise<Array<part> | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "get_royalties", view_get_royalties_arg_to_mich(tokenId), params);
            return mich.value ? att.mich_to_list(mich.value, x => { return part.from_mich(x); }) : undefined;
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
    async get_permits(): Promise<att.Address> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Address.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_owner_candidate(): Promise<att.Option<att.Address>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[2], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_paused(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_token_metadata_value(key: att.Nat): Promise<token_metadata_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[4]).toString()), key.to_mich(), token_metadata_key_mich_type);
            if (data != undefined) {
                return token_metadata_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_token_metadata_value(key: att.Nat): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[4]).toString()), key.to_mich(), token_metadata_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_ledger_value(key: att.Nat): Promise<att.Address | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[5]).toString()), key.to_mich(), ledger_key_mich_type);
            if (data != undefined) {
                return att.Address.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_ledger_value(key: att.Nat): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[5]).toString()), key.to_mich(), ledger_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_royalties_value(key: att.Nat): Promise<Array<part> | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), royalties_key_mich_type);
            if (data != undefined) {
                return att.mich_to_list(data, x => { return part.from_mich(x); });
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_royalties_value(key: att.Nat): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), royalties_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_operator_value(key: operator_key): Promise<operator_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[7]).toString()), key.to_mich(), operator_key_mich_type);
            if (data != undefined) {
                return operator_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_operator_value(key: operator_key): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[7]).toString()), key.to_mich(), operator_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_operator_for_all_value(key: operator_for_all_key): Promise<operator_for_all_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[8]).toString()), key.to_mich(), operator_for_all_key_mich_type);
            if (data != undefined) {
                return operator_for_all_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_operator_for_all_value(key: operator_for_all_key): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[8]).toString()), key.to_mich(), operator_for_all_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_metadata_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[9]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[9]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
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
        SIGNER_NOT_FROM: att.string_to_mich("\"SIGNER_NOT_FROM\""),
        fa2_r9: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r9\"")]),
        NO_TRANSFER: att.string_to_mich("\"NO_TRANSFER\""),
        fa2_r7: att.string_to_mich("\"FA2_NOT_OWNER\""),
        fa2_r6: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r6\"")]),
        FA2_TOKEN_UNDEFINED: att.string_to_mich("\"FA2_TOKEN_UNDEFINED\""),
        fa2_r5: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r5\"")]),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\""),
        fa2_r4: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r4\"")]),
        fa2_r3: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r3\"")]),
        FA2_INSUFFICIENT_BALANCE: att.string_to_mich("\"FA2_INSUFFICIENT_BALANCE\""),
        fa2_r2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r2\"")]),
        FA2_NOT_OWNER: att.string_to_mich("\"FA2_NOT_OWNER\""),
        fa2_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"fa2_r1\"")]),
        p_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"p_r1\"")]),
        tmd_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"tmd_r1\"")]),
        md_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"md_r1\"")]),
        pausable_r2: att.string_to_mich("\"CONTRACT_NOT_PAUSED\""),
        pausable_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"pausable_r1\"")]),
        ownership_r1: att.string_to_mich("\"INVALID_CALLER\""),
        FA2_NOT_OPERATOR: att.string_to_mich("\"FA2_NOT_OPERATOR\""),
        CONTRACT_PAUSED: att.string_to_mich("\"CONTRACT_PAUSED\"")
    };
}
export const fa2_nft = new Fa2_nft();
