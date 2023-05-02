import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum fa2_nft__update_op_types {
    add_operator = "add_operator",
    remove_operator = "remove_operator"
}
export abstract class fa2_nft__update_op extends att.Enum<fa2_nft__update_op_types> {
    abstract to_mich(): att.Micheline;
    equals(v: fa2_nft__update_op): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class add_operator extends fa2_nft__update_op {
    constructor(private content: fa2_nft__operator_param) {
        super(fa2_nft__update_op_types.add_operator);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class remove_operator extends fa2_nft__update_op {
    constructor(private content: fa2_nft__operator_param) {
        super(fa2_nft__update_op_types.remove_operator);
    }
    to_mich() { return att.right_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export enum fa2_nft__update_for_all_op_types {
    add_for_all = "add_for_all",
    remove_for_all = "remove_for_all"
}
export abstract class fa2_nft__update_for_all_op extends att.Enum<fa2_nft__update_for_all_op_types> {
    abstract to_mich(): att.Micheline;
    equals(v: fa2_nft__update_for_all_op): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class add_for_all extends fa2_nft__update_for_all_op {
    constructor(private content: att.Address) {
        super(fa2_nft__update_for_all_op_types.add_for_all);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class remove_for_all extends fa2_nft__update_for_all_op {
    constructor(private content: att.Address) {
        super(fa2_nft__update_for_all_op_types.remove_for_all);
    }
    to_mich() { return att.right_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_fa2_nft__update_op = (m: att.Micheline): fa2_nft__update_op => {
    if ((m as att.Msingle).prim == "Left") {
        return new add_operator(fa2_nft__operator_param.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new remove_operator(fa2_nft__operator_param.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_fa2_nft__update_op : invalid micheline");
};
export const mich_to_fa2_nft__update_for_all_op = (m: att.Micheline): fa2_nft__update_for_all_op => {
    if ((m as att.Msingle).prim == "Left") {
        return new add_for_all(att.Address.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new remove_for_all(att.Address.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_fa2_nft__update_for_all_op : invalid micheline");
};
export class fa2_nft__transfer_destination implements att.ArchetypeType {
    constructor(public to_dest: att.Address, public token_id_dest: att.Nat, public token_amount_dest: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.to_dest.to_mich(), att.pair_to_mich([this.token_id_dest.to_mich(), this.token_amount_dest.to_mich()])]);
    }
    equals(v: fa2_nft__transfer_destination): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__transfer_destination {
        return new fa2_nft__transfer_destination(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export class fa2_nft__transfer_param implements att.ArchetypeType {
    constructor(public tp_from: att.Address, public tp_txs: Array<fa2_nft__transfer_destination>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.tp_from.to_mich(), att.list_to_mich(this.tp_txs, x => {
                return x.to_mich();
            })]);
    }
    equals(v: fa2_nft__transfer_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__transfer_param {
        return new fa2_nft__transfer_param(att.Address.from_mich((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return fa2_nft__transfer_destination.from_mich(x); }));
    }
}
export class fa2_nft__part implements att.ArchetypeType {
    constructor(public part_account: att.Address, public part_value: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.part_account.to_mich(), this.part_value.to_mich()]);
    }
    equals(v: fa2_nft__part): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__part {
        return new fa2_nft__part(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export class fa2_nft__operator_param implements att.ArchetypeType {
    constructor(public opp_owner: att.Address, public opp_operator: att.Address, public opp_token_id: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.opp_owner.to_mich(), att.pair_to_mich([this.opp_operator.to_mich(), this.opp_token_id.to_mich()])]);
    }
    equals(v: fa2_nft__operator_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__operator_param {
        return new fa2_nft__operator_param(att.Address.from_mich((input as att.Mpair).args[0]), att.Address.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export class fa2_nft__gasless_param implements att.ArchetypeType {
    constructor(public transfer_params: Array<fa2_nft__transfer_param>, public user_pk: att.Key, public user_sig: att.Signature) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.list_to_mich(this.transfer_params, x => {
                return x.to_mich();
            }), this.user_pk.to_mich(), this.user_sig.to_mich()]);
    }
    equals(v: fa2_nft__gasless_param): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__gasless_param {
        return new fa2_nft__gasless_param(att.mich_to_list((input as att.Mpair).args[0], x => { return fa2_nft__transfer_param.from_mich(x); }), att.Key.from_mich((input as att.Mpair).args[1]), att.Signature.from_mich((input as att.Mpair).args[2]));
    }
}
export class fa2_nft__balance_of_request implements att.ArchetypeType {
    constructor(public bo_owner: att.Address, public btoken_id: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.bo_owner.to_mich(), this.btoken_id.to_mich()]);
    }
    equals(v: fa2_nft__balance_of_request): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__balance_of_request {
        return new fa2_nft__balance_of_request(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export class fa2_nft__balance_of_response implements att.ArchetypeType {
    constructor(public request: fa2_nft__balance_of_request, public balance_: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.request.to_mich(), this.balance_.to_mich()]);
    }
    equals(v: fa2_nft__balance_of_response): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__balance_of_response {
        return new fa2_nft__balance_of_response(fa2_nft__balance_of_request.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]));
    }
}
export const fa2_nft__transfer_destination_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%to_"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%token_id"]),
        att.prim_annot_to_mich_type("nat", ["%amount"])
    ], [])
], []);
export const fa2_nft__transfer_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%from_"]),
    att.list_annot_to_mich_type(att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%to_"]),
        att.pair_array_to_mich_type([
            att.prim_annot_to_mich_type("nat", ["%token_id"]),
            att.prim_annot_to_mich_type("nat", ["%amount"])
        ], [])
    ], []), ["%txs"])
], []);
export const fa2_nft__part_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%part_account"]),
    att.prim_annot_to_mich_type("nat", ["%part_value"])
], []);
export const fa2_nft__operator_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%owner"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%operator"]),
        att.prim_annot_to_mich_type("nat", ["%token_id"])
    ], [])
], []);
export const fa2_nft__gasless_param_mich_type: att.MichelineType = att.pair_array_to_mich_type([
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
export const fa2_nft__balance_of_request_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%owner"]),
    att.prim_annot_to_mich_type("nat", ["%token_id"])
], []);
export const fa2_nft__balance_of_response_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%owner"]),
        att.prim_annot_to_mich_type("nat", ["%token_id"])
    ], ["%request"]),
    att.prim_annot_to_mich_type("nat", ["%balance"])
], []);
export class fa2_nft__operator_key implements att.ArchetypeType {
    constructor(public oaddr: att.Address, public otoken: att.Nat, public oowner: att.Address) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.oaddr.to_mich(), att.pair_to_mich([this.otoken.to_mich(), this.oowner.to_mich()])]);
    }
    equals(v: fa2_nft__operator_key): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__operator_key {
        return new fa2_nft__operator_key(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Address.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export class fa2_nft__operator_for_all_key implements att.ArchetypeType {
    constructor(public fa_oaddr: att.Address, public fa_oowner: att.Address) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.fa_oaddr.to_mich(), this.fa_oowner.to_mich()]);
    }
    equals(v: fa2_nft__operator_for_all_key): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__operator_for_all_key {
        return new fa2_nft__operator_for_all_key(att.Address.from_mich((input as att.Mpair).args[0]), att.Address.from_mich((input as att.Mpair).args[1]));
    }
}
export const fa2_nft__token_metadata_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const fa2_nft__ledger_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const fa2_nft__royalties_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const fa2_nft__operator_key_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%oaddr"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%otoken"]),
        att.prim_annot_to_mich_type("address", ["%oowner"])
    ], [])
], []);
export const fa2_nft__operator_for_all_key_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%fa_oaddr"]),
    att.prim_annot_to_mich_type("address", ["%fa_oowner"])
], []);
export class fa2_nft__token_metadata_value implements att.ArchetypeType {
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
    equals(v: fa2_nft__token_metadata_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): fa2_nft__token_metadata_value {
        return new fa2_nft__token_metadata_value(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_map((input as att.Mpair).args[1], (x, y) => [att.mich_to_string(x), att.Bytes.from_mich(y)]));
    }
}
export class fa2_nft__operator_value implements att.ArchetypeType {
    constructor() { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.unit_to_mich();
    }
    equals(v: fa2_nft__operator_value): boolean {
        return true;
    }
    static from_mich(input: att.Micheline): fa2_nft__operator_value {
        return new fa2_nft__operator_value();
    }
}
export class fa2_nft__operator_for_all_value implements att.ArchetypeType {
    constructor() { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.unit_to_mich();
    }
    equals(v: fa2_nft__operator_for_all_value): boolean {
        return true;
    }
    static from_mich(input: att.Micheline): fa2_nft__operator_for_all_value {
        return new fa2_nft__operator_for_all_value();
    }
}
export const fa2_nft__token_metadata_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%token_id"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("bytes", []), ["%token_info"])
], []);
export const fa2_nft__ledger_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const fa2_nft__royalties_value_mich_type: att.MichelineType = att.list_annot_to_mich_type(att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%part_account"]),
    att.prim_annot_to_mich_type("nat", ["%part_value"])
], []), []);
export const fa2_nft__operator_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("unit", []);
export const fa2_nft__operator_for_all_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("unit", []);
export type fa2_nft__token_metadata_container = Array<[
    att.Nat,
    fa2_nft__token_metadata_value
]>;
export type fa2_nft__ledger_container = Array<[
    att.Nat,
    att.Address
]>;
export type fa2_nft__royalties_container = Array<[
    att.Nat,
    Array<fa2_nft__part>
]>;
export type fa2_nft__operator_container = Array<[
    fa2_nft__operator_key,
    fa2_nft__operator_value
]>;
export type fa2_nft__operator_for_all_container = Array<[
    fa2_nft__operator_for_all_key,
    fa2_nft__operator_for_all_value
]>;
export const fa2_nft__token_metadata_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%token_id"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.prim_annot_to_mich_type("bytes", []), ["%token_info"])
], []), []);
export const fa2_nft__ledger_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("address", []), []);
export const fa2_nft__royalties_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.list_annot_to_mich_type(att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%part_account"]),
    att.prim_annot_to_mich_type("nat", ["%part_value"])
], []), []), []);
export const fa2_nft__operator_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%oaddr"]),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%otoken"]),
        att.prim_annot_to_mich_type("address", ["%oowner"])
    ], [])
], []), att.prim_annot_to_mich_type("unit", []), []);
export const fa2_nft__operator_for_all_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%fa_oaddr"]),
    att.prim_annot_to_mich_type("address", ["%fa_oowner"])
], []), att.prim_annot_to_mich_type("unit", []), []);
const exec_arg_to_mich = (owner: att.Address, permits: att.Address): att.Micheline => {
    return att.pair_to_mich([
        owner.to_mich(),
        permits.to_mich()
    ]);
}
export class Test_create_contract_arl_fa2 {
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
        const address = (await ex.deploy("../tests/passed/test_create_contract_arl_fa2.arl", {}, params)).address;
        this.address = address;
    }
    async exec(owner: att.Address, permits: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(owner, permits), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(owner: att.Address, permits: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(owner, permits), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_create_contract_arl_fa2 = new Test_create_contract_arl_fa2();
