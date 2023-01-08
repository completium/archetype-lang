import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum token_type_types {
    FT = "FT",
    FA1_2 = "FA1_2",
    NFT = "NFT"
}
export abstract class token_type extends att.Enum<token_type_types> {
    abstract to_mich(): att.Micheline;
    equals(v: token_type): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class FT extends token_type {
    constructor() {
        super(token_type_types.FT);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class FA1_2 extends token_type {
    constructor() {
        super(token_type_types.FA1_2);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class NFT extends token_type {
    constructor() {
        super(token_type_types.NFT);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export enum operator_transfer_policy_types {
    No_transfer = "No_transfer",
    Owner_transfer = "Owner_transfer",
    Owner_or_operator_transfer = "Owner_or_operator_transfer"
}
export abstract class operator_transfer_policy extends att.Enum<operator_transfer_policy_types> {
    abstract to_mich(): att.Micheline;
    equals(v: operator_transfer_policy): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class No_transfer extends operator_transfer_policy {
    constructor() {
        super(operator_transfer_policy_types.No_transfer);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Owner_transfer extends operator_transfer_policy {
    constructor() {
        super(operator_transfer_policy_types.Owner_transfer);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Owner_or_operator_transfer extends operator_transfer_policy {
    constructor() {
        super(operator_transfer_policy_types.Owner_or_operator_transfer);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export enum owner_hook_policy_types {
    Owner_no_hook = "Owner_no_hook",
    Optional_owner_hook = "Optional_owner_hook",
    Required_owner_hook = "Required_owner_hook"
}
export abstract class owner_hook_policy extends att.Enum<owner_hook_policy_types> {
    abstract to_mich(): att.Micheline;
    equals(v: owner_hook_policy): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class Owner_no_hook extends owner_hook_policy {
    constructor() {
        super(owner_hook_policy_types.Owner_no_hook);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Optional_owner_hook extends owner_hook_policy {
    constructor() {
        super(owner_hook_policy_types.Optional_owner_hook);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Required_owner_hook extends owner_hook_policy {
    constructor() {
        super(owner_hook_policy_types.Required_owner_hook);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export enum owner_type_types {
    Sender = "Sender",
    Receiver = "Receiver"
}
export abstract class owner_type extends att.Enum<owner_type_types> {
    abstract to_mich(): att.Micheline;
    equals(v: owner_type): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class Sender extends owner_type {
    constructor() {
        super(owner_type_types.Sender);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Receiver extends owner_type {
    constructor() {
        super(owner_type_types.Receiver);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_token_type = (m: any): token_type => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new FT();
        case 1: return new FA1_2();
        case 2: return new NFT();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const mich_to_operator_transfer_policy = (m: any): operator_transfer_policy => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new No_transfer();
        case 1: return new Owner_transfer();
        case 2: return new Owner_or_operator_transfer();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const mich_to_owner_hook_policy = (m: any): owner_hook_policy => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new Owner_no_hook();
        case 1: return new Optional_owner_hook();
        case 2: return new Required_owner_hook();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const mich_to_owner_type = (m: any): owner_type => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new Sender();
        case 1: return new Receiver();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export class basic_permissions implements att.ArchetypeType {
    constructor(public transfer_policy: operator_transfer_policy, public sender_hook_policy: owner_hook_policy, public receiver_hook_policy: owner_hook_policy) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.transfer_policy.to_mich(), this.sender_hook_policy.to_mich(), this.receiver_hook_policy.to_mich()]);
    }
    equals(v: basic_permissions): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): basic_permissions {
        return new basic_permissions(mich_to_operator_transfer_policy((input as att.Mpair).args[0]), mich_to_owner_hook_policy((input as att.Mpair).args[1]), mich_to_owner_hook_policy((input as att.Mpair).args[2]));
    }
}
export class custom_policy implements att.ArchetypeType {
    constructor(public tag: string, public config_api: att.Option<att.Address>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.string_to_mich(this.tag), this.config_api.to_mich((x => { return x.to_mich(); }))]);
    }
    equals(v: custom_policy): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): custom_policy {
        return new custom_policy(att.mich_to_string((input as att.Mpair).args[0]), att.Option.from_mich((input as att.Mpair).args[1], x => { return att.Address.from_mich(x); }));
    }
}
export class permissions implements att.ArchetypeType {
    constructor(public basic: basic_permissions, public custom: att.Option<custom_policy>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.basic.to_mich(), this.custom.to_mich((x => { return x.to_mich(); }))]);
    }
    equals(v: permissions): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): permissions {
        return new permissions(basic_permissions.from_mich((input as att.Mpair).args[0]), att.Option.from_mich((input as att.Mpair).args[1], x => { return custom_policy.from_mich(x); }));
    }
}
export const basic_permissions_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%transfer_policy"]),
    att.prim_annot_to_mich_type("int", ["%sender_hook_policy"]),
    att.prim_annot_to_mich_type("int", ["%receiver_hook_policy"])
], []);
export const custom_policy_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("string", ["%tag"]),
    att.option_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), ["%config_api"])
], []);
export const permissions_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("int", ["%transfer_policy"]),
        att.prim_annot_to_mich_type("int", ["%sender_hook_policy"]),
        att.prim_annot_to_mich_type("int", ["%receiver_hook_policy"])
    ], ["%basic"]),
    att.option_annot_to_mich_type(att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("string", ["%tag"]),
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), ["%config_api"])
    ], []), ["%custom"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Test_fun8 {
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
        const address = (await ex.deploy("../tests/passed/test_fun8.arl", {}, params)).address;
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
    async get_permissions_descriptor(): Promise<permissions> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return permissions.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_fun8 = new Test_fun8();
