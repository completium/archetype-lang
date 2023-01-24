import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum consumer_op_types {
    add = "add",
    remove = "remove"
}
export abstract class consumer_op extends att.Enum<consumer_op_types> {
    abstract to_mich(): att.Micheline;
    equals(v: consumer_op): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class add extends consumer_op {
    constructor(private content: att.Address) {
        super(consumer_op_types.add);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class remove extends consumer_op {
    constructor(private content: att.Address) {
        super(consumer_op_types.remove);
    }
    to_mich() { return att.right_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_consumer_op = (m: att.Micheline): consumer_op => {
    if ((m as att.Msingle).prim == "Left") {
        return new add(att.Address.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new remove(att.Address.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_consumer_op : invalid micheline");
};
export class user_permit implements att.ArchetypeType {
    constructor(public expiry: att.Option<att.Nat>, public created_at: Date) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.expiry.to_mich((x => { return x.to_mich(); })), att.date_to_mich(this.created_at)]);
    }
    equals(v: user_permit): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): user_permit {
        return new user_permit(att.Option.from_mich((input as att.Mpair).args[0], x => { return att.Nat.from_mich(x); }), att.mich_to_date((input as att.Mpair).args[1]));
    }
}
export class rec_to_sign_permit_data implements att.ArchetypeType {
    constructor(public s_addr_contract: att.Address, public s_chainid: att.Chain_id, public s_counter: att.Nat, public s_data: att.Bytes) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.pair_to_mich([this.s_addr_contract.to_mich(), this.s_chainid.to_mich()]), att.pair_to_mich([this.s_counter.to_mich(), this.s_data.to_mich()])]);
    }
    equals(v: rec_to_sign_permit_data): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): rec_to_sign_permit_data {
        return new rec_to_sign_permit_data(att.Address.from_mich(((input as att.Mpair).args[0] as att.Mpair).args[0]), att.Chain_id.from_mich(((input as att.Mpair).args[0] as att.Mpair).args[1]), att.Nat.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[0]), att.Bytes.from_mich((att.pair_to_mich((input as att.Mpair as att.Mpair).args.slice(1, 3)) as att.Mpair).args[1]));
    }
}
export const user_permit_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.option_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%expiry"]),
    att.prim_annot_to_mich_type("timestamp", ["%created_at"])
], []);
export const rec_to_sign_permit_data_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("address", ["%addr"]),
        att.prim_annot_to_mich_type("chain_id", ["%chain_id"])
    ], []),
    att.pair_array_to_mich_type([
        att.prim_annot_to_mich_type("nat", ["%counter"]),
        att.prim_annot_to_mich_type("bytes", ["%data"])
    ], [])
], []);
export const consumer_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const permits_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export class permits_value implements att.ArchetypeType {
    constructor(public counter: att.Nat, public user_expiry: att.Option<att.Nat>, public user_permits: Array<[
        att.Bytes,
        user_permit
    ]>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.counter.to_mich(), this.user_expiry.to_mich((x => { return x.to_mich(); })), att.list_to_mich(this.user_permits, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), x_value.to_mich());
            })]);
    }
    equals(v: permits_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): permits_value {
        return new permits_value(att.Nat.from_mich((input as att.Mpair).args[0]), att.Option.from_mich((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }), att.mich_to_map((input as att.Mpair).args[2], (x, y) => [att.Bytes.from_mich(x), user_permit.from_mich(y)]));
    }
}
export const permits_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%counter"]),
    att.option_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%user_expiry"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("bytes", []), att.pair_array_to_mich_type([
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%expiry"]),
        att.prim_annot_to_mich_type("timestamp", ["%created_at"])
    ], []), ["%user_permits"])
], []);
export type consumer_container = Array<att.Address>;
export type permits_container = Array<[
    att.Address,
    permits_value
]>;
export const consumer_container_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), []);
export const permits_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("address", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%counter"]),
    att.option_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%user_expiry"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("bytes", []), att.pair_array_to_mich_type([
        att.option_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%expiry"]),
        att.prim_annot_to_mich_type("timestamp", ["%created_at"])
    ], []), ["%user_permits"])
], []), []);
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
const manage_consumer_arg_to_mich = (op: consumer_op): att.Micheline => {
    return op.to_mich();
}
const set_expiry_arg_to_mich = (iv: att.Option<att.Nat>, ip: att.Option<att.Bytes>): att.Micheline => {
    return att.pair_to_mich([
        iv.to_mich((x => { return x.to_mich(); })),
        ip.to_mich((x => { return x.to_mich(); }))
    ]);
}
const setExpiry_arg_to_mich = (u: att.Address, sec: att.Nat, data: att.Option<att.Bytes>): att.Micheline => {
    return att.pair_to_mich([
        u.to_mich(),
        sec.to_mich(),
        data.to_mich((x => { return x.to_mich(); }))
    ]);
}
const set_default_expiry_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const permit_arg_to_mich = (signer: att.Key, sig: att.Signature, permit_key: att.Bytes): att.Micheline => {
    return att.pair_to_mich([
        signer.to_mich(),
        sig.to_mich(),
        permit_key.to_mich()
    ]);
}
const consume_arg_to_mich = (user: att.Address, permit_key: att.Bytes, err: string): att.Micheline => {
    return att.pair_to_mich([
        user.to_mich(),
        permit_key.to_mich(),
        att.string_to_mich(err)
    ]);
}
const check_arg_to_mich = (signer: att.Key, sig: att.Signature, data: att.Bytes): att.Micheline => {
    return att.pair_to_mich([
        signer.to_mich(),
        sig.to_mich(),
        data.to_mich()
    ]);
}
export class Permits {
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
    async deploy(owner: att.Address, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/fa2/permits.arl", {
            owner: owner.to_mich()
        }, params)).address;
        this.address = address;
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
    async manage_consumer(op: consumer_op, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "manage_consumer", manage_consumer_arg_to_mich(op), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_expiry(iv: att.Option<att.Nat>, ip: att.Option<att.Bytes>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_expiry", set_expiry_arg_to_mich(iv, ip), params);
        }
        throw new Error("Contract not initialised");
    }
    async setExpiry(u: att.Address, sec: att.Nat, data: att.Option<att.Bytes>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "setExpiry", setExpiry_arg_to_mich(u, sec, data), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_default_expiry(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_default_expiry", set_default_expiry_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async permit(signer: att.Key, sig: att.Signature, permit_key: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "permit", permit_arg_to_mich(signer, sig, permit_key), params);
        }
        throw new Error("Contract not initialised");
    }
    async consume(user: att.Address, permit_key: att.Bytes, err: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "consume", consume_arg_to_mich(user, permit_key, err), params);
        }
        throw new Error("Contract not initialised");
    }
    async check(signer: att.Key, sig: att.Signature, data: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "check", check_arg_to_mich(signer, sig, data), params);
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
    async get_manage_consumer_param(op: consumer_op, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "manage_consumer", manage_consumer_arg_to_mich(op), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_expiry_param(iv: att.Option<att.Nat>, ip: att.Option<att.Bytes>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_expiry", set_expiry_arg_to_mich(iv, ip), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_setExpiry_param(u: att.Address, sec: att.Nat, data: att.Option<att.Bytes>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "setExpiry", setExpiry_arg_to_mich(u, sec, data), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_default_expiry_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_default_expiry", set_default_expiry_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_permit_param(signer: att.Key, sig: att.Signature, permit_key: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "permit", permit_arg_to_mich(signer, sig, permit_key), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_consume_param(user: att.Address, permit_key: att.Bytes, err: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "consume", consume_arg_to_mich(user, permit_key, err), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_check_param(signer: att.Key, sig: att.Signature, data: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "check", check_arg_to_mich(signer, sig, data), params);
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
    async get_owner_candidate(): Promise<att.Option<att.Address>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[1], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_paused(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_consumer(): Promise<consumer_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[3], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_permits_value(key: att.Address): Promise<permits_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[4]).toString()), key.to_mich(), permits_key_mich_type);
            if (data != undefined) {
                return permits_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_permits_value(key: att.Address): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[4]).toString()), key.to_mich(), permits_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_default_expiry(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[5]);
        }
        throw new Error("Contract not initialised");
    }
    async get_metadata_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
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
        p10: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"p10\"")]),
        NO_TRANSFER: att.string_to_mich("\"NO_TRANSFER\""),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\""),
        p8: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"p8\"")]),
        p7: att.string_to_mich("\"MAX_PERMITS_REACHED\""),
        p4: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"p4\"")]),
        r3: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r3\"")]),
        r5: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r5\"")]),
        r2: att.string_to_mich("\"EXPIRY_TOO_BIG\""),
        r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r1\"")]),
        md_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"md_r1\"")]),
        pausable_r2: att.string_to_mich("\"CONTRACT_NOT_PAUSED\""),
        pausable_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"pausable_r1\"")]),
        ownership_r1: att.string_to_mich("\"INVALID_CALLER\""),
        CONTRACT_PAUSED: att.string_to_mich("\"CONTRACT_PAUSED\"")
    };
}
export const permits = new Permits();
