import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
import * as el from "@completium/event-listener";
export class Response implements att.ArchetypeType {
    constructor(public responder_addr: att.Address, public poll_id: att.Nat, public response: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.responder_addr.to_mich(), this.poll_id.to_mich(), this.response.to_mich()]);
    }
    equals(v: Response): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): Response {
        return new Response(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.Nat.from_mich((input as att.Mpair).args[2]));
    }
}
export class NewPoll implements att.ArchetypeType {
    constructor(public creator: att.Address, public poll_id: att.Bytes) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.creator.to_mich(), this.poll_id.to_mich()]);
    }
    equals(v: NewPoll): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): NewPoll {
        return new NewPoll(att.Address.from_mich((input as att.Mpair).args[0]), att.Bytes.from_mich((input as att.Mpair).args[1]));
    }
}
export class ApprovePoll implements att.ArchetypeType {
    constructor(public creator: att.Address, public poll_id: att.Bytes) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.creator.to_mich(), this.poll_id.to_mich()]);
    }
    equals(v: ApprovePoll): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): ApprovePoll {
        return new ApprovePoll(att.Address.from_mich((input as att.Mpair).args[0]), att.Bytes.from_mich((input as att.Mpair).args[1]));
    }
}
export const poll_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const poll_to_approve_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("bytes", []);
export const responder_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export class poll_value implements att.ArchetypeType {
    constructor(public ipfs_hash: att.Bytes, public responses: Array<[
        att.Nat,
        att.Nat
    ]>, public creation: Date) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.ipfs_hash.to_mich(), att.list_to_mich(this.responses, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), x_value.to_mich());
            }), att.date_to_mich(this.creation)]);
    }
    equals(v: poll_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): poll_value {
        return new poll_value(att.Bytes.from_mich((input as att.Mpair).args[0]), att.mich_to_map((input as att.Mpair).args[1], (x, y) => [att.Nat.from_mich(x), att.Nat.from_mich(y)]), att.mich_to_date((input as att.Mpair).args[2]));
    }
}
export const poll_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bytes", ["%ipfs_hash"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("nat", []), ["%responses"]),
    att.prim_annot_to_mich_type("timestamp", ["%creation"])
], []);
export const poll_to_approve_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const responder_value_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), []);
export type poll_container = Array<[
    att.Nat,
    poll_value
]>;
export type poll_to_approve_container = Array<[
    att.Bytes,
    att.Address
]>;
export type responder_container = Array<[
    att.Address,
    Array<att.Nat>
]>;
export const poll_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bytes", ["%ipfs_hash"]),
    att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("nat", []), att.prim_annot_to_mich_type("nat", []), ["%responses"]),
    att.prim_annot_to_mich_type("timestamp", ["%creation"])
], []), []);
export const poll_to_approve_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("bytes", []), att.prim_annot_to_mich_type("address", []), []);
export const responder_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("address", []), att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), []), []);
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
const add_poll_arg_to_mich = (h: att.Bytes): att.Micheline => {
    return h.to_mich();
}
const approve_arg_to_mich = (h: att.Bytes): att.Micheline => {
    return h.to_mich();
}
const disapprove_arg_to_mich = (h: att.Bytes): att.Micheline => {
    return h.to_mich();
}
const remove_arg_to_mich = (pk: att.Nat): att.Micheline => {
    return pk.to_mich();
}
const respond_arg_to_mich = (pk: att.Nat, choice_id: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        pk.to_mich(),
        choice_id.to_mich()
    ]);
}
const view_get_responses_arg_to_mich = (pk: att.Nat): att.Micheline => {
    return pk.to_mich();
}
const view_already_responded_arg_to_mich = (pk: att.Nat): att.Micheline => {
    return pk.to_mich();
}
export class Poll {
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
        const address = (await ex.deploy("../tests/contracts/poll/poll.arl", {
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
    async add_poll(h: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "add_poll", add_poll_arg_to_mich(h), params);
        }
        throw new Error("Contract not initialised");
    }
    async approve(h: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "approve", approve_arg_to_mich(h), params);
        }
        throw new Error("Contract not initialised");
    }
    async disapprove(h: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "disapprove", disapprove_arg_to_mich(h), params);
        }
        throw new Error("Contract not initialised");
    }
    async remove(pk: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "remove", remove_arg_to_mich(pk), params);
        }
        throw new Error("Contract not initialised");
    }
    async respond(pk: att.Nat, choice_id: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "respond", respond_arg_to_mich(pk, choice_id), params);
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
    async get_add_poll_param(h: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "add_poll", add_poll_arg_to_mich(h), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_approve_param(h: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "approve", approve_arg_to_mich(h), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_disapprove_param(h: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "disapprove", disapprove_arg_to_mich(h), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_remove_param(pk: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "remove", remove_arg_to_mich(pk), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_respond_param(pk: att.Nat, choice_id: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "respond", respond_arg_to_mich(pk, choice_id), params);
        }
        throw new Error("Contract not initialised");
    }
    async view_get_responses(pk: att.Nat, params: Partial<ex.Parameters>): Promise<Array<[
        att.Nat,
        att.Nat
    ]> | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "get_responses", view_get_responses_arg_to_mich(pk), params);
            return mich.value ? att.mich_to_map(mich.value, (x, y) => [att.Nat.from_mich(x), att.Nat.from_mich(y)]) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async view_already_responded(pk: att.Nat, params: Partial<ex.Parameters>): Promise<boolean | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "already_responded", view_already_responded_arg_to_mich(pk), params);
            return mich.value ? att.mich_to_bool(mich.value) : undefined;
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
    async get_polls_counter(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_poll(): Promise<poll_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[4], (x, y) => [att.Nat.from_mich(x), poll_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_poll_to_approve_value(key: att.Bytes): Promise<att.Address | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[5]).toString()), key.to_mich(), poll_to_approve_key_mich_type);
            if (data != undefined) {
                return att.Address.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_poll_to_approve_value(key: att.Bytes): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[5]).toString()), key.to_mich(), poll_to_approve_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_responder_value(key: att.Address): Promise<Array<att.Nat> | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), responder_key_mich_type);
            if (data != undefined) {
                return att.mich_to_list(data, x => { return att.Nat.from_mich(x); });
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_responder_value(key: att.Address): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), responder_key_mich_type);
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[7]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[7]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    register_Response(ep: el.EventProcessor<Response>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "Response"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return Response.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    register_NewPoll(ep: el.EventProcessor<NewPoll>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "NewPoll"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return NewPoll.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    register_ApprovePoll(ep: el.EventProcessor<ApprovePoll>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "ApprovePoll"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return ApprovePoll.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        f1: att.string_to_mich("\"CANNOT_RESPOND_TWICE\""),
        r3: att.string_to_mich("\"POLL_NOT_FOUND\""),
        r2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r2\"")]),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\""),
        POLL_NOT_FOUND: att.string_to_mich("\"POLL_NOT_FOUND\""),
        r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r1\"")]),
        md_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"md_r1\"")]),
        pausable_r2: att.string_to_mich("\"CONTRACT_NOT_PAUSED\""),
        pausable_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"pausable_r1\"")]),
        ownership_r1: att.string_to_mich("\"INVALID_CALLER\""),
        CONTRACT_PAUSED: att.string_to_mich("\"CONTRACT_PAUSED\"")
    };
}
export const poll = new Poll();
