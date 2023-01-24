import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum states {
    Starting = 1,
    Running,
    Paused
}
export const mich_to_state = (m: any): states => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return states.Starting;
        case 1: return states.Running;
        case 2: return states.Paused;
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export class rec_to_sign_propose_feeless implements att.ArchetypeType {
    constructor(public pf_pkh: att.Address, public pf_counter: att.Nat, public pf_entry: string, public pf_lambda: att.Micheline, public pf_validity: att.Duration) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.pf_pkh.to_mich(), this.pf_counter.to_mich(), att.string_to_mich(this.pf_entry), (x => x)(this.pf_lambda), this.pf_validity.to_mich()]);
    }
    equals(v: rec_to_sign_propose_feeless): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): rec_to_sign_propose_feeless {
        return new rec_to_sign_propose_feeless(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.mich_to_string((input as att.Mpair).args[2]), (input as att.Mpair).args[3], att.Duration.from_mich((input as att.Mpair).args[4]));
    }
}
export class rec_to_sign_approve_feeless implements att.ArchetypeType {
    constructor(public af_pkh: att.Address, public af_counter: att.Nat, public af_entry: string, public af_proposal_id: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.af_pkh.to_mich(), this.af_counter.to_mich(), att.string_to_mich(this.af_entry), this.af_proposal_id.to_mich()]);
    }
    equals(v: rec_to_sign_approve_feeless): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): rec_to_sign_approve_feeless {
        return new rec_to_sign_approve_feeless(att.Address.from_mich((input as att.Mpair).args[0]), att.Nat.from_mich((input as att.Mpair).args[1]), att.mich_to_string((input as att.Mpair).args[2]), att.Nat.from_mich((input as att.Mpair).args[3]));
    }
}
export const rec_to_sign_propose_feeless_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%pf_pkh"]),
    att.prim_annot_to_mich_type("nat", ["%pf_counter"]),
    att.prim_annot_to_mich_type("string", ["%pf_entry"]),
    att.pair_annot_to_mich_type("lambda", att.prim_annot_to_mich_type("unit", []), att.list_annot_to_mich_type(att.prim_annot_to_mich_type("operation", []), []), ["%pf_lambda"]),
    att.prim_annot_to_mich_type("int", ["%pf_validity"])
], []);
export const rec_to_sign_approve_feeless_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("address", ["%af_pkh"]),
    att.prim_annot_to_mich_type("nat", ["%af_counter"]),
    att.prim_annot_to_mich_type("string", ["%af_entry"]),
    att.prim_annot_to_mich_type("nat", ["%af_proposal_id"])
], []);
export const manager_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export const pending_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export class pending_value implements att.ArchetypeType {
    constructor(public expiration: Date, public approvals: Array<att.Address>, public actions: att.Micheline) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.date_to_mich(this.expiration), att.list_to_mich(this.approvals, x => {
                return x.to_mich();
            }), (x => x)(this.actions)]);
    }
    equals(v: pending_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): pending_value {
        return new pending_value(att.mich_to_date((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Address.from_mich(x); }), (input as att.Mpair).args[2]);
    }
}
export const manager_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export const pending_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%expiration"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), ["%approvals"]),
    att.pair_annot_to_mich_type("lambda", att.prim_annot_to_mich_type("unit", []), att.list_annot_to_mich_type(att.prim_annot_to_mich_type("operation", []), []), ["%actions"])
], []);
export type manager_container = Array<[
    att.Address,
    att.Nat
]>;
export type pending_container = Array<[
    att.Nat,
    pending_value
]>;
export const manager_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.prim_annot_to_mich_type("nat", []), []);
export const pending_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("nat", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("timestamp", ["%expiration"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), ["%approvals"]),
    att.pair_annot_to_mich_type("lambda", att.prim_annot_to_mich_type("unit", []), att.list_annot_to_mich_type(att.prim_annot_to_mich_type("operation", []), []), ["%actions"])
], []), []);
const declare_ownership_arg_to_mich = (candidate: att.Address): att.Micheline => {
    return candidate.to_mich();
}
const claim_ownership_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const set_metadata_uri_arg_to_mich = (idata: att.Bytes): att.Micheline => {
    return idata.to_mich();
}
const pause_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const approve_unpause_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const unpause_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const control_arg_to_mich = (maddr: att.Address, allowed: boolean): att.Micheline => {
    return att.pair_to_mich([
        maddr.to_mich(),
        att.bool_to_mich(allowed)
    ]);
}
const run_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const require_arg_to_mich = (new_required: att.Nat): att.Micheline => {
    return new_required.to_mich();
}
const set_duration_arg_to_mich = (min: att.Duration, max: att.Duration): att.Micheline => {
    return att.pair_to_mich([
        min.to_mich(),
        max.to_mich()
    ]);
}
const do_propose_arg_to_mich = (actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, sender: att.Address): att.Micheline => {
    return att.pair_to_mich([
        (x => x)(actions_to_exec),
        validity.to_mich(),
        att.bool_to_mich(approved_by_caller),
        sender.to_mich()
    ]);
}
const do_approve_arg_to_mich = (proposal_id: att.Nat, sender: att.Address): att.Micheline => {
    return att.pair_to_mich([
        proposal_id.to_mich(),
        sender.to_mich()
    ]);
}
const propose_arg_to_mich = (actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean): att.Micheline => {
    return att.pair_to_mich([
        (x => x)(actions_to_exec),
        validity.to_mich(),
        att.bool_to_mich(approved_by_caller)
    ]);
}
const approve_arg_to_mich = (proposal_id: att.Nat): att.Micheline => {
    return proposal_id.to_mich();
}
const execute_arg_to_mich = (proposal_id: att.Nat): att.Micheline => {
    return proposal_id.to_mich();
}
const propose_feeless_arg_to_mich = (actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, manager_key: att.Key, sig: att.Signature): att.Micheline => {
    return att.pair_to_mich([
        (x => x)(actions_to_exec),
        validity.to_mich(),
        att.bool_to_mich(approved_by_caller),
        manager_key.to_mich(),
        sig.to_mich()
    ]);
}
const approve_feeless_arg_to_mich = (proposal_id: att.Nat, manager_key: att.Key, sig: att.Signature): att.Micheline => {
    return att.pair_to_mich([
        proposal_id.to_mich(),
        manager_key.to_mich(),
        sig.to_mich()
    ]);
}
const get_manager_counter_arg_to_mich = (pkh: att.Address): att.Micheline => {
    return pkh.to_mich();
}
const get_approvals_arg_to_mich = (proposal_id: att.Nat): att.Micheline => {
    return proposal_id.to_mich();
}
export const deploy_get_manager_counter_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("get_manager_counter", att.prim_annot_to_mich_type("nat", []), params);
};
export const deploy_get_approvals_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("get_approvals", att.set_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), []), params);
};
export class Multisig {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    get_manager_counter_callback_address: string | undefined;
    get_approvals_callback_address: string | undefined;
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
    async deploy(owner: att.Address, required: att.Nat, max_duration: att.Duration, min_duration: att.Duration, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/multisig/multisig.arl", {
            owner: owner.to_mich(),
            required: required.to_mich(),
            max_duration: max_duration.to_mich(),
            min_duration: min_duration.to_mich()
        }, params)).address;
        this.address = address;
        this.get_manager_counter_callback_address = (await deploy_get_manager_counter_callback(params)).address;
        this.get_approvals_callback_address = (await deploy_get_approvals_callback(params)).address;
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
    async set_metadata_uri(idata: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_metadata_uri", set_metadata_uri_arg_to_mich(idata), params);
        }
        throw new Error("Contract not initialised");
    }
    async pause(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "pause", pause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async approve_unpause(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "approve_unpause", approve_unpause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async unpause(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "unpause", unpause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async control(maddr: att.Address, allowed: boolean, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "control", control_arg_to_mich(maddr, allowed), params);
        }
        throw new Error("Contract not initialised");
    }
    async run(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "run", run_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async require(new_required: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "require", require_arg_to_mich(new_required), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_duration(min: att.Duration, max: att.Duration, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_duration", set_duration_arg_to_mich(min, max), params);
        }
        throw new Error("Contract not initialised");
    }
    async do_propose(actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, sender: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "do_propose", do_propose_arg_to_mich(actions_to_exec, validity, approved_by_caller, sender), params);
        }
        throw new Error("Contract not initialised");
    }
    async do_approve(proposal_id: att.Nat, sender: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "do_approve", do_approve_arg_to_mich(proposal_id, sender), params);
        }
        throw new Error("Contract not initialised");
    }
    async propose(actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "propose", propose_arg_to_mich(actions_to_exec, validity, approved_by_caller), params);
        }
        throw new Error("Contract not initialised");
    }
    async approve(proposal_id: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "approve", approve_arg_to_mich(proposal_id), params);
        }
        throw new Error("Contract not initialised");
    }
    async execute(proposal_id: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "execute", execute_arg_to_mich(proposal_id), params);
        }
        throw new Error("Contract not initialised");
    }
    async propose_feeless(actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, manager_key: att.Key, sig: att.Signature, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "propose_feeless", propose_feeless_arg_to_mich(actions_to_exec, validity, approved_by_caller, manager_key, sig), params);
        }
        throw new Error("Contract not initialised");
    }
    async approve_feeless(proposal_id: att.Nat, manager_key: att.Key, sig: att.Signature, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "approve_feeless", approve_feeless_arg_to_mich(proposal_id, manager_key, sig), params);
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
    async get_set_metadata_uri_param(idata: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_metadata_uri", set_metadata_uri_arg_to_mich(idata), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_pause_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "pause", pause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_approve_unpause_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "approve_unpause", approve_unpause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_unpause_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "unpause", unpause_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_control_param(maddr: att.Address, allowed: boolean, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "control", control_arg_to_mich(maddr, allowed), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_run_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "run", run_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_require_param(new_required: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "require", require_arg_to_mich(new_required), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_duration_param(min: att.Duration, max: att.Duration, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_duration", set_duration_arg_to_mich(min, max), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_do_propose_param(actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, sender: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "do_propose", do_propose_arg_to_mich(actions_to_exec, validity, approved_by_caller, sender), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_do_approve_param(proposal_id: att.Nat, sender: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "do_approve", do_approve_arg_to_mich(proposal_id, sender), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_propose_param(actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "propose", propose_arg_to_mich(actions_to_exec, validity, approved_by_caller), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_approve_param(proposal_id: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "approve", approve_arg_to_mich(proposal_id), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_execute_param(proposal_id: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "execute", execute_arg_to_mich(proposal_id), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_propose_feeless_param(actions_to_exec: att.Micheline, validity: att.Duration, approved_by_caller: boolean, manager_key: att.Key, sig: att.Signature, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "propose_feeless", propose_feeless_arg_to_mich(actions_to_exec, validity, approved_by_caller, manager_key, sig), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_approve_feeless_param(proposal_id: att.Nat, manager_key: att.Key, sig: att.Signature, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "approve_feeless", approve_feeless_arg_to_mich(proposal_id, manager_key, sig), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_manager_counter(pkh: att.Address, params: Partial<ex.Parameters>): Promise<att.Nat> {
        if (this.address != undefined) {
            if (this.get_manager_counter_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.get_manager_counter_callback_address), "callback");
                await ex.call(this.address, "get_manager_counter", att.getter_args_to_mich(get_manager_counter_arg_to_mich(pkh), entrypoint), params);
                return await ex.get_callback_value<att.Nat>(this.get_manager_counter_callback_address, x => { return att.Nat.from_mich(x); });
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_approvals(proposal_id: att.Nat, params: Partial<ex.Parameters>): Promise<Array<att.Address>> {
        if (this.address != undefined) {
            if (this.get_approvals_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.get_approvals_callback_address), "callback");
                await ex.call(this.address, "get_approvals", att.getter_args_to_mich(get_approvals_arg_to_mich(proposal_id), entrypoint), params);
                return await ex.get_callback_value<Array<att.Address>>(this.get_approvals_callback_address, x => { return att.mich_to_list(x, x => { return att.Address.from_mich(x); }); });
            }
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
    async get_required(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_max_duration(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_min_duration(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_id_count(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[4]);
        }
        throw new Error("Contract not initialised");
    }
    async get_manager(): Promise<manager_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[5], (x, y) => [att.Address.from_mich(x), att.Nat.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_pending_value(key: att.Nat): Promise<pending_value | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), pending_key_mich_type);
            if (data != undefined) {
                return pending_value.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_pending_value(key: att.Nat): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), pending_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_owner_candidate(): Promise<att.Option<att.Address>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[8], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_approve_unpause_set(): Promise<Array<att.Address>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[9], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_metadata_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[10]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[10]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_state(): Promise<states> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const state = (storage as att.Mpair).args[7];
            switch (att.Int.from_mich(state).to_number()) {
                case 0: return states.Starting;
                case 1: return states.Running;
                case 2: return states.Paused;
            }
        }
        return states.Starting;
    }
    errors = {
        r10: att.string_to_mich("\"INVALID_SIGNATURE\""),
        INVALID_STATE: att.string_to_mich("\"INVALID_STATE\""),
        r9: att.string_to_mich("\"INVALID_SIGNATURE\""),
        r2: att.string_to_mich("\"NOT_APPROVED\""),
        r1: att.string_to_mich("\"EXPIRED_PROPOSAL\""),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\""),
        PROPOSAL_NOT_FOUND: att.string_to_mich("\"PROPOSAL_NOT_FOUND\""),
        r3: att.string_to_mich("\"WRONG_DURATION\""),
        r7: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r7\"")]),
        r0: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r0\"")]),
        r5: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r5\"")]),
        r4: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r4\"")]),
        ownership_r1: att.string_to_mich("\"INVALID_CALLER\"")
    };
}
export const multisig = new Multisig();
