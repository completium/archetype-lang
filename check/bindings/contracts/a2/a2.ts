import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class transferlist implements att.ArchetypeType {
    constructor(public unrestricted: boolean, public allowedTransferlists: Array<att.Nat>) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([att.bool_to_mich(this.unrestricted), att.list_to_mich(this.allowedTransferlists, x => {
                return x.to_mich();
            })]);
    }
    equals(v: transferlist): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): transferlist {
        return new transferlist(att.mich_to_bool((input as att.Mpair).args[0]), att.mich_to_list((input as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }));
    }
}
export const transferlist_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("bool", ["%unrestricted"]),
    att.set_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), ["%allowedTransferlists"])
], []);
export const whitelister_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export type whitelister_container = Array<att.Address>;
export const whitelister_container_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("address", []), []);
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
const addSuperuser_arg_to_mich = (su: att.Address): att.Micheline => {
    return su.to_mich();
}
const removeSuperuser_arg_to_mich = (su: att.Address): att.Micheline => {
    return su.to_mich();
}
const addWhitelister_arg_to_mich = (v: att.Address): att.Micheline => {
    return v.to_mich();
}
const removeWhitelister_arg_to_mich = (v: att.Address): att.Micheline => {
    return v.to_mich();
}
const assertReceivers_arg_to_mich = (addrs: Array<att.Address>): att.Micheline => {
    return att.list_to_mich(addrs, x => {
        return x.to_mich();
    });
}
const assertTransfers_arg_to_mich = (input_list: Array<[
    att.Address,
    Array<att.Address>
]>): att.Micheline => {
    return att.list_to_mich(input_list, x => {
        return att.pair_to_mich([x[0].to_mich(), att.list_to_mich(x[1], x => {
                return x.to_mich();
            })]);
    });
}
const assertTransferlist_arg_to_mich = (fromTransferListId: att.Nat, toTransferListId: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        fromTransferListId.to_mich(),
        toTransferListId.to_mich()
    ]);
}
const updateUser_arg_to_mich = (user: att.Address, transferlistId: att.Option<att.Nat>): att.Micheline => {
    return att.pair_to_mich([
        user.to_mich(),
        transferlistId.to_mich((x => { return x.to_mich(); }))
    ]);
}
const updateUsers_arg_to_mich = (utl: Array<[
    att.Address,
    att.Option<att.Nat>
]>): att.Micheline => {
    return att.list_to_mich(utl, x => {
        return att.pair_to_mich([x[0].to_mich(), x[1].to_mich((x => { return x.to_mich(); }))]);
    });
}
const updateTransferlist_arg_to_mich = (transferlistId: att.Nat, u: att.Option<[
    boolean,
    Array<att.Nat>
]>): att.Micheline => {
    return att.pair_to_mich([
        transferlistId.to_mich(),
        u.to_mich((x => { return att.pair_to_mich([att.bool_to_mich(x[0]), att.list_to_mich(x[1], x => {
                return x.to_mich();
            })]); }))
    ]);
}
const getUser_arg_to_mich = (user: att.Address): att.Micheline => {
    return user.to_mich();
}
const view_assertTransfer_arg_to_mich = (sender: att.Address, from: att.Address, to: att.Address): att.Micheline => {
    return att.pair_to_mich([
        sender.to_mich(),
        from.to_mich(),
        to.to_mich()
    ]);
}
export const deploy_getUser_callback = async (params: Partial<ex.Parameters>): Promise<att.DeployResult> => {
    return await ex.deploy_callback("getUser", att.option_annot_to_mich_type(att.prim_annot_to_mich_type("nat", []), []), params);
};
export class A2 {
    address: string | undefined;
    constructor(address: string | undefined = undefined) {
        this.address = address;
    }
    getUser_callback_address: string | undefined;
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
    async deploy(owner: att.Address, users: att.Address, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("../tests/contracts/a2/a2.arl", {
            owner: owner.to_mich(),
            users: users.to_mich()
        }, params)).address;
        this.address = address;
        this.getUser_callback_address = (await deploy_getUser_callback(params)).address;
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
    async addSuperuser(su: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "addSuperuser", addSuperuser_arg_to_mich(su), params);
        }
        throw new Error("Contract not initialised");
    }
    async removeSuperuser(su: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "removeSuperuser", removeSuperuser_arg_to_mich(su), params);
        }
        throw new Error("Contract not initialised");
    }
    async addWhitelister(v: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "addWhitelister", addWhitelister_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async removeWhitelister(v: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "removeWhitelister", removeWhitelister_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async assertReceivers(addrs: Array<att.Address>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "assertReceivers", assertReceivers_arg_to_mich(addrs), params);
        }
        throw new Error("Contract not initialised");
    }
    async assertTransfers(input_list: Array<[
        att.Address,
        Array<att.Address>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "assertTransfers", assertTransfers_arg_to_mich(input_list), params);
        }
        throw new Error("Contract not initialised");
    }
    async assertTransferlist(fromTransferListId: att.Nat, toTransferListId: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "assertTransferlist", assertTransferlist_arg_to_mich(fromTransferListId, toTransferListId), params);
        }
        throw new Error("Contract not initialised");
    }
    async updateUser(user: att.Address, transferlistId: att.Option<att.Nat>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "updateUser", updateUser_arg_to_mich(user, transferlistId), params);
        }
        throw new Error("Contract not initialised");
    }
    async updateUsers(utl: Array<[
        att.Address,
        att.Option<att.Nat>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "updateUsers", updateUsers_arg_to_mich(utl), params);
        }
        throw new Error("Contract not initialised");
    }
    async updateTransferlist(transferlistId: att.Nat, u: att.Option<[
        boolean,
        Array<att.Nat>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "updateTransferlist", updateTransferlist_arg_to_mich(transferlistId, u), params);
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
    async get_addSuperuser_param(su: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "addSuperuser", addSuperuser_arg_to_mich(su), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_removeSuperuser_param(su: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "removeSuperuser", removeSuperuser_arg_to_mich(su), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_addWhitelister_param(v: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "addWhitelister", addWhitelister_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_removeWhitelister_param(v: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "removeWhitelister", removeWhitelister_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_assertReceivers_param(addrs: Array<att.Address>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "assertReceivers", assertReceivers_arg_to_mich(addrs), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_assertTransfers_param(input_list: Array<[
        att.Address,
        Array<att.Address>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "assertTransfers", assertTransfers_arg_to_mich(input_list), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_assertTransferlist_param(fromTransferListId: att.Nat, toTransferListId: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "assertTransferlist", assertTransferlist_arg_to_mich(fromTransferListId, toTransferListId), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_updateUser_param(user: att.Address, transferlistId: att.Option<att.Nat>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "updateUser", updateUser_arg_to_mich(user, transferlistId), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_updateUsers_param(utl: Array<[
        att.Address,
        att.Option<att.Nat>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "updateUsers", updateUsers_arg_to_mich(utl), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_updateTransferlist_param(transferlistId: att.Nat, u: att.Option<[
        boolean,
        Array<att.Nat>
    ]>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "updateTransferlist", updateTransferlist_arg_to_mich(transferlistId, u), params);
        }
        throw new Error("Contract not initialised");
    }
    async getUser(user: att.Address, params: Partial<ex.Parameters>): Promise<att.Option<att.Nat>> {
        if (this.address != undefined) {
            if (this.getUser_callback_address != undefined) {
                const entrypoint = new att.Entrypoint(new att.Address(this.getUser_callback_address), "callback");
                await ex.call(this.address, "getUser", att.getter_args_to_mich(getUser_arg_to_mich(user), entrypoint), params);
                return await ex.get_callback_value<att.Option<att.Nat>>(this.getUser_callback_address, x => { return att.Option.from_mich(x, x => { return att.Nat.from_mich(x); }); });
            }
        }
        throw new Error("Contract not initialised");
    }
    async view_assertTransfer(sender: att.Address, from: att.Address, to: att.Address, params: Partial<ex.Parameters>): Promise<string | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "assertTransfer", view_assertTransfer_arg_to_mich(sender, from, to), params);
            return mich.value ? att.mich_to_string(mich.value) : undefined;
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
    async get_users(): Promise<att.Address> {
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
    async get_superusers(): Promise<Array<att.Address>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[4], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_whitelister(): Promise<whitelister_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list((storage as att.Mpair).args[5], x => { return att.Address.from_mich(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_transferlists_value(key: att.Nat): Promise<transferlist | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), att.prim_annot_to_mich_type("nat", []));
            if (data != undefined) {
                return transferlist.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_transferlists_value(key: att.Nat): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[6]).toString()), key.to_mich(), att.prim_annot_to_mich_type("nat", []));
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
    errors = {
        r2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r2\"")]),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\""),
        r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r1\"")]),
        r0: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r0\"")]),
        TO_TRANSFERLIST_NOT_FOUND_IN_FROM: att.string_to_mich("\"TO_TRANSFERLIST_NOT_FOUND_IN_FROM\""),
        TO_INVALID_UNRESTRICTED_STATE: att.string_to_mich("\"TO_INVALID_UNRESTRICTED_STATE\""),
        FROM_INVALID_UNRESTRICTED_STATE: att.string_to_mich("\"FROM_INVALID_UNRESTRICTED_STATE\""),
        TO_TRANSFERLIST_NOT_FOUND: att.string_to_mich("\"TO_TRANSFERLIST_NOT_FOUND\""),
        FROM_TRANSFERLIST_NOT_FOUND: att.string_to_mich("\"FROM_TRANSFERLIST_NOT_FOUND\""),
        INTERNAL_ERROR: att.string_to_mich("\"INTERNAL_ERROR\""),
        USER_RESTRICTED: att.string_to_mich("\"USER_RESTRICTED\""),
        NO_TRANSFER: att.string_to_mich("\"NO_TRANSFER\""),
        md_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"md_r1\"")]),
        pausable_r2: att.string_to_mich("\"CONTRACT_NOT_PAUSED\""),
        pausable_r1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"pausable_r1\"")]),
        ownership_r1: att.string_to_mich("\"INVALID_CALLER\""),
        USER_GETOPT_INTERNAL_ERROR: att.string_to_mich("\"USER_GETOPT_INTERNAL_ERROR\""),
        CONTRACT_PAUSED: att.string_to_mich("\"CONTRACT_PAUSED\"")
    };
}
export const a2 = new A2();
