import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const getAllowance_arg_to_mich = (owner: att.Address, spender: att.Address, callback: att.Entrypoint): att.Micheline => {
    return att.pair_to_mich([
        owner.to_mich(),
        spender.to_mich(),
        callback.to_mich()
    ]);
}
const getBalance_arg_to_mich = (owner: att.Address, callback: att.Entrypoint): att.Micheline => {
    return att.pair_to_mich([
        owner.to_mich(),
        callback.to_mich()
    ]);
}
const getTotalSupply_arg_to_mich = (x224: att.Unit, callback: att.Entrypoint): att.Micheline => {
    return att.pair_to_mich([
        att.unit_to_mich(),
        callback.to_mich()
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
const transfer_arg_to_mich = (from: att.Address, to: att.Address, value: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        from.to_mich(),
        to.to_mich(),
        value.to_mich()
    ]);
}
const approve_arg_to_mich = (spender: att.Address, value: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        spender.to_mich(),
        value.to_mich()
    ]);
}
export class Fa1_2 {
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
    async deploy(ledger: Array<[
        att.Address,
        [
            att.Nat,
            Array<[
                att.Address,
                att.Nat
            ]>
        ]
    ]>, token_metadata: Array<[
        att.Nat,
        [
            att.Nat,
            Array<[
                string,
                att.Bytes
            ]>
        ]
    ]>, params: Partial<ex.Parameters>) {
        const address = (await ex.deploy("./contracts/fa1_2.arl", {
            ledger: att.list_to_mich(ledger, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), att.pair_to_mich([x_value[0].to_mich(), att.list_to_mich(x_value[1], x => {
                        const x_key = x[0];
                        const x_value = x[1];
                        return att.elt_to_mich(x_key.to_mich(), x_value.to_mich());
                    })]));
            }),
            token_metadata: att.list_to_mich(token_metadata, x => {
                const x_key = x[0];
                const x_value = x[1];
                return att.elt_to_mich(x_key.to_mich(), att.pair_to_mich([x_value[0].to_mich(), att.list_to_mich(x_value[1], x => {
                        const x_key = x[0];
                        const x_value = x[1];
                        return att.elt_to_mich(att.string_to_mich(x_key), x_value.to_mich());
                    })]));
            })
        }, params)).address;
        this.address = address;
    }
    async getAllowance(owner: att.Address, spender: att.Address, callback: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "getAllowance", getAllowance_arg_to_mich(owner, spender, callback), params);
        }
        throw new Error("Contract not initialised");
    }
    async getBalance(owner: att.Address, callback: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "getBalance", getBalance_arg_to_mich(owner, callback), params);
        }
        throw new Error("Contract not initialised");
    }
    async getTotalSupply(x224: att.Unit, callback: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "getTotalSupply", getTotalSupply_arg_to_mich(x224, callback), params);
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
    async transfer(from: att.Address, to: att.Address, value: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "transfer", transfer_arg_to_mich(from, to, value), params);
        }
        throw new Error("Contract not initialised");
    }
    async approve(spender: att.Address, value: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "approve", approve_arg_to_mich(spender, value), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_getAllowance_param(owner: att.Address, spender: att.Address, callback: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "getAllowance", getAllowance_arg_to_mich(owner, spender, callback), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_getBalance_param(owner: att.Address, callback: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "getBalance", getBalance_arg_to_mich(owner, callback), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_getTotalSupply_param(x224: att.Unit, callback: att.Entrypoint, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "getTotalSupply", getTotalSupply_arg_to_mich(x224, callback), params);
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
    async get_transfer_param(from: att.Address, to: att.Address, value: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "transfer", transfer_arg_to_mich(from, to, value), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_approve_param(spender: att.Address, value: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "approve", approve_arg_to_mich(spender, value), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_ledger_value(key: att.Address): Promise<[
        att.Nat,
        Array<[
            att.Address,
            att.Nat
        ]>
    ] | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[0]).toString()), key.to_mich(), att.prim_annot_to_mich_type("address", []));
            if (data != undefined) {
                return (p => {
                    return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_map((p as att.Mpair).args[1], (x, y) => [att.Address.from_mich(x), att.Nat.from_mich(y)])];
                })(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_ledger_value(key: att.Address): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[0]).toString()), key.to_mich(), att.prim_annot_to_mich_type("address", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_token_metadata_value(key: att.Nat): Promise<[
        att.Nat,
        Array<[
            string,
            att.Bytes
        ]>
    ] | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), key.to_mich(), att.prim_annot_to_mich_type("nat", []));
            if (data != undefined) {
                return (p => {
                    return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_map((p as att.Mpair).args[1], (x, y) => [att.mich_to_string(x), att.Bytes.from_mich(y)])];
                })(data);
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
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((storage as att.Mpair).args[1]).toString()), key.to_mich(), att.prim_annot_to_mich_type("nat", []));
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
        ASSET_NOT_FOUND_LEDGER: att.pair_to_mich([att.string_to_mich("\"ASSET_NOT_FOUND\""), att.string_to_mich("\"ledger\"")]),
        NAT_NEG_ASSIGN: att.string_to_mich("\"NAT_NEG_ASSIGN\""),
        NOTENOUGHBALANCE: att.string_to_mich("\"NotEnoughBalance\""),
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\"")
    };
}
export const fa1_2 = new Fa1_2();
