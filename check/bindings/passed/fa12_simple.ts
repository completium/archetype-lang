import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const ledger_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("int", []);
export const ledger_value_mich_type: att.MichelineType = att.prim_annot_to_mich_type("nat", []);
export type ledger_container = Array<[
    att.Int,
    att.Nat
]>;
export const ledger_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("big_map", att.prim_annot_to_mich_type("int", []), att.prim_annot_to_mich_type("nat", []), []);
const transfer_arg_to_mich = (from_: att.Int, to_: att.Int, value: att.Nat): att.Micheline => {
    return att.pair_to_mich([
        from_.to_mich(),
        to_.to_mich(),
        value.to_mich()
    ]);
}
export class Fa12_simple {
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
        const address = (await ex.deploy("../tests/passed/fa12_simple.arl", {}, params)).address;
        this.address = address;
    }
    async transfer(from_: att.Int, to_: att.Int, value: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "transfer", transfer_arg_to_mich(from_, to_, value), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_transfer_param(from_: att.Int, to_: att.Int, value: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "transfer", transfer_arg_to_mich(from_, to_, value), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_ledger_value(key: att.Int): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), key.to_mich(), ledger_key_mich_type);
            if (data != undefined) {
                return att.Nat.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_ledger_value(key: att.Int): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), key.to_mich(), ledger_key_mich_type);
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const fa12_simple = new Fa12_simple();
