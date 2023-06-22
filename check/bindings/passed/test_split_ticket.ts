import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const create_arg_to_mich = (amount: att.Nat): att.Micheline => {
    return amount.to_mich();
}
const split_arg_to_mich = (amount: att.Nat, destination: att.Address): att.Micheline => {
    return att.pair_to_mich([
        amount.to_mich(),
        destination.to_mich()
    ]);
}
export class Test_split_ticket {
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
        const address = (await ex.deploy("../tests/passed/test_split_ticket.arl", {}, params)).address;
        this.address = address;
    }
    async create(amount: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "create", create_arg_to_mich(amount), params);
        }
        throw new Error("Contract not initialised");
    }
    async split(amount: att.Nat, destination: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "split", split_arg_to_mich(amount, destination), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_create_param(amount: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "create", create_arg_to_mich(amount), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_split_param(amount: att.Nat, destination: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "split", split_arg_to_mich(amount, destination), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_mt_value(key: att.Address): Promise<att.Ticket<att.Unit> | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), key.to_mich(), att.prim_annot_to_mich_type("address", []));
            if (data != undefined) {
                return att.Ticket.from_mich(data, x => { return new att.Unit(); });
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_mt_value(key: att.Address): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), key.to_mich(), att.prim_annot_to_mich_type("address", []));
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
        OPTION_IS_NONE: att.string_to_mich("\"OPTION_IS_NONE\"")
    };
}
export const test_split_ticket = new Test_split_ticket();
