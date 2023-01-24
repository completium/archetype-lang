import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const process_arg_to_mich = (v: att.Nat): att.Micheline => {
    return v.to_mich();
}
const set_owner_arg_to_mich = (v: att.Address): att.Micheline => {
    return v.to_mich();
}
export class Dummy {
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
        const address = (await ex.deploy("../tests/contracts/multisig/dummy.arl", {
            owner: owner.to_mich()
        }, params)).address;
        this.address = address;
    }
    async process(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "process", process_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async set_owner(v: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "set_owner", set_owner_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_process_param(v: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "process", process_arg_to_mich(v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_set_owner_param(v: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "set_owner", set_owner_arg_to_mich(v), params);
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
    async get_result(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\"")
    };
}
export const dummy = new Dummy();
