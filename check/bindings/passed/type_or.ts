import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Type_or {
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
        const address = (await ex.deploy("../tests/passed/type_or.arl", {}, params)).address;
        this.address = address;
    }
    async get_ls(): Promise<att.Or<att.Nat, string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Or.from_mich((storage as att.Mpair).args[0], x => { return att.Nat.from_mich(x); }, x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_lc(): Promise<att.Or<att.Nat, string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Or.from_mich((storage as att.Mpair).args[1], x => { return att.Nat.from_mich(x); }, x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_rs(): Promise<att.Or<att.Nat, string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Or.from_mich((storage as att.Mpair).args[2], x => { return att.Nat.from_mich(x); }, x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_rc(): Promise<att.Or<att.Nat, string>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Or.from_mich((storage as att.Mpair).args[3], x => { return att.Nat.from_mich(x); }, x => { return att.mich_to_string(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const type_or = new Type_or();
