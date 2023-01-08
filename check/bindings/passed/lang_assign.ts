import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Lang_assign {
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
        const address = (await ex.deploy("../tests/passed/lang_assign.arl", {}, params)).address;
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
    async get_n(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_i(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ra(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_da(): Promise<Date> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_date((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_du(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[4]);
        }
        throw new Error("Contract not initialised");
    }
    async get_tz(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[5]);
        }
        throw new Error("Contract not initialised");
    }
    async get_g1(): Promise<att.Bls12_381_g1> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_g1.from_mich((storage as att.Mpair).args[6]);
        }
        throw new Error("Contract not initialised");
    }
    async get_g2(): Promise<att.Bls12_381_g2> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_g2.from_mich((storage as att.Mpair).args[7]);
        }
        throw new Error("Contract not initialised");
    }
    async get_fr(): Promise<att.Bls12_381_fr> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_fr.from_mich((storage as att.Mpair).args[8]);
        }
        throw new Error("Contract not initialised");
    }
    async get_b(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[9]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const lang_assign = new Lang_assign();
