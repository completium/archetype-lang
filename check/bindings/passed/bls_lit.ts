import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Bls_lit {
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
        const address = (await ex.deploy("../tests/passed/bls_lit.arl", {}, params)).address;
        this.address = address;
    }
    async get_a(): Promise<att.Bls12_381_fr> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_fr.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_b(): Promise<att.Bls12_381_fr> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_fr.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_c(): Promise<att.Bls12_381_g1> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_g1.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_d(): Promise<att.Bls12_381_g2> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_g2.from_mich((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_e(): Promise<att.Bls12_381_fr> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bls12_381_fr.from_mich((storage as att.Mpair).args[4]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const bls_lit = new Bls_lit();
