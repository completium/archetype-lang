import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Import_arl_constant_def {
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
        const address = (await ex.deploy("../tests/passed/import_arl_constant_def.arl", {}, params)).address;
        this.address = address;
    }
    errors = {};
}
export const import_arl_constant_def = new Import_arl_constant_def();
