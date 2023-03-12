import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Typetuple {
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
        const address = (await ex.deploy("../tests/passed/typetuple.arl", {}, params)).address;
        this.address = address;
    }
    async get_v(): Promise<[
        att.Int,
        string
    ]> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return (p => {
                return [att.Int.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1])];
            })(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const typetuple = new Typetuple();
