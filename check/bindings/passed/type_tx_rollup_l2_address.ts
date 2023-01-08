import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Type_tx_rollup_l2_address {
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
        const address = (await ex.deploy("../tests/passed/type_tx_rollup_l2_address.arl", {}, params)).address;
        this.address = address;
    }
    async get_a(): Promise<att.Tx_rollup_l2_address> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tx_rollup_l2_address.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const type_tx_rollup_l2_address = new Type_tx_rollup_l2_address();
