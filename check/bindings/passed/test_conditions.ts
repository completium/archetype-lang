import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const a1_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const a2_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Test_conditions {
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
        const address = (await ex.deploy("../tests/passed/test_conditions.arl", {}, params)).address;
        this.address = address;
    }
    async a1(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "a1", a1_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async a2(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "a2", a2_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_a1_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "a1", a1_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_a2_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "a2", a2_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_i(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        c6: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c6\"")]),
        c5: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c5\"")]),
        c4: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c4\"")]),
        c3: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c3\"")]),
        c2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c2\"")]),
        c1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c1\"")])
    };
}
export const test_conditions = new Test_conditions();
