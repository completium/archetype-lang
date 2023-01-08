import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const e1_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e2_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e3_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e4_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e5_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e6_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e7_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e8_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const e9_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Multi_p {
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
        const address = (await ex.deploy("../tests/passed/multi_p.arl", {}, params)).address;
        this.address = address;
    }
    async e1(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e1", e1_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e2(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e2", e2_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e3(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e3", e3_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e4(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e4", e4_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e5(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e5", e5_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e6(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e6", e6_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e7(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e7", e7_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e8(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e8", e8_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async e9(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e9", e9_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e1_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e1", e1_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e2_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e2", e2_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e3_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e3", e3_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e4_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e4", e4_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e5_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e5", e5_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e6_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e6", e6_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e7_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e7", e7_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e8_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e8", e8_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e9_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e9", e9_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const multi_p = new Multi_p();
