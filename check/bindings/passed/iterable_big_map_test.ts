import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const put_arg_to_mich = (k: string, v: att.Bytes): att.Micheline => {
    return att.pair_to_mich([
        att.string_to_mich(k),
        v.to_mich()
    ]);
}
const remove_arg_to_mich = (k: string): att.Micheline => {
    return att.string_to_mich(k);
}
const iterate_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Iterable_big_map_test {
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
        const address = (await ex.deploy("../tests/passed/iterable_big_map_test.arl", {}, params)).address;
        this.address = address;
    }
    async put(k: string, v: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "put", put_arg_to_mich(k, v), params);
        }
        throw new Error("Contract not initialised");
    }
    async remove(k: string, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "remove", remove_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async iterate(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "iterate", iterate_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_put_param(k: string, v: att.Bytes, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "put", put_arg_to_mich(k, v), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_remove_param(k: string, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "remove", remove_arg_to_mich(k), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_iterate_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "iterate", iterate_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_my_map_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const raw_data = await ex.get_big_map_value(BigInt(att.Int.from_mich((((storage as att.Mpair).args[0] as att.Mpair)?.args)[0]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            const data = raw_data ? (raw_data?.args)[1] : undefined;
            if (data != undefined) {
                return att.Bytes.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_my_map_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((((storage as att.Mpair).args[0] as att.Mpair)?.args)[0]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_res_concat(): Promise<att.Bytes> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Bytes.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const iterable_big_map_test = new Iterable_big_map_test();
