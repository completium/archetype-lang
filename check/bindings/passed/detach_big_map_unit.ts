import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const init_arg_to_mich = (a: att.Address): att.Micheline => {
    return a.to_mich();
}
const import_arg_to_mich = (param: att.Ticket<att.Unit>): att.Micheline => {
    return param.to_mich((x => { return att.unit_to_mich(); }));
}
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Detach_big_map_unit {
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
        const address = (await ex.deploy("../tests/passed/detach_big_map_unit.arl", {}, params)).address;
        this.address = address;
    }
    async init(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "init", init_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async import(param: att.Ticket<att.Unit>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "import", import_arg_to_mich(param), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_init_param(a: att.Address, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "init", init_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_import_param(param: att.Ticket<att.Unit>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "import", import_arg_to_mich(param), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_mt_value(key: att.Address): Promise<att.Ticket<att.Unit> | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), key.to_mich(), att.prim_annot_to_mich_type("address", []));
            if (data != undefined) {
                return att.Ticket.from_mich(data, x => { return new att.Unit(); });
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_mt_value(key: att.Address): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich(storage).toString()), key.to_mich(), att.prim_annot_to_mich_type("address", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        OPTION_IS_NONE: att.string_to_mich("\"OPTION_IS_NONE\"")
    };
}
export const detach_big_map_unit = new Detach_big_map_unit();
