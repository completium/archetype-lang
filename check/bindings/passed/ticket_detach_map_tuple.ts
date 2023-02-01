import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const init_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Ticket_detach_map_tuple {
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
        const address = (await ex.deploy("../tests/passed/ticket_detach_map_tuple.arl", {}, params)).address;
        this.address = address;
    }
    async init(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "init", init_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async exec(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_init_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "init", init_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_input(): Promise<Array<[
        att.Nat,
        [
            string,
            att.Ticket<string>
        ]
    ]>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[0], (x, y) => [att.Nat.from_mich(x), (p => {
                    return [att.mich_to_string((p as att.Mpair).args[0]), att.Ticket.from_mich((p as att.Mpair).args[1], x => { return att.mich_to_string(x); })];
                })(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_output(): Promise<att.Option<att.Ticket<string>>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[1], x => { return att.Ticket.from_mich(x, x => { return att.mich_to_string(x); }); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        ERROR: att.string_to_mich("\"ERROR\"")
    };
}
export const ticket_detach_map_tuple = new Ticket_detach_map_tuple();
