import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Ticket_store_option {
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
        const address = (await ex.deploy("../tests/passed/ticket_store_option.arl", {}, params)).address;
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
    async get_ot(): Promise<att.Option<att.Ticket<string>>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[0], x => { return att.Ticket.from_mich(x, x => { return att.mich_to_string(x); }); });
        }
        throw new Error("Contract not initialised");
    }
    async get_info(): Promise<att.Option<[
        att.Address,
        string,
        att.Nat
    ]>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[1], x => { return (p => {
                return [att.Address.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1]), att.Nat.from_mich((p as att.Mpair).args[2])];
            })(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const ticket_store_option = new Ticket_store_option();
