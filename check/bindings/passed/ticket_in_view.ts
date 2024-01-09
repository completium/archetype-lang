import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const view_check_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Ticket_in_view {
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
        const address = (await ex.deploy("../tests/passed/ticket_in_view.arl", {}, params)).address;
        this.address = address;
    }
    async view_check(params: Partial<ex.Parameters>): Promise<boolean | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "check", view_check_arg_to_mich(), params);
            return mich.value ? att.mich_to_bool(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    async get_some_ticket(): Promise<att.Option<att.Ticket<att.Unit>>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich(storage, x => { return att.Ticket.from_mich(x, x => { return new att.Unit(); }); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const ticket_in_view = new Ticket_in_view();
