import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
import * as el from "@completium/event-listener";
export class even implements att.ArchetypeType {
    constructor(public from_: att.Address, public time: Date) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.from_.to_mich(), att.date_to_mich(this.time)]);
    }
    equals(v: even): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): even {
        return new even(att.Address.from_mich((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]));
    }
}
export class odd implements att.ArchetypeType {
    constructor(public from_: att.Address, public time: Date) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.from_.to_mich(), att.date_to_mich(this.time)]);
    }
    equals(v: odd): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): odd {
        return new odd(att.Address.from_mich((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]));
    }
}
const e1_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
export class Event_dup {
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
        const address = (await ex.deploy("../tests/passed/event_dup.arl", {}, params)).address;
        this.address = address;
    }
    async e1(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e1", e1_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e1_param(n: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e1", e1_arg_to_mich(n), params);
        }
        throw new Error("Contract not initialised");
    }
    register_even(ep: el.EventProcessor<even>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "even"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return even.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    register_odd(ep: el.EventProcessor<odd>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "odd"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return odd.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const event_dup = new Event_dup();
