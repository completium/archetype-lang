import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
import * as el from "@completium/event-listener";
export class ev implements att.ArchetypeType {
    constructor(public v: att.Nat) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return this.v.to_mich();
    }
    equals(v: ev): boolean {
        return this.v.equals(v.v);
    }
    static from_mich(input: att.Micheline): ev {
        return new ev(att.Nat.from_mich(input));
    }
}
const e1_arg_to_mich = (n: att.Nat): att.Micheline => {
    return n.to_mich();
}
export class Event_single {
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
        const address = (await ex.deploy("../tests/passed/event_single.arl", {}, params)).address;
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
    register_ev(ep: el.EventProcessor<ev>) {
        if (this.address != undefined) {
            el.registerEvent({ source: this.address, filter: tag => { return tag == "ev"; }, process: (raw: any, data: el.EventData | undefined) => {
                    const event = (x => {
                        return ev.from_mich((att.normalize(x) as att.Micheline));
                    })(raw);
                    ep(event, data);
                } });
            return;
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const event_single = new Event_single();
