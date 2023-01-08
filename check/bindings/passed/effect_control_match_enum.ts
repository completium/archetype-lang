import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum e_types {
    A = "A",
    B = "B",
    C = "C"
}
export abstract class e extends att.Enum<e_types> {
    abstract to_mich(): att.Micheline;
    equals(v: e): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class A extends e {
    constructor() {
        super(e_types.A);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class B extends e {
    constructor() {
        super(e_types.B);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class C extends e {
    constructor() {
        super(e_types.C);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_e = (m: any): e => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new A();
        case 1: return new B();
        case 2: return new C();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Effect_control_match_enum {
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
        const address = (await ex.deploy("../tests/passed/effect_control_match_enum.arl", {}, params)).address;
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
    async get_res(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        KO: att.string_to_mich("\"KO\"")
    };
}
export const effect_control_match_enum = new Effect_control_match_enum();
