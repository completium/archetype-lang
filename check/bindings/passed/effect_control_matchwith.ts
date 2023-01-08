import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum t_types {
    C1 = "C1",
    C2 = "C2",
    C3 = "C3",
    C4 = "C4",
    C5 = "C5"
}
export abstract class t extends att.Enum<t_types> {
    abstract to_mich(): att.Micheline;
    equals(v: t): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class C1 extends t {
    constructor() {
        super(t_types.C1);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class C2 extends t {
    constructor() {
        super(t_types.C2);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class C3 extends t {
    constructor() {
        super(t_types.C3);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class C4 extends t {
    constructor() {
        super(t_types.C4);
    }
    to_mich() { return new att.Int(3).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class C5 extends t {
    constructor() {
        super(t_types.C5);
    }
    to_mich() { return new att.Int(4).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_t = (m: any): t => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new C1();
        case 1: return new C2();
        case 2: return new C3();
        case 3: return new C4();
        case 4: return new C5();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Effect_control_matchwith {
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
        const address = (await ex.deploy("../tests/passed/effect_control_matchwith.arl", {}, params)).address;
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
    async get_res(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const effect_control_matchwith = new Effect_control_matchwith();
