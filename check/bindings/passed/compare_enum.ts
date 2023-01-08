import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum stake_kind_types {
    Tier1 = "Tier1",
    Tier2 = "Tier2",
    Tier3 = "Tier3",
    Tier4 = "Tier4"
}
export abstract class stake_kind extends att.Enum<stake_kind_types> {
    abstract to_mich(): att.Micheline;
    equals(v: stake_kind): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class Tier1 extends stake_kind {
    constructor() {
        super(stake_kind_types.Tier1);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Tier2 extends stake_kind {
    constructor() {
        super(stake_kind_types.Tier2);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Tier3 extends stake_kind {
    constructor() {
        super(stake_kind_types.Tier3);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Tier4 extends stake_kind {
    constructor() {
        super(stake_kind_types.Tier4);
    }
    to_mich() { return new att.Int(3).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_stake_kind = (m: any): stake_kind => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new Tier1();
        case 1: return new Tier2();
        case 2: return new Tier3();
        case 3: return new Tier4();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Compare_enum {
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
        const address = (await ex.deploy("../tests/passed/compare_enum.arl", {}, params)).address;
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
    errors = {};
}
export const compare_enum = new Compare_enum();
