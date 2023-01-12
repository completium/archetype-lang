import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum my_enum_types {
    One = "One",
    Two = "Two",
    Three = "Three"
}
export abstract class my_enum extends att.Enum<my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class One extends my_enum {
    constructor() {
        super(my_enum_types.One);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Two extends my_enum {
    constructor() {
        super(my_enum_types.Two);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Three extends my_enum {
    constructor() {
        super(my_enum_types.Three);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_my_enum = (m: any): my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new One();
        case 1: return new Two();
        case 2: return new Three();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Lang_enum {
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
        const address = (await ex.deploy("../tests/passed/lang_enum.arl", {}, params)).address;
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
    errors = {
        KO: att.string_to_mich("\"ko\"")
    };
}
export const lang_enum = new Lang_enum();
