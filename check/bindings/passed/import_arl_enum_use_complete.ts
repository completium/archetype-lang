import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum import_arl_enum_def__my_enum_types {
    A = "A",
    B = "B",
    C = "C"
}
export abstract class import_arl_enum_def__my_enum extends att.Enum<import_arl_enum_def__my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: import_arl_enum_def__my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class A extends import_arl_enum_def__my_enum {
    constructor() {
        super(import_arl_enum_def__my_enum_types.A);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class B extends import_arl_enum_def__my_enum {
    constructor() {
        super(import_arl_enum_def__my_enum_types.B);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class C extends import_arl_enum_def__my_enum {
    constructor() {
        super(import_arl_enum_def__my_enum_types.C);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export enum my_enum_types {
    X = "X",
    Y = "Y",
    Z = "Z"
}
export abstract class my_enum extends att.Enum<my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class X extends my_enum {
    constructor() {
        super(my_enum_types.X);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Y extends my_enum {
    constructor() {
        super(my_enum_types.Y);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Z extends my_enum {
    constructor() {
        super(my_enum_types.Z);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_import_arl_enum_def__my_enum = (m: any): import_arl_enum_def__my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new A();
        case 1: return new B();
        case 2: return new C();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export const mich_to_my_enum = (m: any): my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new X();
        case 1: return new Y();
        case 2: return new Z();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Import_arl_enum_use_complete {
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
        const address = (await ex.deploy("../tests/passed/import_arl_enum_use_complete.arl", {}, params)).address;
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
    async get_res_imported(): Promise<att.Option<import_arl_enum_def__my_enum>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[0], x => { return mich_to_import_arl_enum_def__my_enum(x); });
        }
        throw new Error("Contract not initialised");
    }
    async get_res_top(): Promise<att.Option<my_enum>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[1], x => { return mich_to_my_enum(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        ERROR: att.string_to_mich("\"error\"")
    };
}
export const import_arl_enum_use_complete = new Import_arl_enum_use_complete();
