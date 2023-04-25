import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum import_arl_enum_with_args_def__my_enum_types {
    A = "A",
    B = "B",
    C = "C"
}
export abstract class import_arl_enum_with_args_def__my_enum extends att.Enum<import_arl_enum_with_args_def__my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: import_arl_enum_with_args_def__my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class A extends import_arl_enum_with_args_def__my_enum {
    constructor(private content: att.Nat) {
        super(import_arl_enum_with_args_def__my_enum_types.A);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class B extends import_arl_enum_with_args_def__my_enum {
    constructor(private content: string) {
        super(import_arl_enum_with_args_def__my_enum_types.B);
    }
    to_mich() { return att.right_to_mich(att.left_to_mich(att.string_to_mich(this.content))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class C extends import_arl_enum_with_args_def__my_enum {
    constructor() {
        super(import_arl_enum_with_args_def__my_enum_types.C);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(att.unit_mich)); }
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
    to_mich() { return att.left_to_mich(att.unit_mich); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class Y extends my_enum {
    constructor(private content: string) {
        super(my_enum_types.Y);
    }
    to_mich() { return att.right_to_mich(att.left_to_mich(att.string_to_mich(this.content))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class Z extends my_enum {
    constructor(private content: att.Nat) {
        super(my_enum_types.Z);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(this.content.to_mich())); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_import_arl_enum_with_args_def__my_enum = (m: att.Micheline): import_arl_enum_with_args_def__my_enum => {
    if ((m as att.Msingle).prim == "Left") {
        return new A(att.Nat.from_mich((m as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new B(att.mich_to_string(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Right") {
        return new C();
    }
    throw new Error("mich_to_import_arl_enum_with_args_def__my_enum : invalid micheline");
};
export const mich_to_my_enum = (m: att.Micheline): my_enum => {
    if ((m as att.Msingle).prim == "Left") {
        return new X();
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new Y(att.mich_to_string(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Right") {
        return new Z(att.Nat.from_mich(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    throw new Error("mich_to_my_enum : invalid micheline");
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Import_arl_enum_with_args_use_collide {
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
        const address = (await ex.deploy("../tests/passed/import_arl_enum_with_args_use_collide.arl", {}, params)).address;
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
    async get_res_imported(): Promise<att.Option<import_arl_enum_with_args_def__my_enum>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Option.from_mich((storage as att.Mpair).args[0], x => { return mich_to_import_arl_enum_with_args_def__my_enum(x); });
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
    errors = {};
}
export const import_arl_enum_with_args_use_collide = new Import_arl_enum_with_args_use_collide();
