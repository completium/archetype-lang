import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum t_types {
    A = "A",
    B = "B",
    C = "C",
    D = "D",
    E = "E"
}
export abstract class t extends att.Enum<t_types> {
    abstract to_mich(): att.Micheline;
    equals(v: t): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class A extends t {
    constructor(private content: string) {
        super(t_types.A);
    }
    to_mich() { return att.left_to_mich(att.string_to_mich(this.content)); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class B extends t {
    constructor(private content: att.Nat) {
        super(t_types.B);
    }
    to_mich() { return att.right_to_mich(att.left_to_mich(this.content.to_mich())); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class C extends t {
    constructor() {
        super(t_types.C);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(att.left_to_mich(att.unit_mich))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class D extends t {
    constructor(private content: att.Address) {
        super(t_types.D);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(att.right_to_mich(att.left_to_mich(this.content.to_mich())))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class E extends t {
    constructor(private content: att.Int) {
        super(t_types.E);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(att.right_to_mich(att.right_to_mich(this.content.to_mich())))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_t = (m: att.Micheline): t => {
    if ((m as att.Msingle).prim == "Left") {
        return new A(att.mich_to_string((m as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new B(att.Nat.from_mich(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    if ((((m as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new C();
    }
    if (((((m as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new D(att.Address.from_mich(((((m as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    if (((((m as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).prim == "Right") {
        return new E(att.Int.from_mich(((((m as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    throw new Error("mich_to_t : invalid micheline");
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Enum_all {
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
        const address = (await ex.deploy("../tests/passed/enum_all.arl", {}, params)).address;
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
            return att.Nat.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_r(): Promise<t> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return mich_to_t((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const enum_all = new Enum_all();
