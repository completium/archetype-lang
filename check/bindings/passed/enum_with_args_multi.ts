import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum t_types {
    A = "A",
    B = "B",
    C = "C"
}
export abstract class t extends att.Enum<t_types> {
    abstract to_mich(): att.Micheline;
    equals(v: t): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class A extends t {
    constructor(private content: [
        att.Nat,
        string
    ]) {
        super(t_types.A);
    }
    to_mich() { return att.left_to_mich(att.pair_to_mich([this.content[0].to_mich(), att.string_to_mich(this.content[1])])); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class B extends t {
    constructor(private content: [
        att.Nat,
        string
    ]) {
        super(t_types.B);
    }
    to_mich() { return att.right_to_mich(att.left_to_mich(att.pair_to_mich([this.content[0].to_mich(), att.string_to_mich(this.content[1])]))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class C extends t {
    constructor(private content: [
        att.Int,
        att.Nat,
        string
    ]) {
        super(t_types.C);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(att.pair_to_mich([this.content[0].to_mich(), this.content[1].to_mich(), att.string_to_mich(this.content[2])]))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_t = (m: att.Micheline): t => {
    if ((m as att.Msingle).prim == "Left") {
        return new A((p => {
            return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1])];
        })((m as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new B((p => {
            return [att.Nat.from_mich((p as att.Mpair).args[0]), att.mich_to_string((p as att.Mpair).args[1])];
        })(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Right") {
        return new C((p => {
            return [att.Int.from_mich((p as att.Mpair).args[0]), att.Nat.from_mich((p as att.Mpair).args[1]), att.mich_to_string((p as att.Mpair).args[2])];
        })(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    throw new Error("mich_to_t : invalid micheline");
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Enum_with_args_multi {
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
        const address = (await ex.deploy("../tests/passed/enum_with_args_multi.arl", {}, params)).address;
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
    errors = {};
}
export const enum_with_args_multi = new Enum_with_args_multi();
