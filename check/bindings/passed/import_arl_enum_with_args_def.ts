import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum my_enum_types {
    A = "A",
    B = "B",
    C = "C"
}
export abstract class my_enum extends att.Enum<my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class A extends my_enum {
    constructor(private content: att.Nat) {
        super(my_enum_types.A);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class B extends my_enum {
    constructor(private content: string) {
        super(my_enum_types.B);
    }
    to_mich() { return att.right_to_mich(att.left_to_mich(att.string_to_mich(this.content))); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class C extends my_enum {
    constructor() {
        super(my_enum_types.C);
    }
    to_mich() { return att.right_to_mich(att.right_to_mich(att.unit_mich)); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_my_enum = (m: att.Micheline): my_enum => {
    if ((m as att.Msingle).prim == "Left") {
        return new A(att.Nat.from_mich((m as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Left") {
        return new B(att.mich_to_string(((m as att.Msingle).args[0] as att.Msingle).args[0]));
    }
    if (((m as att.Msingle).args[0] as att.Msingle).prim == "Right") {
        return new C();
    }
    throw new Error("mich_to_my_enum : invalid micheline");
};
export class Import_arl_enum_with_args_def {
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
        const address = (await ex.deploy("../tests/passed/import_arl_enum_with_args_def.arl", {}, params)).address;
        this.address = address;
    }
    errors = {};
}
export const import_arl_enum_with_args_def = new Import_arl_enum_with_args_def();
