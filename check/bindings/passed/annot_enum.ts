import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum myenum_types {
    abc = "abc",
    xyz = "xyz"
}
export abstract class myenum extends att.Enum<myenum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: myenum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class abc extends myenum {
    constructor(private content: att.Nat) {
        super(myenum_types.abc);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class xyz extends myenum {
    constructor(private content: att.Nat) {
        super(myenum_types.xyz);
    }
    to_mich() { return att.right_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_myenum = (m: att.Micheline): myenum => {
    if ((m as att.Msingle).prim == "Left") {
        return new abc(att.Nat.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new xyz(att.Nat.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_myenum : invalid micheline");
};
const exec_arg_to_mich = (a: Array<myenum>): att.Micheline => {
    return att.list_to_mich(a, x => {
        return x.to_mich();
    });
}
export class Annot_enum {
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
        const address = (await ex.deploy("../tests/passed/annot_enum.arl", {}, params)).address;
        this.address = address;
    }
    async exec(a: Array<myenum>, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(a: Array<myenum>, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(a), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_r(): Promise<myenum> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return mich_to_myenum((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_z(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const annot_enum = new Annot_enum();
