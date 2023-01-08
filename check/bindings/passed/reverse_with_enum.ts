import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum myenum_types {
    entry1 = "entry1",
    entry2 = "entry2"
}
export abstract class myenum extends att.Enum<myenum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: myenum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class entry1 extends myenum {
    constructor(private content: att.Int) {
        super(myenum_types.entry1);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class entry2 extends myenum {
    constructor(private content: string) {
        super(myenum_types.entry2);
    }
    to_mich() { return att.right_to_mich(att.string_to_mich(this.content)); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export const mich_to_myenum = (m: att.Micheline): myenum => {
    if ((m as att.Msingle).prim == "Left") {
        return new entry1(att.Int.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new entry2(att.mich_to_string((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_myenum : invalid micheline");
};
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Reverse_with_enum {
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
        const address = (await ex.deploy("../tests/passed/reverse_with_enum.arl", {}, params)).address;
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
    async get_res(): Promise<Array<myenum>> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_list(storage, x => { return mich_to_myenum(x); });
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const reverse_with_enum = new Reverse_with_enum();
