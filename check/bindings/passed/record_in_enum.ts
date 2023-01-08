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
    constructor(private content: myrecord) {
        super(myenum_types.abc);
    }
    to_mich() { return att.left_to_mich(this.content.to_mich()); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    get() { return this.content; }
}
export class xyz extends myenum {
    constructor(private content: myrecord) {
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
        return new abc(myrecord.from_mich((m as att.Msingle).args[0]));
    }
    if ((m as att.Msingle).prim == "Right") {
        return new xyz(myrecord.from_mich((m as att.Msingle).args[0]));
    }
    throw new Error("mich_to_myenum : invalid micheline");
};
export class myrecord implements att.ArchetypeType {
    constructor(public a: att.Nat, public b: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.a.to_mich(), att.string_to_mich(this.b)]);
    }
    equals(v: myrecord): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): myrecord {
        return new myrecord(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_string((input as att.Mpair).args[1]));
    }
}
export const myrecord_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%a"]),
    att.prim_annot_to_mich_type("string", ["%b"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Record_in_enum {
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
        const address = (await ex.deploy("../tests/passed/record_in_enum.arl", {}, params)).address;
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
export const record_in_enum = new Record_in_enum();
