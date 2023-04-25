import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum my_enum_types {
    eFirst = "eFirst",
    eSecond = "eSecond",
    eThird = "eThird"
}
export abstract class my_enum extends att.Enum<my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class eFirst extends my_enum {
    constructor() {
        super(my_enum_types.eFirst);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class eSecond extends my_enum {
    constructor() {
        super(my_enum_types.eSecond);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class eThird extends my_enum {
    constructor() {
        super(my_enum_types.eThird);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_my_enum = (m: any): my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new eFirst();
        case 1: return new eSecond();
        case 2: return new eThird();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export class my_record implements att.ArchetypeType {
    constructor(public n: att.Nat, public s: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.n.to_mich(), att.string_to_mich(this.s)]);
    }
    equals(v: my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): my_record {
        return new my_record(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_string((input as att.Mpair).args[1]));
    }
}
export const my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%n"]),
    att.prim_annot_to_mich_type("string", ["%s"])
], []);
const e_arg_to_mich = (i: att.Nat): att.Micheline => {
    return i.to_mich();
}
const e2_arg_to_mich = (r: my_record): att.Micheline => {
    return r.to_mich();
}
const view_get_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Import_arl_all_def {
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
        const address = (await ex.deploy("../tests/passed/import_arl_all_def.arl", {}, params)).address;
        this.address = address;
    }
    async e(i: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e", e_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async e2(r: my_record, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "e2", e2_arg_to_mich(r), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e_param(i: att.Nat, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e", e_arg_to_mich(i), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_e2_param(r: my_record, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "e2", e2_arg_to_mich(r), params);
        }
        throw new Error("Contract not initialised");
    }
    async view_get(params: Partial<ex.Parameters>): Promise<att.Nat | undefined> {
        if (this.address != undefined) {
            const mich = await ex.exec_view(this.get_address(), "get", view_get_arg_to_mich(), params);
            return mich.value ? att.Nat.from_mich(mich.value) : undefined;
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const import_arl_all_def = new Import_arl_all_def();
