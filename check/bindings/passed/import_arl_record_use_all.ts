import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export enum import_arl_all_def__my_enum_types {
    eFirst = "eFirst",
    eSecond = "eSecond",
    eThird = "eThird"
}
export abstract class import_arl_all_def__my_enum extends att.Enum<import_arl_all_def__my_enum_types> {
    abstract to_mich(): att.Micheline;
    equals(v: import_arl_all_def__my_enum): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
}
export class eFirst extends import_arl_all_def__my_enum {
    constructor() {
        super(import_arl_all_def__my_enum_types.eFirst);
    }
    to_mich() { return new att.Int(0).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class eSecond extends import_arl_all_def__my_enum {
    constructor() {
        super(import_arl_all_def__my_enum_types.eSecond);
    }
    to_mich() { return new att.Int(1).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export class eThird extends import_arl_all_def__my_enum {
    constructor() {
        super(import_arl_all_def__my_enum_types.eThird);
    }
    to_mich() { return new att.Int(2).to_mich(); }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
}
export const mich_to_import_arl_all_def__my_enum = (m: any): import_arl_all_def__my_enum => {
    const v = (new att.Nat((m as att.Mint).int)).to_big_number().toNumber();
    switch (v) {
        case 0: return new eFirst();
        case 1: return new eSecond();
        case 2: return new eThird();
        default: throw new Error("mich_to_asset_type : invalid value " + v);
    }
};
export class import_arl_all_def__my_record implements att.ArchetypeType {
    constructor(public n: att.Nat, public s: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.n.to_mich(), att.string_to_mich(this.s)]);
    }
    equals(v: import_arl_all_def__my_record): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): import_arl_all_def__my_record {
        return new import_arl_all_def__my_record(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_string((input as att.Mpair).args[1]));
    }
}
export const import_arl_all_def__my_record_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%n"]),
    att.prim_annot_to_mich_type("string", ["%s"])
], []);
const exec_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Import_arl_record_use_all {
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
        const address = (await ex.deploy("../tests/passed/import_arl_record_use_all.arl", {}, params)).address;
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
    async get_res(): Promise<import_arl_all_def__my_record> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return import_arl_all_def__my_record.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_myc(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_foo(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const import_arl_record_use_all = new Import_arl_record_use_all();
