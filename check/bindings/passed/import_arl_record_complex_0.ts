import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class rec implements att.ArchetypeType {
    constructor(public a: att.Nat, public b: string) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.a.to_mich(), att.string_to_mich(this.b)]);
    }
    equals(v: rec): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): rec {
        return new rec(att.Nat.from_mich((input as att.Mpair).args[0]), att.mich_to_string((input as att.Mpair).args[1]));
    }
}
export const rec_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("nat", ["%a"]),
    att.prim_annot_to_mich_type("string", ["%b"])
], []);
export class Import_arl_record_complex_0 {
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
        const address = (await ex.deploy("../tests/passed/import_arl_record_complex_0.arl", {}, params)).address;
        this.address = address;
    }
    errors = {};
}
export const import_arl_record_complex_0 = new Import_arl_record_complex_0();
