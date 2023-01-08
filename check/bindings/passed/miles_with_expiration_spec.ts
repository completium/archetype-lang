import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export const mile_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("string", []);
export const owner_key_mich_type: att.MichelineType = att.prim_annot_to_mich_type("address", []);
export class mile_value implements att.ArchetypeType {
    constructor(public amount: att.Int, public expiration: Date) { }
    toString(): string {
        return JSON.stringify(this, null, 2);
    }
    to_mich(): att.Micheline {
        return att.pair_to_mich([this.amount.to_mich(), att.date_to_mich(this.expiration)]);
    }
    equals(v: mile_value): boolean {
        return att.micheline_equals(this.to_mich(), v.to_mich());
    }
    static from_mich(input: att.Micheline): mile_value {
        return new mile_value(att.Int.from_mich((input as att.Mpair).args[0]), att.mich_to_date((input as att.Mpair).args[1]));
    }
}
export const mile_value_mich_type: att.MichelineType = att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%amount"]),
    att.prim_annot_to_mich_type("timestamp", ["%expiration"])
], []);
export const owner_value_mich_type: att.MichelineType = att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []);
export type mile_container = Array<[
    string,
    mile_value
]>;
export type owner_container = Array<[
    att.Address,
    Array<string>
]>;
export const mile_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("string", []), att.pair_array_to_mich_type([
    att.prim_annot_to_mich_type("int", ["%amount"]),
    att.prim_annot_to_mich_type("timestamp", ["%expiration"])
], []), []);
export const owner_container_mich_type: att.MichelineType = att.pair_annot_to_mich_type("map", att.prim_annot_to_mich_type("address", []), att.set_annot_to_mich_type(att.prim_annot_to_mich_type("string", []), []), []);
const add_arg_to_mich = (ow: att.Address, newmile_id: string, newmile_amount: att.Int, newmile_expiration: Date): att.Micheline => {
    return att.pair_to_mich([
        ow.to_mich(),
        att.string_to_mich(newmile_id),
        newmile_amount.to_mich(),
        att.date_to_mich(newmile_expiration)
    ]);
}
const consume_arg_to_mich = (a: att.Address, quantity: att.Int): att.Micheline => {
    return att.pair_to_mich([
        a.to_mich(),
        quantity.to_mich()
    ]);
}
const clear_expired_arg_to_mich = (): att.Micheline => {
    return att.unit_mich;
}
export class Miles_with_expiration {
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
        const address = (await ex.deploy("../tests/passed/miles_with_expiration.arl", {}, params)).address;
        this.address = address;
    }
    async add(ow: att.Address, newmile_id: string, newmile_amount: att.Int, newmile_expiration: Date, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "add", add_arg_to_mich(ow, newmile_id, newmile_amount, newmile_expiration), params);
        }
        throw new Error("Contract not initialised");
    }
    async consume(a: att.Address, quantity: att.Int, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "consume", consume_arg_to_mich(a, quantity), params);
        }
        throw new Error("Contract not initialised");
    }
    async clear_expired(params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "clear_expired", clear_expired_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_add_param(ow: att.Address, newmile_id: string, newmile_amount: att.Int, newmile_expiration: Date, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "add", add_arg_to_mich(ow, newmile_id, newmile_amount, newmile_expiration), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_consume_param(a: att.Address, quantity: att.Int, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "consume", consume_arg_to_mich(a, quantity), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_clear_expired_param(params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "clear_expired", clear_expired_arg_to_mich(), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_admin(): Promise<att.Address> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Address.from_mich((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_mile(): Promise<mile_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[1], (x, y) => [att.mich_to_string(x), mile_value.from_mich(y)]);
        }
        throw new Error("Contract not initialised");
    }
    async get_owner(): Promise<owner_container> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_map((storage as att.Mpair).args[2], (x, y) => [att.Address.from_mich(x), att.mich_to_list(y, x => { return att.mich_to_string(x); })]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {
        INVALID_CALLER: att.string_to_mich("\"INVALID_CALLER\""),
        NOTENOUGHMILES: att.string_to_mich("\"NotEnoughMiles\""),
        r2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"r2\"")]),
        c2: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c2\"")]),
        c1: att.pair_to_mich([att.string_to_mich("\"INVALID_CONDITION\""), att.string_to_mich("\"c1\"")])
    };
}
export const miles_with_expiration = new Miles_with_expiration();
