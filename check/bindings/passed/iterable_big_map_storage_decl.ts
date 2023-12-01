import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Iterable_big_map_storage_decl {
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
        const address = (await ex.deploy("../tests/passed/iterable_big_map_storage_decl.arl", {}, params)).address;
        this.address = address;
    }
    async get_my_map_value(key: string): Promise<att.Bytes | undefined> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const raw_data = await ex.get_big_map_value(BigInt(att.Int.from_mich((((storage as att.Mpair).args[0] as att.Mpair)?.args)[0]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            const data = raw_data ? (raw_data?.args)[1] : undefined;
            if (data != undefined) {
                return att.Bytes.from_mich(data);
            }
            else {
                return undefined;
            }
        }
        throw new Error("Contract not initialised");
    }
    async has_my_map_value(key: string): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            const data = await ex.get_big_map_value(BigInt(att.Int.from_mich((((storage as att.Mpair).args[0] as att.Mpair)?.args)[0]).toString()), att.string_to_mich(key), att.prim_annot_to_mich_type("string", []));
            if (data != undefined) {
                return true;
            }
            else {
                return false;
            }
        }
        throw new Error("Contract not initialised");
    }
    async get_r(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const iterable_big_map_storage_decl = new Iterable_big_map_storage_decl();
