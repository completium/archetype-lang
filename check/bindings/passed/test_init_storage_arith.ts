import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Test_init_storage_arith {
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
        const address = (await ex.deploy("../tests/passed/test_init_storage_arith.arl", {}, params)).address;
        this.address = address;
    }
    async get_bool_bool_and(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_bool_bool_or(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_bool_bool_not(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_nat_nat_plus(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_nat_nat_minus(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[4]);
        }
        throw new Error("Contract not initialised");
    }
    async get_nat_nat_mult(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[5]);
        }
        throw new Error("Contract not initialised");
    }
    async get_nat_nat_ediv(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[6]);
        }
        throw new Error("Contract not initialised");
    }
    async get_nat_nat_modulo(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[7]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_int_plus(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[8]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_int_minus(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[9]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_int_mult(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[10]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_int_ediv(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[11]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_int_modulo(): Promise<att.Nat> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Nat.from_mich((storage as att.Mpair).args[12]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_rat_plus(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[13]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_rat_minus(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[14]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_rat_mult(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[15]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_rat_div(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[16]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_rat_plus(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[17]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_rat_minus(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[18]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_rat_mult(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[19]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_rat_div(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[20]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_int_plus(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[21]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_int_minus(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[22]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_int_mult(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[23]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_int_div(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[24]);
        }
        throw new Error("Contract not initialised");
    }
    async get_date_date_minus(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[25]);
        }
        throw new Error("Contract not initialised");
    }
    async get_dur_dur_plus(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[26]);
        }
        throw new Error("Contract not initialised");
    }
    async get_dur_dur_minus(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[27]);
        }
        throw new Error("Contract not initialised");
    }
    async get_dur_dur_div(): Promise<att.Rational> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Rational.from_mich((storage as att.Mpair).args[28]);
        }
        throw new Error("Contract not initialised");
    }
    async get_dur_dur_ediv(): Promise<att.Int> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Int.from_mich((storage as att.Mpair).args[29]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_dur_mult(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[30]);
        }
        throw new Error("Contract not initialised");
    }
    async get_dur_int_ediv(): Promise<att.Duration> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Duration.from_mich((storage as att.Mpair).args[31]);
        }
        throw new Error("Contract not initialised");
    }
    async get_date_dur_plus(): Promise<Date> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_date((storage as att.Mpair).args[32]);
        }
        throw new Error("Contract not initialised");
    }
    async get_date_dur_minus(): Promise<Date> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_date((storage as att.Mpair).args[33]);
        }
        throw new Error("Contract not initialised");
    }
    async get_dur_date_plus(): Promise<Date> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_date((storage as att.Mpair).args[34]);
        }
        throw new Error("Contract not initialised");
    }
    async get_str_str_plus(): Promise<string> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_string((storage as att.Mpair).args[35]);
        }
        throw new Error("Contract not initialised");
    }
    async get_tez_tez_plus(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[36]);
        }
        throw new Error("Contract not initialised");
    }
    async get_tez_tez_minus(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[37]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_tez_mult(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[38]);
        }
        throw new Error("Contract not initialised");
    }
    async get_rat_tez_mult(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[39]);
        }
        throw new Error("Contract not initialised");
    }
    async get_int_tez_ediv(): Promise<att.Tez> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.Tez.from_mich((storage as att.Mpair).args[40]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_init_storage_arith = new Test_init_storage_arith();
