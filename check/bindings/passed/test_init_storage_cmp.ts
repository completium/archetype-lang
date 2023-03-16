import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
export class Test_init_storage_cmp {
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
        const address = (await ex.deploy("../tests/passed/test_init_storage_cmp.arl", {}, params)).address;
        this.address = address;
    }
    async get_eq_int_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[0]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_int_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[1]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_int_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[2]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_int_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[3]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_int_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[4]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_int_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[5]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_rat_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[6]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_rat_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[7]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_rat_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[8]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_rat_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[9]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_rat_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[10]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_rat_int(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[11]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_int_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[12]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_int_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[13]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_int_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[14]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_int_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[15]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_int_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[16]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_int_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[17]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_rat_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[18]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_rat_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[19]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_rat_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[20]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_rat_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[21]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_rat_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[22]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_rat_rat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[23]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_tez_tez(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[24]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_tez_tez(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[25]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_tez_tez(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[26]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_tez_tez(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[27]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_tez_tez(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[28]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_tez_tez(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[29]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_dur_dur(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[30]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_dur_dur(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[31]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_dur_dur(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[32]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_dur_dur(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[33]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_dur_dur(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[34]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_dur_dur(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[35]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_dat_dat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[36]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_dat_dat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[37]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_dat_dat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[38]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_dat_dat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[39]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_dat_dat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[40]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_dat_dat(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[41]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_addr_addr(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[42]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_addr_addr(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[43]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_addr_addr(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[44]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_addr_addr(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[45]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_addr_addr(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[46]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_addr_addr(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[47]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_str_str(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[48]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_str_str(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[49]);
        }
        throw new Error("Contract not initialised");
    }
    async get_gt_str_str(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[50]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ge_str_str(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[51]);
        }
        throw new Error("Contract not initialised");
    }
    async get_lt_str_str(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[52]);
        }
        throw new Error("Contract not initialised");
    }
    async get_le_str_str(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[53]);
        }
        throw new Error("Contract not initialised");
    }
    async get_eq_bool_bool(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[54]);
        }
        throw new Error("Contract not initialised");
    }
    async get_ne_bool_bool(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool((storage as att.Mpair).args[55]);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const test_init_storage_cmp = new Test_init_storage_cmp();
