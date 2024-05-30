import * as ex from "@completium/experiment-ts";
import * as att from "@completium/archetype-ts-types";
const exec_arg_to_mich = (input_x: att.Bls12_381_fr, input_y: att.Bls12_381_fr, proof_a: att.Bls12_381_g1, proof_b: att.Bls12_381_g2, proof_c: att.Bls12_381_g1): att.Micheline => {
    return att.pair_to_mich([
        input_x.to_mich(),
        input_y.to_mich(),
        proof_a.to_mich(),
        proof_b.to_mich(),
        proof_c.to_mich()
    ]);
}
export class Root_zok {
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
        const address = (await ex.deploy("../tests/passed/root_zok.arl", {}, params)).address;
        this.address = address;
    }
    async exec(input_x: att.Bls12_381_fr, input_y: att.Bls12_381_fr, proof_a: att.Bls12_381_g1, proof_b: att.Bls12_381_g2, proof_c: att.Bls12_381_g1, params: Partial<ex.Parameters>): Promise<att.CallResult> {
        if (this.address != undefined) {
            return await ex.call(this.address, "exec", exec_arg_to_mich(input_x, input_y, proof_a, proof_b, proof_c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_exec_param(input_x: att.Bls12_381_fr, input_y: att.Bls12_381_fr, proof_a: att.Bls12_381_g1, proof_b: att.Bls12_381_g2, proof_c: att.Bls12_381_g1, params: Partial<ex.Parameters>): Promise<att.CallParameter> {
        if (this.address != undefined) {
            return await ex.get_call_param(this.address, "exec", exec_arg_to_mich(input_x, input_y, proof_a, proof_b, proof_c), params);
        }
        throw new Error("Contract not initialised");
    }
    async get_res(): Promise<boolean> {
        if (this.address != undefined) {
            const storage = await ex.get_raw_storage(this.address);
            return att.mich_to_bool(storage);
        }
        throw new Error("Contract not initialised");
    }
    errors = {};
}
export const root_zok = new Root_zok();
