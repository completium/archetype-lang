import { list_to_mich_type, list_to_mich, Bytes, Address, Nat, pair_to_mich, string_to_mich, Signature, Chain_id } from '@completium/archetype-ts-types'
import { blake2b, pack } from '@completium/experiment-ts'

import { transfer_param, transfer_param_mich_type } from '../../bindings/contracts/fa2/fa2_fungible'
import { rec_to_sign_permit_data, rec_to_sign_permit_data_mich_type } from '../../bindings/contracts/fa2/permits'

export const get_packed_transfer_params = (tps : transfer_param[]) : Bytes => {
  const mich = list_to_mich(tps, x => {
    return x.to_mich();
  })
  return (pack(mich, list_to_mich_type(transfer_param_mich_type)))
}

export const get_transfer_permit_data = (ptps : Bytes, contract : Address, chain_id : Chain_id, permit_counter : Nat | undefined) : Bytes => {
  let counter = permit_counter ?? new Nat(0)
  return pack(new rec_to_sign_permit_data(contract, chain_id, counter, blake2b(ptps)).to_mich(), rec_to_sign_permit_data_mich_type);
}

export const wrong_sig = new Signature("edsigu3QDtEZeSCX146136yQdJnyJDfuMRsDxiCgea3x7ty2RTwDdPpgioHWJUe86tgTCkeD2u16Az5wtNFDdjGyDpb7MiyU3fn");
export const wrong_packed_transfer_params = new Bytes('9aabe91d035d02ffb550bb9ea6fe19970f6fb41b5e69459a60b1ae401192a2dc');

export const get_missigned_error = (permit_data : Bytes) => {
  return pair_to_mich([string_to_mich("\"MISSIGNED\""), permit_data.to_mich()])
}
