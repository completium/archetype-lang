(* -------------------------------------------------------------------- *)
open Tools

module M = Michelson

let pp_type = Printer_michelson.pp_type

(* -------------------------------------------------------------------- *)
type stack1 = M.type_ [@@deriving  show {with_path = false}]

type arg1 = [
  | `Int  of int
  | `Code of M.code list
  | `Type of M.type_
]

type stack = stack1 list [@@deriving  show {with_path = false}]
type args  = arg1   list

type s_Int   = SInt
type s_Code  = SCode
type s_Type  = SType

type _ targ1 =
  | TInt  : int -> s_Int targ1
  | TCode : M.code list -> s_Code targ1
  | TType : M.type_ -> s_Type targ1

(* -------------------------------------------------------------------- *)
exception MichelsonTypingError

(* -------------------------------------------------------------------- *)
module Ty : sig
  val check_eq : stack1 -> stack1 -> unit

  val check_address      : stack1 -> unit
  val check_bls12_381_g1 : stack1 -> unit
  val check_bls12_381_g2 : stack1 -> unit
  val check_bool         : stack1 -> unit
  val check_bytes        : stack1 -> unit
  val check_contract     : stack1 -> stack1
  val check_int          : stack1 -> unit
  val check_key          : stack1 -> unit
  val check_key_hash     : stack1 -> unit
  val check_lambda       : stack1 -> stack1 * stack1
  val check_list         : stack1 -> stack1
  val check_mutez        : stack1 -> unit
  val check_nat          : stack1 -> unit
  val check_option       : stack1 -> stack1
  val check_or           : stack1 -> stack1 * stack1
  val check_pair         : stack1 -> stack1 * stack1
  val check_list_pair    : stack1 -> stack1 * stack1
  val check_set          : stack1 -> stack1
  val check_sapling_tx   : stack1 -> int
  val check_sapling_st   : stack1 -> int
  val check_signature    : stack1 -> unit
  val check_ticket       : stack1 -> stack1
end = struct
  let check_eq (ty1 : stack1) (ty2 : stack1) =
    if not (M.cmp_type ty1 ty2) then
      raise MichelsonTypingError

  let check_address (ty : stack1) =
    match ty.node with
    | M.Taddress -> ()
    | _ -> raise MichelsonTypingError

  let check_bls12_381_g1 (ty : stack1) =
    match ty.node with
    | M.Tbls12_381_g1 -> ()
    | _ -> raise MichelsonTypingError

  let check_bls12_381_g2 (ty : stack1) =
    match ty.node with
    | M.Tbls12_381_g2 -> ()
    | _ -> raise MichelsonTypingError

  let check_bool (ty : stack1) =
    match ty.node with
    | M.Tbool -> ()
    | _ -> raise MichelsonTypingError

  let check_bytes (ty : stack1) =
    match ty.node with
    | M.Tbytes -> ()
    | _ -> raise MichelsonTypingError

  let check_contract (ty : stack1) =
    match ty.node with
    | M.Tcontract ty -> ty
    | _ -> raise MichelsonTypingError

  let check_int (ty : stack1) =
    match ty.node with
    | M.Tint -> ()
    | _ -> raise MichelsonTypingError

  let check_key (ty : stack1) =
    match ty.node with
    | M.Tkey -> ()
    | _ -> raise MichelsonTypingError

  let check_key_hash (ty : stack1) =
    match ty.node with
    | M.Tkey_hash -> ()
    | _ -> raise MichelsonTypingError

  let check_lambda (ty : stack1) =
    match ty.node with
    | M.Tlambda (dom, codom) -> (dom, codom)
    | _ -> raise MichelsonTypingError

  let check_list (ty : stack1) =
    match ty.node with
    | M.Tlist ty -> ty
    | _ -> raise MichelsonTypingError

  let check_mutez (ty : stack1) =
    match ty.node with
    | M.Tmutez -> ()
    | _ -> raise MichelsonTypingError

  let check_nat (ty : stack1) =
    match ty.node with
    | M.Tnat -> ()
    | _ -> raise MichelsonTypingError

  let check_or (ty : stack1) =
    match ty.node with
    | M.Tor (tyl, tyr) -> (tyl, tyr)
    | _ -> raise MichelsonTypingError

  let check_option (ty : stack1) =
    match ty.node with
    | M.Toption ty -> ty
    | _ -> raise MichelsonTypingError

  let check_pair (ty : stack1) =
    match ty.node with
    | M.Tpair [ty1; ty2] -> (ty1, ty2)
    | M.Tpair (_::[]) -> raise MichelsonTypingError
    | M.Tpair (ty1::l) -> (ty1, M.tpair l)
    | _ -> raise MichelsonTypingError

  let check_list_pair (ty : stack1) =
    match ty.node with
    | M.Tlist ty -> check_pair ty
    | _ -> raise MichelsonTypingError

  let check_sapling_tx (ty : stack1) =
    match ty.node with
    | M.Tsapling_transaction ms -> ms
    | _ -> raise MichelsonTypingError

  let check_sapling_st (ty : stack1) =
    match ty.node with
    | M.Tsapling_state ms -> ms
    | _ -> raise MichelsonTypingError

  let check_set (ty : stack1) =
    match ty.node with
    | M.Tset ty -> ty
    | _ -> raise MichelsonTypingError

  let check_signature (ty : stack1) =
    match ty.node with
    | M.Tsignature -> ()
    | _ -> raise MichelsonTypingError

  let check_ticket (ty : stack1) =
    match ty.node with
    | M.Tticket ty -> ty
    | _ -> raise MichelsonTypingError
end

(* -------------------------------------------------------------------- *)
module Stack : sig
  val pop   : stack -> stack1 * stack
  val pop2  : stack -> (stack1 * stack1) * stack
  val pop3  : stack -> (stack1 * stack1 * stack1) * stack
  val merge : stack option -> stack option -> stack option
  val split : int -> stack -> stack * stack
end = struct
  let pop (stack : stack) =
    match stack with
    | ty :: stack -> (ty, stack)
    | _ -> assert false

  let pop2 (stack : stack) =
    match stack with
    | ty1 :: ty2 :: stack -> ((ty1, ty2), stack)
    | _ -> assert false

  let pop3 (stack : stack) =
    match stack with
    | ty1 :: ty2 :: ty3 :: stack -> ((ty1, ty2, ty3), stack)
    | _ -> assert false

  let merge (stk1 : stack option) (stk2 : stack option) =
    match stk1, stk2 with
    | None, None ->
      None
    | Some stk1, None ->
      Some stk1
    | None, Some stk2 ->
      Some stk2
    | Some stk1, Some stk2 ->
      if List.length stk1 <> List.length stk2 then
        raise MichelsonTypingError;
      let () = List.iter2 Ty.check_eq stk1 stk2 in
      Some stk2

  let split n stack =
    if List.length stack < n then
      raise MichelsonTypingError;
    List.split_at n stack
end

(* -------------------------------------------------------------------- *)
let rec op_ABS (stack : stack) =
  let ty, stack = Stack.pop stack in
  let _ = Ty.check_int ty in
  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_ADD (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tnat, M.Tnat -> M.Tnat
    | M.Tnat, M.Tint -> M.Tint
    | M.Tint, M.Tnat -> M.Tint
    | M.Tint, M.Tint -> M.Tint

    | M.Tint, M.Ttimestamp -> M.Ttimestamp
    | M.Ttimestamp, M.Tint -> M.Ttimestamp

    | M.Tmutez, M.Tmutez -> M.Tmutez

    | M.Tbls12_381_g1, M.Tbls12_381_g1 -> M.Tbls12_381_g1
    | M.Tbls12_381_g2, M.Tbls12_381_g2 -> M.Tbls12_381_g2
    | M.Tbls12_381_fr, M.Tbls12_381_fr -> M.Tbls12_381_fr

    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_ADDRESS (stack : stack) =
  let cty, stack = Stack.pop stack in
  let _ = Ty.check_contract cty in
  Some (M.taddress :: stack)

(* -------------------------------------------------------------------- *)
and op_AMOUNT (stack : stack) =
  Some (M.tmutez :: stack)

(* -------------------------------------------------------------------- *)
and op_AND (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tbool , M.Tbool  -> M.Tbool
    | M.Tbytes, M.Tbytes -> M.Tbytes
    | M.Tnat  , M.Tnat   -> M.Tnat
    | M.Tint  , M.Tnat   -> M.Tnat
    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_APPLY (stack : stack) =
  let (ty, lty), stack = Stack.pop2 stack in
  let dom, codom = Ty.check_lambda lty in
  let ty1, ty2 = Ty.check_pair dom in
  let () = Ty.check_eq ty ty1 in
  Some (M.mk_type (M.Tlambda (ty2, codom)) :: stack)

(* -------------------------------------------------------------------- *)
and op_BALANCE (stack : stack) =
  Some (M.tmutez :: stack)

(* -------------------------------------------------------------------- *)
and op_BLAKE2B (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_bytes ty in
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_CAR (stack : stack) =
  let value, stack = Stack.pop stack in
  let ty1, _ty2 = Ty.check_pair value in
  Some (ty1 :: stack)

(* -------------------------------------------------------------------- *)
and op_CDR (stack : stack) =
  let value, stack = Stack.pop stack in
  let _ty1, ty2 = Ty.check_pair value in
  Some (ty2 :: stack)

(* -------------------------------------------------------------------- *)
and op_CHAIN_ID (stack : stack) =
  Some (M.tchain_id :: stack)

(* -------------------------------------------------------------------- *)
and op_CHECK_SIGNATURE (stack : stack) =
  let (kty, sty, bty), stack = Stack.pop3 stack in
  let () = Ty.check_key kty in
  let () = Ty.check_signature sty in
  let () = Ty.check_bytes bty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_COMPARE (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in
  let () = Ty.check_eq ty1 ty2 in
  Some (M.tint :: stack)

(* -------------------------------------------------------------------- *)
and op_CONCAT (stack : stack) =
  let ty, stack = Stack.pop stack in
  let aout, stack =
    match ty.node with
    | M.Tlist ({ node = (M.Tstring | M.Tbytes) as aout }) ->
      aout, stack
    | (M.Tstring | M.Tbytes) as aout ->
      let ty2, stack = Stack.pop stack in
      let () = Ty.check_eq ty ty2 in
      aout, stack
    | _ -> raise MichelsonTypingError in
  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_CONS (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in
  let ty2 = Ty.check_list ty2 in
  let () = Ty.check_eq ty1 ty2 in
  Some (M.tlist ty1 :: stack)

(* -------------------------------------------------------------------- *)
and op_CONTRACT (stack : stack) (cty : stack1) =
  let aty, stack = Stack.pop stack in
  let () = Ty.check_address aty in
  Some (M.toption (M.tcontract cty) :: stack)

(* -------------------------------------------------------------------- *)
and op_DIG (stack : stack) (n : int) =
  let pre, post = Stack.split n stack in
  let x, post   = Stack.pop post in
  Some (x :: (pre @ post))

(* -------------------------------------------------------------------- *)
and op_DIP (stack : stack) (n : int) (code : M.code list) =
  let pre, stack = Stack.split n stack in
  match tycheck stack (M.cseq code) with
  | None -> None
  | Some substack -> Some (pre @ substack)

(* -------------------------------------------------------------------- *)
and op_DROP (stack : stack) (n : int) =
  let _, stack = Stack.split n stack in
  Some stack

(* -------------------------------------------------------------------- *)
and op_DUG (stack : stack) (n : int) =
  let x, stack = Stack.pop stack in
  let pre, post = Stack.split n stack in
  Some (pre @ x :: post)

(* -------------------------------------------------------------------- *)
and op_DUP (stack : stack) =
  let x, stack = Stack.pop stack in
  Some (x :: x :: stack)

(* -------------------------------------------------------------------- *)
and op_DUP_N (stack : stack) (n : int) =
  if n < 1 then raise MichelsonTypingError;
  let pre, stack = Stack.split (n-1) stack in
  let x, stack = Stack.pop stack in
  Some (x :: pre @ x :: stack)

(* -------------------------------------------------------------------- *)
and op_EDIV (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout1, aout2 =
    match ty1.node, ty2.node with
    | M.Tnat, M.Tnat -> M.Tnat, M.Tnat
    | M.Tnat, M.Tint -> M.Tint, M.Tnat
    | M.Tint, M.Tnat -> M.Tint, M.Tnat
    | M.Tint, M.Tint -> M.Tint, M.Tnat

    | M.Tmutez, M.Tnat   -> M.Tmutez, M.Tmutez
    | M.Tmutez, M.Tmutez -> M.Tnat  , M.Tmutez

    | _, _ -> raise MichelsonTypingError in

  Some (M.toption (M.tpair [(M.mk_type aout1); (M.mk_type aout2)]) :: stack)

(* -------------------------------------------------------------------- *)
and op_EMIT (stack : stack) =
  let _ty, stack = Stack.pop stack in (* TODO: check if type _ty is correct *)
  Some (M.toperation :: stack)

(* -------------------------------------------------------------------- *)
and op_EMPTY_BIG_MAP (stack : stack) (kty : stack1) (vty : stack1) =
  Some (M.mk_type (M.Tbig_map (kty, vty)) :: stack)

(* -------------------------------------------------------------------- *)
and op_EMPTY_MAP (stack : stack) (kty : stack1) (vty : stack1) =
  Some (M.mk_type (M.Tmap (kty, vty)) :: stack)

(* -------------------------------------------------------------------- *)
and op_EQ (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_EXEC (stack : stack) =
  let (ty, lty), stack = Stack.pop2 stack in
  let dom, codom = Ty.check_lambda lty in
  let () = Ty.check_eq ty dom in
  Some (codom :: stack)

(* -------------------------------------------------------------------- *)
and op_FAILWITH (stack : stack) =
  let _ty, _stack = Stack.pop stack in
  None

(* -------------------------------------------------------------------- *)
and op_GE (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_GET (stack : stack) =
  let (kty, mty), stack = Stack.pop2 stack in
  let kty', vty =
    match mty.node with
    | M.Tmap     (kty, vty) -> (kty, vty)
    | M.Tbig_map (kty, vty) -> (kty, vty)
    | _ -> raise MichelsonTypingError in
  let () = Ty.check_eq kty kty' in
  Some (M.toption vty :: stack)

(* -------------------------------------------------------------------- *)
and op_GET_N (stack : stack) (n : int) =
  let ty, stack = Stack.pop stack in

  let rec unpairR n ty =
    if   n <= 0
    then ty
    else unpairR (n-1) (snd (Ty.check_pair ty)) in

  let ty = unpairR (n / 2) ty in
  let ty = if n mod 2 <> 0 then fst (Ty.check_pair ty) else ty in

  Some (ty :: stack)

(* -------------------------------------------------------------------- *)
and op_GET_AND_UPDATE (stack : stack) =
  let (kty, vty, cty), stack = Stack.pop3 stack in

  begin
    match kty.node, vty.node, cty.node with
    | _, M.Toption ovty, (M.Tmap (kty', vty') | M.Tbig_map (kty', vty')) ->
      Ty.check_eq kty kty';
      Ty.check_eq ovty vty'
    | _ -> raise MichelsonTypingError
  end;

  Some (vty :: cty :: stack)

(* -------------------------------------------------------------------- *)
and op_GT (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_HASH_KEY (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_key ty in
  Some (M.tkey_hash :: stack)

(* -------------------------------------------------------------------- *)
and op_IF (stack : stack) (ct : M.code list) (ce : M.code list) =
  let cond, stack = Stack.pop stack in

  Ty.check_bool cond;

  let stt = tycheck stack (M.cseq ct) in
  let ste = tycheck stack (M.cseq ce) in

  Stack.merge stt ste

(* -------------------------------------------------------------------- *)
and op_IF_CONS (stack : stack) (ccons : M.code list) (cnil : M.code list) =
  let cond, stack = Stack.pop stack in

  let ty = Ty.check_list cond in

  let scons = tycheck (ty :: M.tlist ty :: stack) (M.cseq ccons) in
  let snil  = tycheck stack (M.cseq cnil) in

  Stack.merge scons snil

(* -------------------------------------------------------------------- *)
and op_IF_LEFT (stack : stack) (cl : M.code list) (cr : M.code list) =
  let cond, stack = Stack.pop stack in

  let tyl, tyr = Ty.check_or cond in

  let sl = tycheck (tyl :: stack) (M.cseq cl) in
  let sr = tycheck (tyr :: stack) (M.cseq cr) in

  Stack.merge sl sr

(* -------------------------------------------------------------------- *)
and op_IF_NONE (stack : stack) (cnone : M.code list) (csome : M.code list) =
  let cond, stack = Stack.pop stack in

  let ty = Ty.check_option cond in

  let snone = tycheck stack (M.cseq cnone) in
  let ssome = tycheck (ty :: stack) (M.cseq csome) in

  Stack.merge snone ssome

(* -------------------------------------------------------------------- *)
and op_IMPLICIT_ACCOUNT (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_key_hash ty in
  Some (M.tcontract M.tunit :: stack)

(* -------------------------------------------------------------------- *)
and op_NAT (stack : stack) =
  let ty, stack = Stack.pop stack in
  begin match ty.node with
    | M.Tbytes -> ()
    | _ -> raise MichelsonTypingError end;
  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_INT (stack : stack) =
  let ty, stack = Stack.pop stack in
  begin match ty.node with
    | M.Tbytes | M.Tnat | M.Tbls12_381_fr -> ()
    | _ -> raise MichelsonTypingError end;
  Some (M.tint :: stack)

(* -------------------------------------------------------------------- *)
and op_BYTES (stack : stack) =
  let ty, stack = Stack.pop stack in
  begin match ty.node with
    | M.Tnat | M.Tint -> ()
    | _ -> raise MichelsonTypingError end;
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_ISNAT (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.toption M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_ITER (stack : stack) (code : M.code list) =
  let ty, stack = Stack.pop stack in
  let inty =
    match ty.node with
    | M.Tlist ty ->
      ty
    | M.Tset kty ->
      kty
    | M.Tmap (kty, vty) ->
      M.tpair [kty; vty]
    | _ -> raise MichelsonTypingError in
  let substack = tycheck (inty :: stack) (M.cseq code) in
  Stack.merge (Some stack) substack

(* -------------------------------------------------------------------- *)
and op_JOIN_TICKETS (stack : stack) =
  let ty, stack = Stack.pop stack in
  let ty1, ty2 = Ty.check_pair ty in
  let ty1 = Ty.check_ticket ty1 in
  let ty2 = Ty.check_ticket ty2 in
  let () = Ty.check_eq ty1 ty2 in
  Some (M.toption (M.tticket ty1) :: stack)

(* -------------------------------------------------------------------- *)
and op_KECCAK (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_bytes ty in
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_LAMBDA
    (stack : stack) (dom : stack1) (codom : stack1) (code : M.code list)
  =
  let substack = tycheck [dom] (M.cseq code) in
  begin match substack with
    | Some [codom'] -> Ty.check_eq codom codom'
    | _ -> raise MichelsonTypingError end;
  Some (M.mk_type (M.Tlambda (dom, codom)) :: stack)

(* -------------------------------------------------------------------- *)
and op_LE (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_LT (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_LEFT (stack : stack) (tyr : stack1) =
  let tyl, stack = Stack.pop stack in
  Some (M.tor tyl tyr :: stack)

(* -------------------------------------------------------------------- *)
and op_LEVEL (stack : stack) =
  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_MIN_BLOCK_TIME (stack : stack) =
  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_LOOP (stack : stack) (code : M.code list) =
  let cond, stack = Stack.pop stack in
  let () = Ty.check_bool cond in
  let substack = tycheck stack (M.cseq code) in
  let substack =
    match substack with
    | None -> None
    | Some substack ->
      let cond2, substack = Stack.pop substack in
      let () = Ty.check_bool cond2 in
      Some substack in
  Stack.merge (Some stack) substack

(* -------------------------------------------------------------------- *)
and op_LOOP_LEFT (stack : stack) (code : M.code list) =
  let cond, stack = Stack.pop stack in
  let ty1, ty2 = Ty.check_or cond in
  let substack = tycheck (ty1 :: stack) (M.cseq code) in
  let substack =
    match substack with
    | None -> None
    | Some substack ->
      let cond2, substack = Stack.pop substack in
      let () = Ty.check_eq cond cond2 in
      Some (ty2 :: substack) in
  Stack.merge (Some (ty2 :: stack)) substack

(* -------------------------------------------------------------------- *)
and op_LSL (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tnat, M.Tnat -> M.Tnat
    | M.Tbytes, M.Tnat -> M.Tbytes
    | _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_LSR (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tnat, M.Tnat -> M.Tnat
    | M.Tbytes, M.Tnat -> M.Tbytes
    | _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_MAP (stack : stack) (code : M.code list) =
  let ty, stack = Stack.pop stack in
  let inty, mkaout =
    match ty.node with
    | M.Tlist ty ->
      (ty, M.tlist)
    | M.Tmap (kty, vty) ->
      (M.tpair [kty; vty], (M.tmap kty))
    | M.Toption ty ->
      (ty, M.toption)
    | _ -> raise MichelsonTypingError in
  let substack = tycheck (inty :: stack) (M.cseq code) in
  let outty, substack =
    match substack with
    | None -> raise MichelsonTypingError
    | Some substack -> Stack.pop substack in
  Stack.merge
    (Some (mkaout outty :: stack   ))
    (Some (mkaout outty :: substack))

(* -------------------------------------------------------------------- *)
and op_MEM (stack : stack) =
  let (kty, mty), stack = Stack.pop2 stack in
  let kty' =
    match mty.node with
    | M.Tmap     (kty, _) -> kty
    | M.Tbig_map (kty, _) -> kty
    | M.Tset     kty      -> kty
    | _ -> raise MichelsonTypingError in
  let () = Ty.check_eq kty kty' in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_MUL (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tnat, M.Tnat -> M.Tnat
    | M.Tnat, M.Tint -> M.Tint
    | M.Tint, M.Tnat -> M.Tint
    | M.Tint, M.Tint -> M.Tint

    | M.Tmutez, M.Tnat -> M.Tmutez
    | M.Tnat, M.Tmutez -> M.Tmutez

    | M.Tbls12_381_g1, M.Tbls12_381_fr -> M.Tbls12_381_g1
    | M.Tbls12_381_g2, M.Tbls12_381_fr -> M.Tbls12_381_g2
    | M.Tbls12_381_fr, M.Tbls12_381_fr -> M.Tbls12_381_fr

    | M.Tnat, M.Tbls12_381_fr -> M.Tbls12_381_fr
    | M.Tint, M.Tbls12_381_fr -> M.Tbls12_381_fr
    | M.Tbls12_381_fr, M.Tnat -> M.Tbls12_381_fr
    | M.Tbls12_381_fr, M.Tint -> M.Tbls12_381_fr

    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_NIL (stack : stack) (ty : stack1) =
  Some (M.tlist ty :: stack)

(* -------------------------------------------------------------------- *)
and op_NEG (stack : stack) =
  let ty, stack = Stack.pop stack in
  let aout =
    match ty.node with
    | M.Tnat -> M.Tint
    | M.Tint -> M.Tint
    | M.Tbls12_381_g1 -> M.Tbls12_381_g1
    | M.Tbls12_381_g2 -> M.Tbls12_381_g2
    | M.Tbls12_381_fr -> M.Tbls12_381_fr
    | _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_NEQ (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_int ty in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_NONE (stack : stack) (ty : stack1) =
  Some (M.toption ty :: stack)

(* -------------------------------------------------------------------- *)
and op_NOT (stack : stack) =
  let ty, stack = Stack.pop stack in

  let aout =
    match ty.node with
    | M.Tbool  -> M.Tbool
    | M.Tbytes -> M.Tbytes
    | M.Tnat   -> M.Tnat
    | M.Tint   -> M.Tint
    | _        -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_NOW (stack : stack) =
  Some (M.ttimestamp :: stack)

(* -------------------------------------------------------------------- *)
and op_OR (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tbool , M.Tbool  -> M.Tbool
    | M.Tbytes, M.Tbytes -> M.Tbytes
    | M.Tnat  , M.Tnat   -> M.Tnat
    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_PACK (stack : stack) =
  let _ty, stack = Stack.pop stack in
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_PAIR (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in
  Some (M.tpair [ty1; ty2] :: stack)

(* -------------------------------------------------------------------- *)
and op_PAIR_N (stack : stack) (n : int) =
  if n <= 1 then raise MichelsonTypingError;

  let tys, stack = Stack.split n stack in
  let aout =
    match List.rev tys with
    | [] -> assert false
    | ty :: tys -> List.fold_right (fun x accu ->  M.tpair [x; accu]) (List.rev tys) ty in

  Some (aout :: stack)

(* -------------------------------------------------------------------- *)
and op_PAIRING_CHECK (stack : stack) =
  let ty, stack = Stack.pop stack in
  let ty1, ty2 = Ty.check_list_pair ty in
  let () = Ty.check_bls12_381_g1 ty1 in
  let () = Ty.check_bls12_381_g2 ty2 in
  Some (M.tbool :: stack)

(* -------------------------------------------------------------------- *)
and op_PUSH (stack : stack) ((ty, _): stack1 * M.data) =
  Some (ty :: stack)

(* -------------------------------------------------------------------- *)
and op_READ_TICKET (stack : stack) =
  let ty, stack = Stack.pop stack in
  let ty = Ty.check_ticket ty in
  Some (
    M.tpair [M.taddress; (M.tpair [ty; M.tnat])]
    :: M.tticket ty :: stack)

(* -------------------------------------------------------------------- *)
and op_RIGHT (stack : stack) (tyl : stack1) =
  let tyr, stack = Stack.pop stack in
  Some (M.tor tyl tyr :: stack)

(* -------------------------------------------------------------------- *)
and op_SAPLING_EMPTY_STATE (stack : stack) (ms : int) =
  Some (M.mk_type (M.Tsapling_state ms) :: stack)

(* -------------------------------------------------------------------- *)
and op_SAPLING_VERIFY_UPDATE (stack : stack) =
  let (tx, st), stack = Stack.pop2 stack in
  let ms1 = Ty.check_sapling_tx tx in
  let ms2 = Ty.check_sapling_st st in
  if ms1 <> ms2 then
    raise MichelsonTypingError;
  Some (M.toption (M.tpair [M.tint; st]) :: stack)

(* -------------------------------------------------------------------- *)
and op_SELF_ADDRESS (stack : stack) =
  Some (M.taddress :: stack)

(* -------------------------------------------------------------------- *)
and op_SENDER (stack : stack) =
  Some (M.taddress :: stack)

(* -------------------------------------------------------------------- *)
and op_SET (stack : stack) (ty : stack1) =
  Some (M.tset ty :: stack)

(* -------------------------------------------------------------------- *)
and op_SET_DELEGATE (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_key_hash (Ty.check_option ty) in
  Some (M.toperation :: stack)

(* -------------------------------------------------------------------- *)
and op_SHA256 (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_bytes ty in
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_SHA512 (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_bytes ty in
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_SHA3 (stack : stack) =
  let ty, stack = Stack.pop stack in
  let () = Ty.check_bytes ty in
  Some (M.tbytes :: stack)

(* -------------------------------------------------------------------- *)
and op_SIZE (stack : stack) =
  let ty, stack = Stack.pop stack in

  begin match ty.node with
    | M.Tset _ | M.Tmap _ | M.Tlist _ | M.Tstring | M.Tbytes -> ()
    | _ -> raise MichelsonTypingError end;

  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_SLICE (stack : stack) =
  let (i1, i2, ty), stack = Stack.pop3 stack in
  let () = Ty.check_nat i1 in
  let () = Ty.check_nat i2 in
  begin match ty.node with
    | M.Tstring | M.Tbytes -> ()
    | _ -> raise MichelsonTypingError end;
  Some (M.toption ty :: stack)

(* -------------------------------------------------------------------- *)
and op_SOME (stack : stack) =
  let ty, stack = Stack.pop stack in
  Some (M.toption ty :: stack)

(* -------------------------------------------------------------------- *)
and op_SOURCE (stack : stack) =
  Some (M.taddress :: stack)

(* -------------------------------------------------------------------- *)
and op_SPLIT_TICKET (stack : stack) =
  let (ty, typ), stack = Stack.pop2 stack in
  let ty = Ty.check_ticket ty in
  let typ1, typ2 = Ty.check_pair typ in
  let () = Ty.check_nat typ1 in
  let () = Ty.check_nat typ2 in
  Some (M.toption (M.tpair [(M.tticket ty); (M.tticket ty)]) :: stack)

(* -------------------------------------------------------------------- *)
and op_SWAP (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in
  Some (ty2 :: ty1 :: stack)

(* -------------------------------------------------------------------- *)
and op_SUB (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in
  let aout =
    match ty1.node, ty2.node with
    | M.Tnat, M.Tnat -> M.Tint
    | M.Tnat, M.Tint -> M.Tint
    | M.Tint, M.Tnat -> M.Tint
    | M.Tint, M.Tint -> M.Tint

    | M.Ttimestamp, M.Tint       -> M.Ttimestamp
    | M.Ttimestamp, M.Ttimestamp -> M.Tint

    | M.Tmutez, M.Tmutez -> M.Tmutez

    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

and op_SUB_MUTEZ (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in
  let aout =
    match ty1.node, ty2.node with

    | M.Tmutez, M.Tmutez -> M.Toption M.tmutez

    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and op_TICKET (stack : stack) =
  let (ty, tl), stack = Stack.pop2 stack in
  let () = Ty.check_nat tl in
  Some (M.toption (M.tticket ty) :: stack)

(* -------------------------------------------------------------------- *)
and op_TOTAL_VOTING_POWER (stack : stack) =
  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_TRANSFER_TOKENS (stack : stack) =
  let (ty, mty, cty), stack = Stack.pop3 stack in
  let ()  = Ty.check_mutez mty in
  let cty = Ty.check_contract cty in
  let ()  = Ty.check_eq ty cty in
  Some (M.toperation :: stack)

(* -------------------------------------------------------------------- *)
and op_UNIT (stack : stack) =
  Some (M.tunit :: stack)

(* -------------------------------------------------------------------- *)
and op_UNPACK (stack : stack) (ty : stack1) =
  Some (M.toption ty :: stack)

(* -------------------------------------------------------------------- *)
and op_UNPAIR (stack : stack) =
  let ty, stack = Stack.pop stack in
  let ty1, ty2 = Ty.check_pair ty in
  Some (ty1 :: ty2 :: stack)

(* -------------------------------------------------------------------- *)
and op_UNPAIR_N (stack : stack) (n : int) =
  if n < 2 then raise MichelsonTypingError;

  let ty, stack = Stack.pop stack in

  let rec doit acc ty n =
    if n <= 1 then
      List.rev acc @ [ty]
    else
      let ty1, ty = Ty.check_pair ty in
      doit (ty1::acc) ty (n-1) in

  Some (doit [] ty n @ stack)

(* -------------------------------------------------------------------- *)
and op_UPDATE (stack : stack) =
  let (kty, vty, cty), stack = Stack.pop3 stack in

  begin
    match kty.node, vty.node, cty.node with
    | _, M.Tbool, M.Tset kty' ->
      Ty.check_eq kty kty'
    | _, M.Toption ovty, (M.Tmap (kty', vty') | M.Tbig_map (kty', vty')) ->
      Ty.check_eq kty kty';
      Ty.check_eq ovty vty'
    | _ -> raise MichelsonTypingError
  end;

  Some (cty :: stack)

(* -------------------------------------------------------------------- *)
and op_UPDATE_N (stack : stack) (n : int) =
  let (tys, tyd), stack = Stack.pop2 stack in
  let aout = tyd :: stack in

  let rec unpairR n tyd =
    if   n <= 0
    then tyd
    else unpairR (n-1) (snd (Ty.check_pair tyd)) in

  let tyd = unpairR (n / 2) tyd in
  let tyd = if n mod 2 <> 0 then fst (Ty.check_pair tyd) else tyd in

  Ty.check_eq tys tyd; Some aout

(* -------------------------------------------------------------------- *)
and op_VIEW (stack : stack) (ty : stack1) =
  let _arg_ty, stack = Stack.pop stack in
  let addr_ty, stack = Stack.pop stack in

  let _ = Ty.check_address addr_ty in
  Some (M.toption ty :: stack)

(* -------------------------------------------------------------------- *)
and op_VOTING_POWER (stack : stack) =
  let kty, stack = Stack.pop stack in
  let () = Ty.check_key_hash kty in

  Some (M.tnat :: stack)

(* -------------------------------------------------------------------- *)
and op_XOR (stack : stack) =
  let (ty1, ty2), stack = Stack.pop2 stack in

  let aout =
    match ty1.node, ty2.node with
    | M.Tbool , M.Tbool  -> M.Tbool
    | M.Tnat  , M.Tnat   -> M.Tnat
    | M.Tbytes, M.Tbytes -> M.Tbytes
    | _, _ -> raise MichelsonTypingError in

  Some (M.mk_type aout :: stack)

(* -------------------------------------------------------------------- *)
and tycheck_r (stack : stack) (code : M.code_node) : stack option =
  (* Format.eprintf "%a@." pp_stack stack; *)

  match code with
  | SEQ cs ->
    List.fold_left (fun stack c ->
        match stack with
        | None -> None
        | Some stack -> tycheck stack c) (Some stack) cs

  | APPLY ->
    op_APPLY stack

  | EXEC ->
    op_EXEC stack

  | FAILWITH ->
    op_FAILWITH stack

  | IF (ct, ce) ->
    op_IF stack ct ce

  | IF_CONS (ccons, cnil) ->
    op_IF_CONS stack ccons cnil

  | IF_LEFT (cl, cr) ->
    op_IF_LEFT stack cl cr

  | IF_NONE (cnone, csome) ->
    op_IF_NONE stack cnone csome

  | ITER c ->
    op_ITER stack c

  | LAMBDA (dom, codom, c) ->
    op_LAMBDA stack dom codom c

  | LOOP c ->
    op_LOOP stack c

  | LOOP_LEFT c ->
    op_LOOP_LEFT stack c

  (* Stack manipulation *)
  | DIG n ->
    op_DIG stack n

  | DIP (n, c) ->
    op_DIP stack n c

  | DROP n ->
    op_DROP stack n

  | DUG n ->
    op_DUG stack n

  | DUP ->
    op_DUP stack

  | DUP_N n ->
    op_DUP_N stack n

  | PUSH (ty, v) ->
    op_PUSH stack (ty, v)

  | SWAP ->
    op_SWAP stack

  (* Arthmetic operations *)
  | ABS ->
    op_ABS stack

  | ADD ->
    op_ADD stack

  | COMPARE ->
    op_COMPARE stack

  | EDIV ->
    op_EDIV stack

  | EQ ->
    op_EQ stack

  | GE ->
    op_GE stack

  | GT ->
    op_GT stack

  | NAT ->
    op_NAT stack

  | INT ->
    op_INT stack

  | BYTES ->
    op_BYTES stack

  | ISNAT ->
    op_ISNAT stack

  | LE ->
    op_LE stack

  | LSL ->
    op_LSL stack

  | LSR ->
    op_LSR stack

  | LT ->
    op_LT stack

  | MUL ->
    op_MUL stack

  | NEG ->
    op_NEG stack

  | NEQ ->
    op_NEQ stack

  | SUB ->
    op_SUB stack

  | SUB_MUTEZ ->
    op_SUB_MUTEZ stack

  (* Boolean operations *)
  | AND ->
    op_AND stack

  | NOT ->
    op_NOT stack

  | OR ->
    op_OR stack

  | XOR ->
    op_XOR stack

  (* Cryptographic operations *)
  | BLAKE2B ->
    op_BLAKE2B stack

  | CHECK_SIGNATURE ->
    op_CHECK_SIGNATURE stack

  | HASH_KEY ->
    op_HASH_KEY stack

  | KECCAK ->
    op_KECCAK stack

  | PAIRING_CHECK ->
    op_PAIRING_CHECK stack

  | SAPLING_EMPTY_STATE ty ->
    op_SAPLING_EMPTY_STATE stack ty

  | SAPLING_VERIFY_UPDATE ->
    op_SAPLING_VERIFY_UPDATE stack

  | SHA256 ->
    op_SHA256 stack

  | SHA512 ->
    op_SHA512 stack

  | SHA3 ->
    op_SHA3 stack

  (* Blockchain operations *)
  | ADDRESS ->
    op_ADDRESS stack

  | AMOUNT ->
    op_AMOUNT stack

  | BALANCE ->
    op_BALANCE stack

  | CHAIN_ID ->
    op_CHAIN_ID stack

  | CONTRACT (ty, _) ->
    op_CONTRACT stack ty

  | CREATE_CONTRACT _  ->
    assert false

  | EMIT _ ->
    op_EMIT stack

  | IMPLICIT_ACCOUNT ->
    op_IMPLICIT_ACCOUNT stack

  | LEVEL ->
    op_LEVEL stack

  | MIN_BLOCK_TIME ->
    op_MIN_BLOCK_TIME stack

  | NOW ->
    op_NOW stack

  | SELF _ ->
    assert false

  | SELF_ADDRESS ->
    op_SELF_ADDRESS stack

  | SENDER ->
    op_SENDER stack

  | SET_DELEGATE ->
    op_SET_DELEGATE stack

  | SOURCE ->
    op_SOURCE stack

  | TOTAL_VOTING_POWER ->
    op_TOTAL_VOTING_POWER stack

  | TRANSFER_TOKENS ->
    op_TRANSFER_TOKENS stack

  | VOTING_POWER ->
    op_VOTING_POWER stack

  (* Operations on data structures *)
  | CAR ->
    op_CAR stack

  | CDR ->
    op_CDR stack

  | CONCAT ->
    op_CONCAT stack

  | CONS ->
    op_CONS stack

  | EMPTY_BIG_MAP (kty, vty) ->
    op_EMPTY_BIG_MAP stack kty vty

  | EMPTY_MAP (kty, vty) ->
    op_EMPTY_MAP stack kty vty

  | EMPTY_SET ty ->
    op_SET stack ty

  | GET ->
    op_GET stack

  | GET_N n ->
    op_GET_N stack n

  | GET_AND_UPDATE ->
    op_GET_AND_UPDATE stack

  | LEFT tyr ->
    op_LEFT stack tyr

  | MAP c  ->
    op_MAP stack c

  | MEM ->
    op_MEM stack

  | NEVER ->
    None

  | NIL ty ->
    op_NIL stack ty

  | NONE ty ->
    op_NONE stack ty

  | PACK ->
    op_PACK stack

  | PAIR ->
    op_PAIR stack

  | PAIR_N n ->
    op_PAIR_N stack n

  | RIGHT tyl ->
    op_RIGHT stack tyl

  | SIZE ->
    op_SIZE stack

  | SLICE ->
    op_SLICE stack

  | SOME ->
    op_SOME stack

  | UNIT ->
    op_UNIT stack

  | UNPACK ty ->
    op_UNPACK stack ty

  | UNPAIR ->
    op_UNPAIR stack

  | UNPAIR_N n ->
    op_UNPAIR_N stack n

  | UPDATE ->
    op_UPDATE stack

  | UPDATE_N n ->
    op_UPDATE_N stack n

  (* Operations on tickets *)
  | JOIN_TICKETS ->
    op_JOIN_TICKETS stack

  | READ_TICKET ->
    op_READ_TICKET stack

  | SPLIT_TICKET ->
    op_SPLIT_TICKET stack

  | TICKET ->
    op_TICKET stack

  (* Other *)

  | CAST _ ->
    Some stack                (* ? *)

  | RENAME                   -> (* Unused? *) assert false

  | VIEW (_, ty) ->
    op_VIEW stack ty

  | OPEN_CHEST -> assert false (* TODO *)

  (* Macro *)

  | CAR_N _ -> assert false (* TODO *)

  | CDR_N _ -> assert false (* TODO *)

  (* Custom *)

  | CUSTOM _ -> assert false


(* -------------------------------------------------------------------- *)
and tycheck (stack : stack) (code : M.code) : stack option =
  let stk = tycheck_r stack code.node in
  let stk = Stack.merge stk !(code.type_) in
  code.type_ := stk; stk
