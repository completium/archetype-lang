open Location
open Tools

module M = Model
module T = Michelson

let to_ir (model : M.model) : T.ir =

  let to_one_gen init f l =
    match List.rev l with
    | [] -> init
    | i::q -> List.fold_right (fun x accu -> f x accu) q i
  in

  let to_one_type (l : T.type_ list) : T.type_ = to_one_gen (T.mk_type T.Tunit) (fun x accu -> (T.mk_type (T.Tpair (x, accu)))) l in

  let to_one_data (l : T.data list) : T.data = to_one_gen (T.Dunit) (fun x accu -> (T.Dpair (x, accu))) l in

  let rec to_type ?annotation (t : M.type_) : T.type_ =
    let node =
      match t with
      | Tasset _   -> assert false
      | Tenum _    -> assert false
      | Tstate     -> assert false
      | Tbuiltin b -> begin
          match b with
          | Bunit      -> T.Tunit
          | Bbool      -> T.Tbool
          | Bint       -> T.Tint
          | Brational  -> assert false
          | Bdate      -> T.Ttimestamp
          | Bduration  -> T.Tint
          | Btimestamp -> T.Ttimestamp
          | Bstring    -> T.Tstring
          | Baddress   -> T.Taddress
          | Brole      -> T.Taddress
          | Bcurrency  -> T.Tmutez
          | Bsignature -> T.Tsignature
          | Bkey       -> T.Tkey
          | Bkeyhash   -> T.Tkey_hash
          | Bbytes     -> T.Tbytes
          | Bnat       -> T.Tnat
          | Bchainid   -> T.Tchain_id
        end
      | Tcontainer _ -> assert false
      | Tlist t      -> T.Tlist (to_type t)
      | Toption t    -> T.Toption (to_type t)
      | Ttuple lt    -> to_one_type (List.map to_type lt) |> fun x -> x.node
      | Tset t -> T.Tset (to_type t)
      | Tmap (b, k, v) -> if b then T.Tbig_map (to_type k, to_type v) else T.Tmap (to_type k, to_type v)
      | Trecord _id -> begin
          (* TODO *)
          assert false
        end
      | Tunit -> T.Tunit
      | Toperation -> T.Toperation
      | Tentrysig t -> T.Tcontract (to_type t)
      | Tstorage -> assert false
      | Tprog  _ -> assert false
      | Tvset  _ -> assert false
      | Ttrace _ -> assert false
    in
    T.mk_type ?annotation node
  in

  let rec to_data (mt : M.mterm) : T.data =
    match mt.node with
    | Munit             -> T.Dunit
    | Mbool      v      -> (if v then T.Dtrue else T.Dfalse)
    | Mint       n      -> T.Dint n
    | Mnat       n      -> T.Dint n
    | Mstring    s      -> T.Dstring s
    | Mcurrency  (v, _) -> T.Dint v
    | Maddress   v      -> T.Dstring v
    | Mdate      v      -> T.Dint (Core.date_to_timestamp v)
    | Mduration  n      -> T.Dint (Core.duration_to_timestamp n)
    | Mtimestamp v      -> T.Dint v
    | Mbytes     b      -> T.Dbytes b
    | Mnone             -> T.Dnone
    | Msome      v      -> T.Dsome (to_data v)
    | Mtuple     l      -> to_one_data (List.map to_data l)
    | Mlitset    l      -> T.Dlist (List.map to_data l)
    | Mlitlist   l      -> T.Dlist (List.map to_data l)
    | Mlitmap    l      -> T.Dplist (List.map (fun (x, y) -> to_data x, to_data y) l)
    | Muminus    v      -> to_data v |> (function | T.Dint n -> T.Dint (Big_int.mult_int_big_int (-1) n) | _ -> assert false )
    (* | Mlitrecord l -> to_data v *)
    | Mnattoint v
    | Mnattorat v
    | Minttorat v
    | Mcast (_, _, v) -> to_data v
    | _ -> Format.printf "%a@." M.pp_mterm mt; assert false

  in

  let l = List.map (
      fun (si : M.storage_item) ->
        to_type ~annotation:(unloc si.id) si.typ, to_data si.default)
      model.storage
  in

  let storage_type, storage_data =
    match l with
    | [] -> T.mk_type Tunit, T.Dunit
    | _ -> let lt, ld = List.split l in to_one_type lt, to_one_data ld
  in
  T.mk_ir (storage_type, storage_data)

let to_michelson (ir : T.ir) : T.michelson =
  let storage = ir.storage |> fst in
  let parameter = T.mk_type T.Tunit in
  let code = T.SEQ [T.CDR; T.NIL (T.mk_type Toperation); T.PAIR ] in
  T.mk_michelson storage parameter code
