open Location

module A = Ast
module W = Model_wse
module T = Mltree

let to_ident ident = unloc ident

let to_basic = function
  | A.VTbool       -> T.Tbool
  | A.VTint        -> T.Tint
  | A.VTuint       -> T.Tnat
  | A.VTrational   -> assert false
  | A.VTdate       -> T.Ttimestamp
  | A.VTduration   -> T.Tint
  | A.VTstring     -> T.Tstring
  | A.VTaddress    -> T.Taddress
  | A.VTrole       -> T.Tkey_hash
  | A.VTcurrency _ -> T.Ttez
  | A.VTkey        -> T.Tkey

let rec to_type = function
  | W.Tstorage     -> T.Tlocal "storage"
  | W.Toperations  -> T.Tlist  (T.Tlocal "operation")
  | W.Tbuiltin b   -> T.Tbasic (to_basic b)
  | W.Trecord id
  | W.Tenum id     -> T.Tlocal (to_ident id)
  | W.Ttuple types -> T.Ttuple (List.map to_type types)
  | W.Tcontainer t -> T.Tlist  (to_type t)
  | W.Tmap (k, v)  -> T.Tmap   (to_type k, to_type v)


let to_liquidity (model : W.model) : T.tree =

  let cont f x d = d @ f x in

  let add_enums =
    List.map (fun (x : W.enum_struct) ->
        let name = to_ident x.name in
        let values = List.map (fun x -> (to_ident x, None)) x.values in
        T.Dtype (T.mk_type name ~values:values))
  in

  let add_structs =
    List.map (fun (x : W.record_struct) ->
        let name = to_ident x.name in
        let fields = List.map (fun (id, t) -> (to_ident id, to_type t)) x.values in
        T.Dstruct (T.mk_struct name ~fields:fields))
  in

  let name = to_ident model.name in
  let instr : T.expr =
    Eletin (
      [
        ["ops", Tlist (Tlocal "operation")], Econtainer []
      ],
      Etuple [Evar "ops"; Evar "s"]) in
  let decls =
    []
    |> cont add_enums model.enums
    |> cont add_structs model.records
    |> (fun x -> x @
                 [T.Dtype (T.mk_type "myenum" ~values:([("First", None); ("Second", None)]));
                  T.Dstruct (T.mk_struct "storage" ~fields:[("i", Tbasic T.Tint); ("str", Tbasic T.Tstring)]);
                  T.Dfun (T.mk_fun "main" T.Entry ["params", Tbasic Tunit; "s", Tlocal "storage"] instr)])
  in
  T.mk_tree name ~decls:decls
