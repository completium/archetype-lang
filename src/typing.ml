(* -------------------------------------------------------------------- *)
open Ident
open Tools
open Location

module L  = Location
module PT = ParseTree
module M  = Model

(* -------------------------------------------------------------------- *)
type type_ =
  | TUnit
  | TBool
  | TInt
  | TRational
  | TString
  | TAddress
  | TDuration
  | TDate
  | TCurrency
  | TTuple     of type_ list
  | TContainer of (M.container * type_)
  | TAsset     of ident

(* -------------------------------------------------------------------- *)
type argtype = [`Type of type_ | `Effect of ident]

(* -------------------------------------------------------------------- *)
module Type : sig
  val as_container : type_ -> (M.container * type_) option
  val as_asset     : type_ -> ident option

  val up : type_ -> type_ -> type_ option
end = struct
  let as_container = function TContainer x -> Some x | _ -> None
  let as_asset     = function TAsset     x -> Some x | _ -> None

  let up ty1 ty2 =
    if ty1 = ty2 then Some ty1 else None
end

(* -------------------------------------------------------------------- *)
type error_desc =
  | DivergentExpr
  | AssetExpected
  | CollectionExpected
  | IncompatibleTypes                  of type_ * type_
  | UnknownProcedure                   of ident
  | UnknownField                       of ident * ident
  | InvalidNumberOfArguments           of int * int
  | InvalidEffect
  | OpInRecordLiteral
  | MixedAnonInRecordLiteral
  | CannotInferAnonRecord
  | InvalidFieldsCountInRecordLiteral
  | DuplicatedFieldInRecordLiteral     of ident
  | MissingFieldInRecordLiteral        of ident
  | UnknownFieldName                   of ident
  | MixedFieldNames                    of ident list

type error = L.t * error_desc

(* -------------------------------------------------------------------- *)
type procsig = {
  psl_sig  : argtype list;
  psl_ret  : type_;
}

(* -------------------------------------------------------------------- *)
type assetdecl = {
  as_name   : ident;
  as_fields : (ident * type_) list;
}

(* -------------------------------------------------------------------- *)
let procsig_of_operator (_op : PT.operator) : procsig =
  assert false

(* -------------------------------------------------------------------- *)
module Env : sig
  type t

  val emit_error : t -> error -> unit

  module Local : sig
    val push   : t -> ident * type_ -> t
    val lookup : t -> ident -> (ident * type_) option
  end

  module Proc : sig
    val lookup : t -> ident -> procsig option
  end

  module Asset : sig
    val lookup  : t -> ident -> assetdecl option
    val get     : t -> ident -> assetdecl
    val byfield : t -> ident -> (assetdecl * type_) option
  end
end = struct
  type t = unit

  let emit_error (_env : t) (_e : error) =
    assert false

  module Local = struct
    let push   = assert false
    let lookup = assert false
  end

  module Proc = struct
    let lookup = assert false
  end

  module Asset = struct
    let lookup =
      assert false

    let get (env : t) (name : ident) =
      get (lookup env name)

    let byfield =
      assert false
  end
end

type env = Env.t

(* -------------------------------------------------------------------- *)
let for_literal (env : env) (topv : PT.literal loced) : type_ =
  match unloc topv with
  | Lbool _ ->
      TBool

  | Lnumber _ ->
      TInt

  | Lrational _ ->
      TRational 

  | Lstring _ ->
      TString

  | Ltz _ ->
      TCurrency

  | Laddress _ ->
      TAddress

  | Lduration _ ->
      TDuration

  | Ldate _ ->
      TDate

(* -------------------------------------------------------------------- *)
let rec for_expr_dvg (env : env) ?(ety : type_ option) (tope : PT.expr) : type_ option =
  let module E = struct exception Bailout end in

  let bailout = fun () -> raise E.Bailout in

  let infty () =
    match unloc tope with
    | Eterm (_, x) ->
        assert false
  
    | Eliteral v ->
        Some (for_literal env (mkloc (loc tope) v))
  
    | Earray es ->
        assert false
  
    | Erecord fields -> begin
        let module E = struct
          type state = {
            hasupdate : bool;
            fields    : ident list;
            anon      : bool;
          }

          let state0 = {
            hasupdate = true; fields = []; anon = false;
          }
        end in

        let is_update = function
          | (None | Some (PT.ValueAssign, _)) -> false
          |  _ -> true in

        let infos = List.fold_left (fun state (fname, _) ->
            E.{ hasupdate = state.hasupdate || is_update fname;
                fields    = fold_option
                              (fun names (_, name)-> unloc name :: names)
                              state.fields fname;
                anon      = state.anon || is_none fname; })
          E.state0 fields in

        if infos.E.hasupdate then
          Env.emit_error env (loc tope, OpInRecordLiteral);

        if infos.E.anon && not (List.is_empty (infos.E.fields)) then begin
          Env.emit_error env (loc tope, MixedAnonInRecordLiteral);
          bailout ()
        end;

        if infos.E.anon || List.is_empty fields then
          match map_option Type.as_asset ety with
          | None | Some None ->
              Env.emit_error env (loc tope, CannotInferAnonRecord);
              bailout ()

          | Some (Some asset) ->
              let asset = Env.Asset.get env asset in

              let ne, ng = List.length fields, List.length asset.as_fields in
              
              if ne <> ng then begin
                Env.emit_error env (loc tope, InvalidFieldsCountInRecordLiteral);
                bailout ()
              end;
              
              List.iter2 (fun (_, fe) (_, fty) ->
                  ignore (for_expr env ~ety:fty fe)
              ) fields asset.as_fields;
              
              ety

        else begin
          let fmap =
            List.fold_left (fun fmap (fname, e) ->
              let fname = unloc (snd (get fname)) in

              Mid.update fname (function
                  | None -> begin
                      let asset = Env.Asset.byfield env fname in
                      if is_none asset then begin
                        let err = UnknownFieldName fname in
                        Env.emit_error env (loc tope, err)
                      end; Some (asset, [e])
                    end

                  | Some (asset, es) ->
                      if List.length es = 1 then begin
                        let err = DuplicatedFieldInRecordLiteral fname in
                        Env.emit_error env (loc tope, err)
                      end; Some (asset, e :: es)) fmap
            ) Mid.empty fields
          in

          let assets =
            Mid.fold (fun _ (asset, _) assets ->
              fold_option
                (fun assets (asset, _) -> asset :: assets)
                assets asset
            ) fmap []

          in

          Mid.iter (fun _ (asset, es) ->
            let aty = map_option snd asset in
            List.iter (fun e -> ignore (for_expr env ?ety:aty e)) es
          ) fmap;

          let asset =
            match assets with
            | [] ->
                bailout ()
  
            | _ :: _ :: _ ->
                let err = MixedFieldNames (List.map (fun x -> x.as_name) assets) in
                Env.emit_error env (loc tope, err); bailout ()
  
            | [asset] ->
                List.iter (fun (fname, _) ->
                  if is_none (Mid.find_opt fname fmap) then
                    let err = MissingFieldInRecordLiteral fname in
                    Env.emit_error env (loc tope, err)) asset.as_fields;
                asset

          in Some (TAsset asset.as_name)
        end
      end
  
    | Etuple es ->
        Some (TTuple (List.map (for_expr env) es))
  
    | Edot (e, x) -> begin
        let ety = for_expr env e in
        let asset = Type.as_asset ety in

        match asset with
        | None ->
            Env.emit_error env (loc e, AssetExpected);
            bailout ()

        | Some asset ->

        let asset = Env.Asset.get env asset in

        match List.Exn.assoc (unloc x) asset.as_fields with
        | None ->
            let err = UnknownField (asset.as_name, unloc x) in
            Env.emit_error env (loc x, err); bailout ()

        | Some _ as fty -> fty
      end          
  
    | Eapp (f, args) -> begin
        match
          match f with
          | Fident x ->
              let ps = Env.Proc.lookup env (unloc x) in
              if is_none ps then
                Env.emit_error env (loc x, UnknownProcedure (unloc x));
              ps

          | Foperator op -> 
              Some (procsig_of_operator (unloc op))
        with None -> None | Some pdf ->

        let ne, ng = List.length pdf.psl_sig, List.length args in

        begin if ne <> ng then
          Env.emit_error env (loc tope, InvalidNumberOfArguments (ne, ng))
        else
          List.iter2
            (fun arg aty -> ignore (for_arg env aty arg))
            args pdf.psl_sig
        end;

        Some (pdf.psl_ret)
      end

    | Emethod(the, m, args) ->
        assert false
  
    | Etransfer (e, b, x) ->
        assert false
  
    | Eassign (op, lv, e) ->
        assert false
  
    | Eif (c, et, ef) -> begin
        let _   = for_expr env ~ety:TBool c in
        let ty1 = for_expr_dvg env ?ety:(map_option (fun _ -> TUnit) ef) et in
  
        match ef with
        | None ->
            Some TUnit
  
        | Some ef ->
            for_expr_dvg env ?ety:ty1 ef
      end
  
    | Erequire e | Eassert e | Efailif e ->
        let _ = for_expr env ~ety:TBool e in None

    | Ebreak ->
        (* FIXME: env: add a flag for loops *)
        Some TUnit
  
    | Efor (x, rg, body) -> begin
        let tyrg = for_expr env rg in

        begin match Type.as_container tyrg with
        | None ->
            Env.emit_error env (loc rg, CollectionExpected)

        | Some (_, ty) ->
            let env0 = Env.Local.push env (unloc x, ty) in
            ignore (for_expr_dvg env0 ~ety:TUnit body) end;

        Some TUnit
            
      end
  
    | Eseq (e1, e2) ->
        ignore (for_expr_dvg env ~ety:TUnit e1);
        for_expr_dvg env e2
  
    | Eletin (lv, e1, e2, c) ->
        assert false
  
    | Ematchwith (e, bs) ->
        assert false
  
    | Equantifier (bd, x, e) ->
        assert false
  
    | Elabel (l, e) ->
        assert false

  in

  try
    match infty (), ety with
    | None, _ | (Some _, None) ->
        None
  
    | Some infty, Some ety ->
        match Type.up infty ety with
        | Some _ as topty ->
            topty
  
        | None ->
            let err = IncompatibleTypes (infty, ety) in
            Env.emit_error env (loc tope, err); Some ety

  with E.Bailout -> ety

(* -------------------------------------------------------------------- *)
and for_expr (env : env) ?(ety : type_ option) (tope : PT.expr) : type_ =
  match for_expr_dvg env ?ety tope with
  | Some ty ->
      ty

  | None ->
      Env.emit_error env (loc tope, DivergentExpr); TUnit

(* -------------------------------------------------------------------- *)
and for_arg (env : env) (ety : argtype) (tope : PT.expr) : argtype =
  match ety with
  | `Type ety ->
      `Type (for_expr env ~ety tope)

  | `Effect name ->
      for_effect env name tope; ety

(* -------------------------------------------------------------------- *)
and for_effect (env : env) (asset : ident) (tope : PT.expr) : unit =
  match unloc tope with
  | Erecord fields ->
      assert false

  | _ ->
      Env.emit_error env (loc tope, InvalidEffect)
