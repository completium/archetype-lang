(* -------------------------------------------------------------------- *)
open Ident
open Tools
open Location

module L  = Location
module PT = ParseTree
module A  = Ast
module T  = Michelson

(* -------------------------------------------------------------------- *)
module Type : sig
  val as_builtin          : A.ptyp -> A.vtyp option
  val as_container        : A.ptyp -> (A.ptyp * A.container) option
  val as_asset            : A.ptyp -> A.lident option
  val as_asset_collection : A.ptyp -> (A.lident * A.container) option
  val as_contract         : A.ptyp -> A.ptyp option
  val as_tuple            : A.ptyp -> (A.ptyp list) option
  val as_option           : A.ptyp -> A.ptyp option
  val as_set              : A.ptyp -> A.ptyp option
  val as_list             : A.ptyp -> A.ptyp option
  val as_map              : A.ptyp -> (A.ptyp * A.ptyp) option
  val as_big_map          : A.ptyp -> (A.ptyp * A.ptyp) option
  val as_iterable_big_map : A.ptyp -> (A.ptyp * A.ptyp) option
  val as_or               : A.ptyp -> (A.ptyp * A.ptyp) option
  val as_lambda           : A.ptyp -> (A.ptyp * A.ptyp) option
  val as_content_array    : A.ptyp -> A.ptyp option

  val is_asset     : A.ptyp -> bool
  val is_numeric   : A.ptyp -> bool
  val is_address   : A.ptyp -> bool
  val is_currency  : A.ptyp -> bool
  val is_primitive : A.ptyp -> bool
  val is_contract  : A.ptyp -> bool
  val is_option    : A.ptyp -> bool
  val is_set       : A.ptyp -> bool
  val is_list      : A.ptyp -> bool
  val is_map       : A.ptyp -> bool
  val is_big_map   : A.ptyp -> bool
  val is_iterable_big_map : A.ptyp -> bool
  val is_lambda    : A.ptyp -> bool
  val is_or        : A.ptyp -> bool

  module Michelson : sig
    val is_type          : A.ptyp -> bool
    val is_comparable    : A.ptyp -> bool
    val is_passable      : A.ptyp -> bool
    val is_storable      : A.ptyp -> bool
    val is_packable      : A.ptyp -> bool
    val is_big_map_value : A.ptyp -> bool
    val is_dupable       : A.ptyp -> bool
  end

  val support_eq : A.ptyp -> bool

  val equal     : A.ptyp -> A.ptyp -> bool
  val sig_equal : A.ptyp list -> A.ptyp list -> bool

  val compatible     : ?autoview:bool -> ?for_eq:bool -> from_:A.ptyp -> to_:A.ptyp -> bool
  val distance       : from_:A.ptyp -> to_:A.ptyp -> int option
  val sig_compatible : from_:A.ptyp list -> to_:A.ptyp list -> bool
  val sig_distance   : from_:A.ptyp list -> to_:A.ptyp list -> int option
  val join           : ?autoview:bool -> A.ptyp list -> A.ptyp option

  type trestr = [`MichelsonPackable]

  exception UnificationFailure

  val unify : ?restr:trestr Mint.t -> ptn:A.ptyp -> tg:A.ptyp -> A.ptyp Mint.t ref -> unit
  val subst : A.ptyp Mint.t -> A.ptyp -> A.ptyp

  val pktype : A.ptyp -> bool

  val create_tuple : A.ptyp list -> A.ptyp
end = struct
  let as_builtin   = function A.Tbuiltin   ty      -> Some ty      | _ -> None
  let as_container = function A.Tcontainer (ty, c) -> Some (ty, c) | _ -> None
  let as_asset     = function A.Tasset     x       -> Some x       | _ -> None
  let as_tuple     = function A.Ttuple     ts      -> Some ts      | _ -> None
  let as_contract  = function A.Tcontract  x       -> Some x       | _ -> None
  let as_ticket    = function A.Tticket    x       -> Some x       | _ -> None
  let as_option    = function A.Toption    t       -> Some t       | _ -> None
  let as_set       = function A.Tset       t       -> Some t       | _ -> None
  let as_list      = function A.Tlist      t       -> Some t       | _ -> None
  let as_map       = function A.Tmap       (k, v)  -> Some (k, v)  | _ -> None
  let as_big_map   = function A.Tbig_map   (a, r)  -> Some (a, r)  | _ -> None
  let as_iterable_big_map   = function A.Titerable_big_map   (a, r)  -> Some (a, r)  | _ -> None
  let as_or        = function A.Tor        (l, r)  -> Some (l, r)  | _ -> None
  let as_lambda    = function A.Tlambda    (a, r)  -> Some (a, r)  | _ -> None

  let as_content_array = function
      A.Tcontainer (ty, _) -> Some ty
    | A.Tset       t       -> Some t
    | A.Tlist      t       -> Some t
    | A.Tmap       (k, v)  -> Some (A.Ttuple [k; v])
    | A.Tbig_map   (k, v)  -> Some (A.Ttuple [k; v])
    | A.Titerable_big_map   (k, v)  -> Some (A.Ttuple [k; v])
    | _ -> None

  let as_asset_collection = function
    | A.Tcontainer (A.Tasset asset, c) -> Some (asset, c)
    | _ -> None

  let is_asset = function
    | A.Tasset _ -> true |  _ -> false

  let is_numeric = function
    | A.Tbuiltin (A.VTnat | A.VTint | A.VTrational) -> true |  _ -> false

  let is_address = function
    | A.Tbuiltin (A.VTaddress) -> true | _ -> false

  let is_currency = function
    | A.Tbuiltin (A.VTcurrency) -> true | _ -> false

  let is_primitive = function
    | A.Tbuiltin _ -> true | _ -> false

  let is_contract = function
    | A.Tcontract _ -> true | _ -> false

  let is_option = function
    | A.Toption _ -> true | _ -> false

  let is_set = function
    | A.Tset _ -> true | _ -> false

  let is_list = function
    | A.Tlist _ -> true | _ -> false

  let is_map = function
    | A.Tmap _ -> true | _ -> false

  let is_big_map = function
    | A.Tbig_map _ -> true | _ -> false

  let is_iterable_big_map = function
    | A.Titerable_big_map _ -> true | _ -> false

  let is_or = function
    | A.Tor _ -> true | _ -> false

  let is_lambda = function
    | A.Tlambda _ -> true | _ -> false

  module Michelson = struct
    let is_type t =
      match t with
      | A.Tnamed               _ -> false
      | A.Tasset               _ -> false
      | A.Trecord              _ -> true (* all record fields got a michelson type *)
      | A.Tenum                _ -> true
      | A.Tevent               _ -> true (* all record fields got a michelson type *)
      | A.Tbuiltin VTunit        -> true
      | A.Tbuiltin VTbool        -> true
      | A.Tbuiltin VTnat         -> true
      | A.Tbuiltin VTint         -> true
      | A.Tbuiltin VTrational    -> true
      | A.Tbuiltin VTdate        -> true
      | A.Tbuiltin VTduration    -> true
      | A.Tbuiltin VTstring      -> true
      | A.Tbuiltin VTaddress     -> true
      | A.Tbuiltin VTcurrency    -> true
      | A.Tbuiltin VTkey         -> true
      | A.Tbuiltin VTkeyhash     -> true
      | A.Tbuiltin VTsignature   -> true
      | A.Tbuiltin VTbytes       -> true
      | A.Tbuiltin VTchainid     -> true
      | A.Tbuiltin VTbls12_381_fr-> true
      | A.Tbuiltin VTbls12_381_g1-> true
      | A.Tbuiltin VTbls12_381_g2-> true
      | A.Tbuiltin VTnever       -> true
      | A.Tbuiltin VTchest       -> true
      | A.Tbuiltin VTchest_key   -> true
      | A.Tcontainer (_, Collection) -> false
      | A.Tcontainer _           -> true
      | A.Tset                 _ -> true
      | A.Tlist                _ -> true
      | A.Tmap                 _ -> true
      | A.Tbig_map             _ -> true
      | A.Titerable_big_map    _ -> true
      | A.Tor                  _ -> true
      | A.Tlambda              _ -> true
      | A.Ttuple               _ -> true
      | A.Toption              _ -> true
      | A.Toperation             -> true
      | A.Tcontract            _ -> true
      | A.Ttrace               _ -> false
      | A.Tticket              _ -> true
      | A.Tsapling_state       _ -> true
      | A.Tsapling_transaction _ -> true

    let rec is_comparable t =
      match t with
      | A.Tnamed               _ -> false
      | A.Tasset               _ -> false
      | A.Trecord              _ -> true (* TODO: Check if first field is comparable *)
      | A.Tenum                _ -> true
      | A.Tevent               _ -> true (* TODO: Check if first field is comparable *)
      | A.Tbuiltin VTunit        -> true
      | A.Tbuiltin VTbool        -> true
      | A.Tbuiltin VTnat         -> true
      | A.Tbuiltin VTint         -> true
      | A.Tbuiltin VTrational    -> true
      | A.Tbuiltin VTdate        -> true
      | A.Tbuiltin VTduration    -> true
      | A.Tbuiltin VTstring      -> true
      | A.Tbuiltin VTaddress     -> true
      | A.Tbuiltin VTcurrency    -> true
      | A.Tbuiltin VTkey         -> true
      | A.Tbuiltin VTkeyhash     -> true
      | A.Tbuiltin VTsignature   -> true
      | A.Tbuiltin VTbytes       -> true
      | A.Tbuiltin VTchainid     -> true
      | A.Tbuiltin VTbls12_381_fr-> false
      | A.Tbuiltin VTbls12_381_g1-> false
      | A.Tbuiltin VTbls12_381_g2-> false
      | A.Tbuiltin VTnever       -> true
      | A.Tbuiltin VTchest       -> true
      | A.Tbuiltin VTchest_key   -> true
      | A.Tcontainer           _ -> false
      | A.Tset                 _ -> false
      | A.Tlist                _ -> false
      | A.Tmap                 _ -> false
      | A.Tbig_map             _ -> false
      | A.Titerable_big_map    _ -> false
      | A.Tor           (t1, t2) -> is_comparable t1 && is_comparable t2
      | A.Tlambda              _ -> false
      | A.Ttuple               l -> List.for_all is_comparable l
      | A.Toption             ty -> is_comparable ty
      | A.Toperation             -> false
      | A.Tcontract            _ -> false
      | A.Ttrace               _ -> false
      | A.Tticket              _ -> false
      | A.Tsapling_state       _ -> false
      | A.Tsapling_transaction _ -> false

    let rec is_passable t =
      match t with
      | A.Tnamed               _ -> false
      | A.Tasset               _ -> false
      | A.Trecord              _ -> true
      | A.Tenum                _ -> true
      | A.Tevent               _ -> true
      | A.Tbuiltin VTunit        -> true
      | A.Tbuiltin VTbool        -> true
      | A.Tbuiltin VTnat         -> true
      | A.Tbuiltin VTint         -> true
      | A.Tbuiltin VTrational    -> true
      | A.Tbuiltin VTdate        -> true
      | A.Tbuiltin VTduration    -> true
      | A.Tbuiltin VTstring      -> true
      | A.Tbuiltin VTaddress     -> true
      | A.Tbuiltin VTcurrency    -> true
      | A.Tbuiltin VTkey         -> true
      | A.Tbuiltin VTkeyhash     -> true
      | A.Tbuiltin VTsignature   -> true
      | A.Tbuiltin VTbytes       -> true
      | A.Tbuiltin VTchainid     -> true
      | A.Tbuiltin VTbls12_381_fr-> true
      | A.Tbuiltin VTbls12_381_g1-> true
      | A.Tbuiltin VTbls12_381_g2-> true
      | A.Tbuiltin VTnever       -> true
      | A.Tbuiltin VTchest       -> true
      | A.Tbuiltin VTchest_key   -> true
      | A.Tcontainer (_, Collection) -> false
      | A.Tcontainer _           -> true
      | A.Tset                 _ -> true
      | A.Tlist                _ -> true
      | A.Tmap                 _ -> true
      | A.Tbig_map             _ -> true
      | A.Titerable_big_map    _ -> true
      | A.Tor                  _ -> true
      | A.Tlambda              _ -> true
      | A.Ttuple               l -> List.for_all is_passable l
      | A.Toption              _ -> true
      | A.Toperation             -> false
      | A.Tcontract            _ -> true
      | A.Ttrace               _ -> false
      | A.Tticket              t -> is_passable t
      | A.Tsapling_state       _ -> true
      | A.Tsapling_transaction _ -> true

    let rec is_storable t =
      match t with
      | A.Tnamed               _ -> false
      | A.Tasset               _ -> false
      | A.Trecord              _ -> true (* TODO: Check if all fields are storable *)
      | A.Tenum                _ -> true
      | A.Tevent               _ -> true (* TODO: Check if all fields are storable *)
      | A.Tbuiltin VTunit        -> true
      | A.Tbuiltin VTbool        -> true
      | A.Tbuiltin VTnat         -> true
      | A.Tbuiltin VTint         -> true
      | A.Tbuiltin VTrational    -> true
      | A.Tbuiltin VTdate        -> true
      | A.Tbuiltin VTduration    -> true
      | A.Tbuiltin VTstring      -> true
      | A.Tbuiltin VTaddress     -> true
      | A.Tbuiltin VTcurrency    -> true
      | A.Tbuiltin VTkey         -> true
      | A.Tbuiltin VTkeyhash     -> true
      | A.Tbuiltin VTsignature   -> true
      | A.Tbuiltin VTbytes       -> true
      | A.Tbuiltin VTchainid     -> true
      | A.Tbuiltin VTbls12_381_fr-> true
      | A.Tbuiltin VTbls12_381_g1-> true
      | A.Tbuiltin VTbls12_381_g2-> true
      | A.Tbuiltin VTnever       -> true
      | A.Tbuiltin VTchest       -> true
      | A.Tbuiltin VTchest_key   -> true
      | A.Tcontainer (_, Collection) -> false
      | A.Tcontainer _           -> true
      | A.Tset                 t -> is_storable t
      | A.Tlist                t -> is_storable t
      | A.Tmap            (k, v) -> List.for_all is_storable [k; v]
      | A.Tbig_map        (k, v) -> List.for_all is_storable [k; v]
      | A.Titerable_big_map(k, v)-> List.for_all is_storable [k; v]
      | A.Tor             (l, r) -> List.for_all is_storable [l; r]
      | A.Tlambda              _ -> true
      | A.Ttuple               l -> List.for_all is_storable l
      | A.Toption              t -> is_storable t
      | A.Toperation             -> false
      | A.Tcontract            _ -> false
      | A.Tticket              t -> is_storable t
      | A.Ttrace               _ -> false
      | A.Tsapling_state       _ -> true
      | A.Tsapling_transaction _ -> true

    let rec is_packable t =
      match t with
      | A.Tnamed               _ -> false
      | A.Tasset               _ -> false
      | A.Trecord              _ -> true (* TODO: check if all fields are packable *)
      | A.Tenum                _ -> true
      | A.Tevent               _ -> true (* TODO: check if all fields are packable *)
      | A.Tbuiltin VTunit        -> true
      | A.Tbuiltin VTbool        -> true
      | A.Tbuiltin VTnat         -> true
      | A.Tbuiltin VTint         -> true
      | A.Tbuiltin VTrational    -> true
      | A.Tbuiltin VTdate        -> true
      | A.Tbuiltin VTduration    -> true
      | A.Tbuiltin VTstring      -> true
      | A.Tbuiltin VTaddress     -> true
      | A.Tbuiltin VTcurrency    -> true
      | A.Tbuiltin VTkey         -> true
      | A.Tbuiltin VTkeyhash     -> true
      | A.Tbuiltin VTsignature   -> true
      | A.Tbuiltin VTbytes       -> true
      | A.Tbuiltin VTchainid     -> true
      | A.Tbuiltin VTbls12_381_fr-> true
      | A.Tbuiltin VTbls12_381_g1-> true
      | A.Tbuiltin VTbls12_381_g2-> true
      | A.Tbuiltin VTnever       -> true
      | A.Tbuiltin VTchest       -> true
      | A.Tbuiltin VTchest_key   -> true
      | A.Tcontainer (_, Collection) -> false
      | A.Tcontainer _           -> true
      | A.Tset                 t -> is_packable t
      | A.Tlist                t -> is_packable t
      | A.Tmap            (k, v) -> List.for_all is_packable [k; v]
      | A.Tbig_map             _ -> false
      | A.Titerable_big_map    _ -> false
      | A.Tor             (l, r) -> List.for_all is_packable [l; r]
      | A.Tlambda              _ -> true
      | A.Ttuple               l -> List.for_all is_packable l
      | A.Toption              t -> is_packable t
      | A.Toperation             -> false
      | A.Tcontract            _ -> true
      | A.Tticket              _ -> false
      | A.Ttrace               _ -> false
      | A.Tsapling_state       _ -> false
      | A.Tsapling_transaction _ -> true

    let rec is_big_map_value t =
      match t with
      | A.Tnamed        _        -> false
      | A.Tasset        _        -> false
      | A.Trecord       _        -> true (* TODO: check if all fields are typed for big map value *)
      | A.Tenum         _        -> true
      | A.Tevent        _        -> true (* TODO: check if all fields are typed for big map value *)
      | A.Tbuiltin VTunit        -> true
      | A.Tbuiltin VTbool        -> true
      | A.Tbuiltin VTnat         -> true
      | A.Tbuiltin VTint         -> true
      | A.Tbuiltin VTrational    -> true
      | A.Tbuiltin VTdate        -> true
      | A.Tbuiltin VTduration    -> true
      | A.Tbuiltin VTstring      -> true
      | A.Tbuiltin VTaddress     -> true
      | A.Tbuiltin VTcurrency    -> true
      | A.Tbuiltin VTkey         -> true
      | A.Tbuiltin VTkeyhash     -> true
      | A.Tbuiltin VTsignature   -> true
      | A.Tbuiltin VTbytes       -> true
      | A.Tbuiltin VTchainid     -> true
      | A.Tbuiltin VTbls12_381_fr-> true
      | A.Tbuiltin VTbls12_381_g1-> true
      | A.Tbuiltin VTbls12_381_g2-> true
      | A.Tbuiltin VTnever       -> true
      | A.Tbuiltin VTchest       -> true
      | A.Tbuiltin VTchest_key   -> true
      | A.Tcontainer (_, Collection) -> false
      | A.Tcontainer _           -> true
      | A.Tset                 t -> is_big_map_value t
      | A.Tlist                t -> is_big_map_value t
      | A.Tmap            (k, v) -> List.for_all is_big_map_value [k; v]
      | A.Tbig_map             _ -> false
      | A.Titerable_big_map    _ -> false
      | A.Tor             (l, r) -> List.for_all is_big_map_value [l; r]
      | A.Tlambda              _ -> true
      | A.Ttuple               l -> List.for_all is_big_map_value l
      | A.Toption              t -> is_big_map_value t
      | A.Toperation             -> false
      | A.Tcontract            _ -> true
      | A.Tticket              t -> is_big_map_value t
      | A.Ttrace               _ -> false
      | A.Tsapling_state       _ -> false
      | A.Tsapling_transaction _ -> true

    let is_dupable t =
      match t with
      | A.Tticket _ -> false
      | _           -> true
  end

  let support_eq = Michelson.is_comparable

  let equal = ((=) : A.ptyp -> A.ptyp -> bool)

  let distance ?(autoview = false) ?(for_eq = false) ~(from_ : A.ptyp) ~(to_ : A.ptyp) =
    match from_, to_ with
    | _, _ when equal from_ to_ ->
      Some 0

    | A.Tbuiltin bfrom, A.Tbuiltin bto -> begin
        match bfrom, bto with
        | A.VTnat      , A.VTint        -> Some 1
        | A.VTnat      , A.VTrational   -> Some 2
        | A.VTint      , A.VTrational   -> Some 1
        | A.VTstring   , A.VTkey        -> Some 1
        | A.VTstring   , A.VTkeyhash    -> Some 1
        | A.VTstring   , A.VTsignature  -> Some 1

        | A.VTbls12_381_fr, A.VTnat -> Some 3
        | A.VTbls12_381_fr, A.VTint -> Some 2

        | A.VTint, A.VTbls12_381_fr -> Some 1
        | A.VTnat, A.VTbls12_381_fr -> Some 2

        | A.VTstring, A.VTchainid -> Some 1
        | A.VTbytes, A.VTchainid -> Some 1

        | A.VTbytes, A.VTbls12_381_fr -> Some 1
        | A.VTbytes, A.VTbls12_381_g2 -> Some 1
        | A.VTbytes, A.VTbls12_381_g1 -> Some 1

        | A.VTbytes, A.VTchest -> Some 1
        | A.VTbytes, A.VTchest_key -> Some 1

        (* | A.VTcurrency , A.VTnat when not for_eq -> Some 1 *)
        | A.VTduration , A.VTint when not for_eq -> Some 1

        | _, _ -> None
      end

    | A.Tbuiltin A.VTbytes, A.Tsapling_transaction _ -> Some 1

    | A.Tbuiltin A.VTaddress, A.Tcontract Tbuiltin (VTunit) ->
      Some 1

    | A.Tcontainer (ty1, cf), A.Tcontainer (ty2, ct) ->
      if   equal ty1 ty2 && (cf = ct || (autoview && ct = A.AssetView))
      then Some 0
      else None

    | _, _ ->
      None

  let compatible ?autoview ?for_eq ~(from_ : A.ptyp) ~(to_ : A.ptyp) =
    Option.is_some (distance ?autoview ?for_eq ~from_ ~to_)

  let join ?autoview (tys : A.ptyp list) =
    let module E = struct exception Error end in

    let join2 ty1 ty2 =
      if compatible ?autoview ~for_eq:false ~from_:ty1 ~to_:ty2 then ty2 else
      if compatible ?autoview ~for_eq:false ~from_:ty2 ~to_:ty1 then ty1 else
        raise E.Error in

    try
      match tys with
      | [] -> raise E.Error
      | ty :: tys -> Some (List.fold_left join2 ty tys)

    with E.Error -> None

  let distance ~(from_ : A.ptyp) ~(to_ : A.ptyp) =
    distance ~autoview:false ~for_eq:false ~from_ ~to_

  let sig_compatible ~(from_ : A.ptyp list) ~(to_ : A.ptyp list) =
    List.length from_ = List.length to_
    && List.for_all2
      (fun from_ to_ -> compatible ~autoview:false ~for_eq:false ~from_ ~to_)
      from_ to_

  let sig_distance ~(from_ : A.ptyp list) ~(to_ : A.ptyp list) =
    if List.length from_ <> List.length to_ then None else

      let module E = struct exception Reject end in

      try
        let i =
          List.fold_left2 (fun d from_ to_ ->
              d + Option.get_exn E.Reject (distance ~from_ ~to_)
            ) 0 from_ to_
        in Some i
      with E.Reject -> None

  let sig_equal tys1 tys2 =
    List.length tys1 = List.length tys2
    && List.for_all2 equal tys1 tys2

  type trestr = [`MichelsonPackable]

  exception UnificationFailure

  let unify ?(restr = Mint.empty) ~(ptn : A.ptyp) ~(tg : A.ptyp) (map : A.ptyp Mint.t ref) =
    let module E = struct exception Error end in

    try
      let rec doit (ptn : A.ptyp) (tg : A.ptyp) =
        match ptn, tg with
        | A.Tnamed i, _ -> begin
            begin match Mint.find_opt i restr with
              | Some `MichelsonPackable ->
                if not (Michelson.is_packable tg) then
                  raise E.Error;
              | None -> () end;

            map := !map |> Mint.update i (function
                | None    -> Some tg
                | Some ty ->
                  if   compatible ~autoview:false ~for_eq:false ~to_:ty ~from_:tg
                  then Some ty
                  else raise E.Error)
          end

        | Toperation, Toperation ->
          ()

        | Tasset    x, Tasset    y
        | Tenum     x, Tenum     y ->
          if unloc x <> unloc y then raise E.Error

        | Ttrace x, Ttrace y ->
          if x <> y then raise E.Error

        | Tbuiltin x, Tbuiltin y ->
          if x <> y then raise E.Error

        | Tset      ptn, Tset      tg
        | Tlist     ptn, Tlist     tg
        | Toption   ptn, Toption   tg
        | Tcontract ptn, Tcontract tg
        | Tticket   ptn, Tticket   tg ->
          doit ptn tg

        | Tmap (kptn, vptn), Tmap (ktg, vtg) ->
          List.iter2 doit [kptn; vptn] [ktg; vtg]

        | Tbig_map (kptn, vptn), Tbig_map (ktg, vtg) ->
          List.iter2 doit [kptn; vptn] [ktg; vtg]

        | Titerable_big_map (kptn, vptn), Titerable_big_map (ktg, vtg) ->
          List.iter2 doit [kptn; vptn] [ktg; vtg]

        | Tor (lptn, rptn), Tor (ltg, rtg) ->
          List.iter2 doit [lptn; rptn] [ltg; rtg]

        | Tlambda (lptn, rptn), Tlambda (ltg, rtg) ->
          List.iter2 doit [lptn; rptn] [ltg; rtg]

        | Tcontainer (ptn, x), Tcontainer (tg, y) when x = y ->
          doit ptn tg

        | Ttuple ptn, Ttuple tg when List.length ptn = List.length tg ->
          List.iter2 doit ptn tg

        | _, _ ->
          raise E.Error

      in

      if not (compatible ~autoview:false ~for_eq:false ~to_:ptn ~from_:tg) then
        doit ptn tg

    with E.Error -> raise UnificationFailure

  let subst (subst : A.ptyp Mint.t) (ty : A.ptyp) : A.ptyp =
    let rec doit (ty : A.ptyp) =
      match ty with
      | Tnamed i -> Option.get (Mint.find_opt i subst)
      | Tasset    _
      | Trecord   _
      | Tenum     _
      | Tevent    _
      | Toperation
      | Ttrace    _
      | Tsapling_state _
      | Tsapling_transaction _
      | Tbuiltin  _ -> ty
      | Tcontainer (ty, c) -> Tcontainer (doit ty, c)
      | Tset        ty     -> Tset       (doit ty)
      | Tlist       ty     -> Tlist      (doit ty)
      | Tmap       (k, v)  -> Tmap       (doit k, doit v)
      | Tbig_map   (k, v)  -> Tbig_map   (doit k, doit v)
      | Titerable_big_map   (k, v)  -> Titerable_big_map   (doit k, doit v)
      | Tor        (l, r)  -> Tor        (doit l, doit r)
      | Tlambda    (a, r)  -> Tlambda    (doit a, doit r)
      | Ttuple      ty     -> Ttuple     (List.map doit ty)
      | Toption     ty     -> Toption    (doit ty)
      | Tcontract   ty     -> Tcontract  (doit ty)
      | Tticket     ty     -> Tticket    (doit ty)

    in doit ty

  let pktype = Michelson.is_comparable

  let create_tuple (tys : A.ptyp list) =
    match tys with
    | []   -> A.vtunit
    | [ty] -> ty
    | tys  -> A.Ttuple tys
end

(* -------------------------------------------------------------------- *)
type opsig = {
  osl_sig : A.ptyp list;
  osl_ret : A.ptyp;
} [@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type error_desc =
  | TODO
  | AEntryExpected                     of A.ptyp
  | AlienPattern
  | AnonymousFieldInEffect
  | AssertInGlobalSpec
  | AssetExpected                      of A.ptyp
  | AssetOrRecordExpected              of A.ptyp
  | AssetUpdateInNonFormula
  | AssetWithoutFields
  | AssetWithoutPKey
  | BeforeIrrelevant                   of [`Local | `State]
  | BeforeOrLabelInExpr
  | BindingInExpr
  | CannotAssignArgument               of ident
  | CannotAssignConstVar               of ident
  | CannotAssignLoopIndex              of ident
  | CannotAssignPatternVariable        of ident
  | CannotCaptureVariables
  | CannotEffectConstVar
  | CannotInfer
  | CannotInferAnonAssetOrRecord
  | CannotInferCollectionType
  | CannotInitShadowField
  | CannotUpdatePKey
  | CollectionExpected
  | ContainerOfNonAsset
  | ContractInvariantInLocalSpec
  | DifferentMemoSizeForSaplingVerifyUpdate of int * int
  | DivergentExpr
  | DoesNotSupportMethodCall
  | DuplicatedArgName                  of ident
  | DuplicatedCtorName                 of ident
  | DuplicatedFieldInAssetDecl         of ident
  | DuplicatedFieldInAssetOrRecordLiteral of ident
  | DuplicatedFieldInRecordDecl        of ident
  | DuplicatedInitMarkForCtor
  | DuplicatedPackingVar               of ident
  | DuplicatedPkeyField                of ident
  | DuplicatedVarDecl                  of ident
  | EffectInGlobalSpec
  | EmptyEnumDecl
  | ExpressionExpected
  | FileNotFound                       of ident
  | ForeignState                       of ident option * ident option
  | FormulaExpected
  | IncompatibleSpecSig
  | IncompatibleTypes                  of A.ptyp * A.ptyp
  | IndexOutOfBoundForTuple
  | InvalidArcheTypeDecl
  | InvalidAssetCollectionExpr         of A.ptyp
  | InvalidAssetExpression
  | InvalidAssetGetContainer           of A.container
  | InvalidCallByAsset
  | InvalidCallByExpression
  | InvalidEffectForCtn                of A.container * A.container list
  | InvalidEntryDescription
  | InvalidEntryExpression
  | InvalidEventType
  | InvalidExpression
  | InvalidExpressionForEffect
  | InvalidExprressionForTupleAccess
  | InvalidFailIdType                  of ident * A.ptyp * A.ptyp
  | InvalidFieldsCountInAssetOrRecordLiteral
  | InvalidFoldInit                    of A.ptyp
  | InvalidForIdentMap
  | InvalidForIdentSimple
  | InvalidFormula
  | InvalidInstruction
  | InvalidLValue
  | InvalidMapType
  | InvalidMethodInExec
  | InvalidMethodInFormula
  | InvalidMethodWithBigMap            of ident
  | InvalidMethodWithIterableBigMap    of ident
  | InvalidNumberOfArguments           of int * int
  | InvalidNumberOfParameters          of int * int
  | InvalidPackingExpr
  | InvalidPackingFormat
  | InvalidRecordFieldType
  | InvalidRoleExpression
  | InvalidSaplingEmptyStateArg
  | InvalidSecurityEntry
  | InvalidSecurityRole
  | InvalidShadowFieldAccess
  | InvalidShadowVariableAccess
  | InvalidSortingExpression
  | InvalidSourcedByAsset
  | InvalidSourcedByExpression
  | InvalidStateExpression
  | InvalidStringValue
  | InvalidTezValueOverflow
  | InvalidTypeDeclOpt
  | InvalidTypeForAddressToContract
  | InvalidTypeForBigMapKey
  | InvalidTypeForBigMapValue
  | InvalidTypeForCallview
  | InvalidTypeForContract
  | InvalidTypeForDoFailIf
  | InvalidTypeForDoRequire
  | InvalidTypeForEntrypoint
  | InvalidTypeForFail
  | InvalidTypeForFailSome
  | InvalidTypeForLambdaArgument
  | InvalidTypeForLambdaReturn
  | InvalidTypeForMake
  | InvalidTypeForMapKey
  | InvalidTypeForMapOperator          of A.ptyp
  | InvalidTypeForMapValue
  | InvalidTypeForOptionalAssign
  | InvalidTypeForOrLeft
  | InvalidTypeForOrRight
  | InvalidTypeForParameter
  | InvalidTypeForPk
  | InvalidTypeForSet
  | InvalidTypeForTuple
  | InvalidTzFile
  | InvalidValueForCurrency
  | InvalidValueForMemoSize
  | InvalidVariableForMethod
  | InvalidVarOrArgType
  | LabelInNonInvariant
  | LetInElseInInstruction
  | LetInElseOnNonOption
  | MethodCallInPredicate
  | MisorderedPkeyFields
  | MissingFieldInAssetOrRecordLiteral of ident
  | MissingInitValueForShadowField
  | MixedAnonInAssetOrRecordLiteral
  | MixedFieldNamesInAssetOrRecordLiteral of ident list
  | MoreThanOneInitState               of ident list
  | MultipleAssetStateDeclaration
  | MultipleInitialMarker
  | MultipleMatchingFunction           of ident * A.ptyp list * (A.ptyp list * A.ptyp) list
  | MultipleMatchingOperator           of PT.operator * A.ptyp list * opsig list
  | MultipleStateDeclaration
  | NameIsAlreadyBound                 of ident * Location.t option
  | NoLetInInstruction
  | NoMatchingFunction                 of ident * A.ptyp list
  | NoMatchingOperator                 of PT.operator * A.ptyp list
  | NonCodeLabel                       of ident
  | NonHomogeneousPattern              of ident
  | NonIterable
  | NonIterableBigMapAsset             of ident
  | NonLoopLabel                       of ident
  | NoSuchMethod                       of ident
  | NoSuchSecurityPredicate            of ident
  | NotAKeyOfType
  | NotAnAssetType
  | NotAnEnumType
  | NotAPrimitiveType
  | NotARole                           of ident
  | NumericExpressionExpected
  | NumericOrCurrencyExpressionExpected
  | OpInRecordLiteral
  | OrphanedLabel                      of ident
  | PackUnpackOnNonPrimitive
  | PartialMatch                       of ident list
  | PostConditionInGlobalSpec
  | PredicateCallInExpr
  | ReadOnlyGlobal                     of ident
  | RecordExpected
  | ReturnInVoidContext
  | RecordUpdateDuplicatedFieldName    of ident
  | RecordUpdateOnNonRecordOrAsset
  | RecordUpdateOnPKey
  | RecordUpdateWithInvalidFieldName
  | SecurityInExpr
  | ShadowPKey
  | ShadowSKey
  | SpecOperatorInExpr
  | StringLiteralExpected
  | TransferWithoutDest
  | UninitializedVar
  | UnknownAsset                       of ident
  | UnknownAssetToProperty             of ident
  | UnknownEntry                       of ident
  | UnknownEnum                        of ident
  | UnknownFailId                      of ident
  | UnknownField                       of ident * ident
  | UnknownFieldName                   of ident
  | UnknownFunction                    of ident
  | UnknownGetter                      of ident
  | UnknownImport                      of ident
  | UnknownView                        of ident
  | UnknownLabel                       of ident
  | UnknownLocalOrVariable             of ident
  | UnknownProcedure                   of ident
  | UnknownState                       of ident
  | UnknownTypeName                    of ident
  | UnknownVariable                    of ident
  | UnpureInFormula
  | UpdateEffectOnPkey
  | UpdateEffectWithoutDefault
  | UselessPattern
  | UsePkeyOfInsteadOfAsset
  | VoidMethodInExpr
  | VSetInExpr
  | VSetOnNonAsset
[@@deriving show {with_path = false}]

type error = L.t * error_desc

(* -------------------------------------------------------------------- *)
let pp_operator fmt (op : PT.operator) : unit =
  let pp = Printer_tools.pp_str fmt in
  match op with
  | Logical And   -> pp "and"
  | Logical Or    -> pp "or"
  | Logical Xor   -> pp "xor"
  | Logical Imply -> pp "->"
  | Logical Equiv -> pp "<->"
  | Cmp Equal     -> pp "="
  | Cmp Nequal    -> pp "<>"
  | Cmp Gt        -> pp ">"
  | Cmp Ge        -> pp ">="
  | Cmp Lt        -> pp "<"
  | Cmp Le        -> pp "<="
  | Arith Plus    -> pp "+"
  | Arith Minus   -> pp "-"
  | Arith Mult    -> pp "*"
  | Arith DivRat  -> pp "/"
  | Arith DivEuc  -> pp "div"
  | Arith Modulo  -> pp "%"
  | Arith DivMod  -> pp "/%"
  | Arith ThreeWayCmp -> pp "<=>"
  | Arith ShiftLeft   -> pp "<<"
  | Arith ShiftRight  -> pp ">>"
  | Unary Uminus  -> pp "unary -"
  | Unary Not     -> pp "not"


(* -------------------------------------------------------------------- *)
let pp_error_desc fmt e =
  let pp s = Format.fprintf fmt s in

  match e with
  | TODO                               -> pp "TODO"
  | AEntryExpected ty                  -> pp "Expecting an entry point, not a `%a'" Printer_ast.pp_ptyp ty
  | AlienPattern                       -> pp "This pattern does not belong to the enumeration"
  | AnonymousFieldInEffect             -> pp "Anonymous field in effect"
  | AssertInGlobalSpec                 -> pp "Assertions specification at global level are forbidden"
  | AssetExpected ty                   -> pp "Asset expected (found a %a)" Printer_ast.pp_ptyp ty
  | AssetOrRecordExpected ty           -> pp "Asset or record expected (found a %a)" Printer_ast.pp_ptyp ty
  | AssetUpdateInNonFormula            -> pp "Asset record updated can only appear in formulas"
  | AssetWithoutFields                 -> pp "Asset declaration without fields"
  | AssetWithoutPKey                   -> pp "Asset declaration without a primary key"
  | BeforeIrrelevant `Local            -> pp "The `before' modifier cannot be used on local variables"
  | BeforeIrrelevant `State            -> pp "The `before' modifier cannot be used on state constructors"
  | BeforeOrLabelInExpr                -> pp "The `before' or label modifiers can only be used in formulas"
  | BindingInExpr                      -> pp "Binding in expression"
  | CannotAssignArgument  x            -> pp "Cannot assign argument `%s'" x
  | CannotAssignConstVar x             -> pp "Cannot assign to `%s' because it is a constant" x
  | CannotAssignLoopIndex x            -> pp "Cannot assign loop index `%s'" x
  | CannotAssignPatternVariable x      -> pp "Cannot assign pattern variable `%s'" x
  | CannotCaptureVariables             -> pp "Cannot capture variables in this context"
  | CannotEffectConstVar               -> pp "Cannot apply an effect on constant variable"
  | CannotInfer                        -> pp "Cannot infer type"
  | CannotInferAnonAssetOrRecord       -> pp "Cannot infer anonymous asset or record"
  | CannotInferCollectionType          -> pp "Cannot infer collection type"
  | CannotInitShadowField              -> pp "Cannot initialize a shadow field"
  | CannotUpdatePKey                   -> pp "Cannot modify the primary key of asset"
  | CollectionExpected                 -> pp "Collection expected"
  | ContainerOfNonAsset                -> pp "The base type of a container must be an asset type"
  | ContractInvariantInLocalSpec       -> pp "Contract invariants at local levl are forbidden"
  | DifferentMemoSizeForSaplingVerifyUpdate (n1, n2) -> pp "Different memo size for sapling_verify_update (%i <> %i)" n1 n2
  | DivergentExpr                      -> pp "Divergent expression"
  | DoesNotSupportMethodCall           -> pp "Cannot use method calls on this kind of objects"
  | DuplicatedArgName x                -> pp "Duplicated argument name: %s" x
  | DuplicatedCtorName i               -> pp "Duplicated constructor name: %a" pp_ident i
  | DuplicatedFieldInAssetDecl i       -> pp "Duplicated field in asset declaration: %a" pp_ident i
  | DuplicatedFieldInAssetOrRecordLiteral i
    -> pp "Duplicated field in asset or record literal: %a" pp_ident i
  | DuplicatedFieldInRecordDecl i      -> pp "Duplicated field in record declaration: %a" pp_ident i
  | DuplicatedInitMarkForCtor          -> pp "Duplicated 'initialized by' section for asset"
  | DuplicatedPackingVar x             -> pp "Duplicated packing variable: %a" pp_ident x
  | DuplicatedPkeyField x              -> pp "Duplicated primary key field: %a" pp_ident x
  | DuplicatedVarDecl i                -> pp "Duplicated variable declaration: %a" pp_ident i
  | EffectInGlobalSpec                 -> pp "(Shadow) effects at global level are forbidden"
  | EmptyEnumDecl                      -> pp "Empty state/enum declaration"
  | ExpressionExpected                 -> pp "Expression expected"
  | FileNotFound i                     -> pp "File not found: %s" i
  | ForeignState (i1, i2)              -> pp "Expecting a state of %a, not %a" pp_ident (Option.get_dfl "<global>" i1) pp_ident (Option.get_dfl "<global>" i2)
  | FormulaExpected                    -> pp "Formula expected"
  | IncompatibleSpecSig                -> pp "Specification's signature does not match the one of the targeted object"
  | IncompatibleTypes (t1, t2)         -> pp "Incompatible types: found '%a' but expected '%a'" Printer_ast.pp_ptyp t1 Printer_ast.pp_ptyp t2
  | IndexOutOfBoundForTuple            -> pp "Index out of bounds for tuple"
  | InvalidArcheTypeDecl               -> pp "Invalid Archetype declaration"
  | InvalidAssetCollectionExpr ty      -> pp "Invalid asset collection expression: %a" A.pp_ptyp ty
  | InvalidAssetExpression             -> pp "Invalid asset expression"
  | InvalidAssetGetContainer c         -> pp "Operator `[]` is not available for %a" Printer_ast.pp_container c
  | InvalidCallByExpression            -> pp "Invalid 'Calledby' expression"
  | InvalidCallByAsset                 -> pp "Invalid 'Calledby' asset, the key must be typed address"
  | InvalidEffectForCtn _              -> pp "Invalid effect for this container kind"
  | InvalidEntryDescription            -> pp "Invalid entry description"
  | InvalidEntryExpression             -> pp "Invalid entry expression"
  | InvalidExpression                  -> pp "Invalid expression"
  | InvalidExpressionForEffect         -> pp "Invalid expression for effect"
  | InvalidExprressionForTupleAccess   -> pp "Invalid expression for tuple access, only int literals are allowed"
  | InvalidEventType                   -> pp "Invalid type, only event type are allowed"
  | InvalidFailIdType (id, e, _)       -> pp "'%s' type, expected '%a'" id Printer_ast.pp_ptyp e
  | InvalidFieldsCountInAssetOrRecordLiteral
    -> pp "Invalid fields count in asset or record literal"
  | InvalidFoldInit ty                 -> pp "Fold operator initializer should have a sum type, not %a" Printer_ast.pp_ptyp ty
  | InvalidForIdentMap                 -> pp "Invalid identifier for map iteration, must specify two identifiers like (x, y) instead of x"
  | InvalidForIdentSimple              -> pp "Invalid identifiers for iteration, excpted only one identifier"
  | InvalidFormula                     -> pp "Invalid formula"
  | InvalidInstruction                 -> pp "Invalid instruction"
  | InvalidLValue                      -> pp "Invalid left-value"
  | InvalidMapType                     -> pp "Invalid map type"
  | InvalidMethodInExec                -> pp "Invalid method in execution"
  | InvalidMethodInFormula             -> pp "Invalid method in formula"
  | InvalidMethodWithIterableBigMap id -> pp "Invalid method with iterable big map asset: %s" id
  | InvalidMethodWithBigMap id         -> pp "Invalid method with big map asset: %s" id
  | InvalidNumberOfArguments (n1, n2)  -> pp "Invalid number of arguments: found '%i', but expected '%i'" n2 n1
  | InvalidNumberOfParameters (n1, n2) -> pp "Invalid number of parameters: found '%i', but expected '%i'" n2 n1
  | InvalidPackingExpr                 -> pp "Invalid packing expression"
  | InvalidPackingFormat               -> pp "Invalid packing format"
  | InvalidRecordFieldType             -> pp "Invalid record field's type"
  | InvalidRoleExpression              -> pp "Invalid role expression"
  | InvalidSaplingEmptyStateArg        -> pp "Invalid sapling empty state argument, it must be a literal"
  | InvalidSecurityEntry               -> pp "Invalid security entry"
  | InvalidSecurityRole                -> pp "Invalid security role"
  | InvalidShadowFieldAccess           -> pp "Shadow field access in non-shadow code"
  | InvalidShadowVariableAccess        -> pp "Shadow variable access in non-shadow code"
  | InvalidSortingExpression           -> pp "Invalid sorting expression"
  | InvalidSourcedByExpression         -> pp "Invalid 'Sourcedby' expression"
  | InvalidSourcedByAsset              -> pp "Invalid 'Sourcedby' asset, the key must be typed address"
  | InvalidStateExpression             -> pp "Invalid state expression"
  | InvalidStringValue                 -> pp "Invalid string value"
  | InvalidTezValueOverflow            -> pp "Overflow tez value"
  | InvalidTypeForAddressToContract    -> pp "Invalid type for address_to_contract"
  | InvalidTypeForBigMapKey            -> pp "Invalid type for big map key"
  | InvalidTypeForBigMapValue          -> pp "Invalid type for big map value"
  | InvalidTypeForCallview             -> pp "Invalid type for call_view"
  | InvalidTypeForContract             -> pp "Invalid type for contract"
  | InvalidTypeDeclOpt                 -> pp "Invalid type optional declaration, must be typed option"
  | InvalidTypeForDoFailIf             -> pp "Invalid type for dofailif"
  | InvalidTypeForDoRequire            -> pp "Invalid type for dorequire"
  | InvalidTypeForEntrypoint           -> pp "Invalid type for entrypoint"
  | InvalidTypeForFail                 -> pp "Invalid type for fail, must be typed packable type"
  | InvalidTypeForFailSome             -> pp "Invalid type for fail_some, must be typed option of packable type"
  | InvalidTypeForLambdaArgument       -> pp "Invalid type for lambda argument"
  | InvalidTypeForLambdaReturn         -> pp "Invalid type for lambda return"
  | InvalidTypeForMake                 -> pp "Invalid type for Make"
  | InvalidTypeForMapOperator ty       -> pp "Map operator should be applied to a list or an option, not %a" Printer_ast.pp_ptyp ty
  | InvalidTypeForMapKey               -> pp "Invalid type for map key"
  | InvalidTypeForMapValue             -> pp "Invalid type for map value"
  | InvalidTypeForParameter            -> pp "Invalid type for parameter"
  | InvalidTypeForOrLeft               -> pp "Invalid type for or left"
  | InvalidTypeForOrRight              -> pp "Invalid type for or right"
  | InvalidTypeForOptionalAssign       -> pp "Invalid type for optional assignment, must be an option type"
  | InvalidTypeForPk                   -> pp "Invalid type for primary key"
  | InvalidTypeForSet                  -> pp "Invalid type for set"
  | InvalidTypeForTuple                -> pp "Invalid type for tuple"
  | InvalidTzFile                      -> pp "Invalid tz file"
  | InvalidValueForCurrency            -> pp "Invalid value for currency"
  | InvalidVariableForMethod           -> pp "Invalid variable for method"
  | InvalidVarOrArgType                -> pp "A variable / argument type cannot be an asset or a collection"
  | InvalidValueForMemoSize            -> pp "Invalid value for memo size (0 <= n <= 65535)"
  | LabelInNonInvariant                -> pp "The label modifier can only be used in invariants"
  | LetInElseInInstruction             -> pp "Let In else in instruction"
  | LetInElseOnNonOption               -> pp "Let in else on non-option type"
  | MethodCallInPredicate              -> pp "Cannot call methods in predicates"
  | MisorderedPkeyFields               -> pp "Primary keys order should follow asset fields order"
  | MissingFieldInAssetOrRecordLiteral i
    -> pp "Missing field in asset or record literal: %a" pp_ident i
  | MissingInitValueForShadowField     -> pp "Shadow fields must have a default value"
  | MixedAnonInAssetOrRecordLiteral    -> pp "Mixed anonymous in asset or record literal"
  | MixedFieldNamesInAssetOrRecordLiteral l
    -> pp "Mixed field names in asset or record literal: %a" (Printer_tools.pp_list "," pp_ident) l
  | MoreThanOneInitState l             -> pp "More than one initial state: %a" (Printer_tools.pp_list ", " pp_ident) l
  | MultipleAssetStateDeclaration      -> pp "Multiple asset states declaration"
  | MultipleInitialMarker              -> pp "Multiple 'initial' marker"
  | MultipleStateDeclaration           -> pp "Multiple state declaration"
  | NameIsAlreadyBound (i, None)       -> pp "Name is already bound: %a" pp_ident i
  | NameIsAlreadyBound (i, Some l)     -> pp "Name is already bound: %a (previous definition: %s)" pp_ident i (Location.tostring l)
  | NoLetInInstruction                 -> pp "No Let In in instruction"
  | NonCodeLabel i                     -> pp "Not a code label: %a" pp_ident i
  | NonHomogeneousPattern x            -> pp "This variable must appear in all sub-patterns: %a" pp_ident x
  | NonIterable                        -> pp "Cannot iterate over"
  | NonIterableBigMapAsset i           -> pp "Asset to big_map is not iterable: %s" i
  | NonLoopLabel i                     -> pp "Not a loop label: %a" pp_ident i
  | NoSuchMethod i                     -> pp "No such method: %a" pp_ident i
  | NoSuchSecurityPredicate i          -> pp "No such security predicate: %a" pp_ident i
  | NotAKeyOfType                      -> pp "pkey-of type expected"
  | NotAnAssetType                     -> pp "Asset type expected"
  | NotAnEnumType                      -> pp "Enumeration type expected"
  | NotAPrimitiveType                  -> pp "Primitive type expected"
  | NotARole i                         -> pp "Not a role: %a" pp_ident i
  | NumericExpressionExpected          -> pp "Expecting numerical expression"
  | NumericOrCurrencyExpressionExpected-> pp "Expecting numerical or currency expression"
  | OpInRecordLiteral                  -> pp "Operation in record literal"
  | OrphanedLabel i                    -> pp "Label not used: %a" pp_ident i
  | PackUnpackOnNonPrimitive           -> pp "Cannot pack / unpack non primitive types"
  | PartialMatch ps                    -> pp "Partial match (%a)" (Printer_tools.pp_list ", " pp_ident) ps
  | PostConditionInGlobalSpec          -> pp "Post-conditions at global level are forbidden"
  | PredicateCallInExpr                -> pp "Cannot access predicates in code"
  | ReadOnlyGlobal i                   -> pp "Global is read only: %a" pp_ident i
  | RecordExpected                     -> pp "Record expected"
  | ReturnInVoidContext                -> pp "Unexpected return in void context"
  | RecordUpdateDuplicatedFieldName x  -> pp "Duplicated field name: %a" pp_ident x
  | RecordUpdateOnNonRecordOrAsset     -> pp "Record update on a non-record/asset expression"
  | RecordUpdateOnPKey                 -> pp "Record updates cannot act on primary keys"
  | RecordUpdateWithInvalidFieldName   -> pp "Unknown or invalid field name"
  | SecurityInExpr                     -> pp "Found securtiy predicate in expression"
  | ShadowPKey                         -> pp "Primary key cannot be a shadow field"
  | ShadowSKey                         -> pp "Sort key cannot be a shadow field"
  | SpecOperatorInExpr                 -> pp "Specification operator in expression"
  | StringLiteralExpected              -> pp "Expecting a string literal"
  | TransferWithoutDest                -> pp "Transfer without destination"
  | UninitializedVar                   -> pp "This variable declaration is missing an initializer"
  | UnknownAsset i                     -> pp "Unknown asset: %a" pp_ident i
  | UnknownAssetToProperty i           -> pp "Unknown asset to property: %a" pp_ident i
  | UnknownEntry i                     -> pp "Unknown entry: %a" pp_ident i
  | UnknownEnum i                      -> pp "Unknown enum: %a" pp_ident i
  | UnknownFailId s                    -> pp "Unknown fail id: %s" s
  | UnknownField (i1, i2)              -> pp "Unknown field: asset %a does not have a field %a" pp_ident i1 pp_ident i2
  | UnknownFieldName i                 -> pp "Unknown field name: %a" pp_ident i
  | UnknownFunction  i                 -> pp "Unknown function name: %a" pp_ident i
  | UnknownGetter  i                   -> pp "Unknown getter name: %a" pp_ident i
  | UnknownImport i                    -> pp "Unknown import identifier name: %a" pp_ident i
  | UnknownView  i                     -> pp "Unknown view name: %a" pp_ident i
  | UnknownLabel i                     -> pp "Unknown label: %a" pp_ident i
  | UnknownLocalOrVariable i           -> pp "Unknown local or variable: %a" pp_ident i
  | UnknownProcedure i                 -> pp "Unknown procedure: %a" pp_ident i
  | UnknownState i                     -> pp "Unknown state: %a" pp_ident i
  | UnknownTypeName i                  -> pp "Unknown type: %a" pp_ident i
  | UnknownVariable i                  -> pp "Unknown variable: %a" pp_ident i
  | UnpureInFormula                    -> pp "Cannot use expression with side effect"
  | UpdateEffectOnPkey                 -> pp "Cannot set/update the primary key in an effect"
  | UpdateEffectWithoutDefault         -> pp "Update effect without default value for field"
  | UselessPattern                     -> pp "Useless match branch"
  | UsePkeyOfInsteadOfAsset            -> pp "Cannot reference assets directly, use `pkey of` instead"
  | VoidMethodInExpr                   -> pp "Void method in non-void context"
  | VSetInExpr                         -> pp "Virtual set in expression"
  | VSetOnNonAsset                     -> pp "Virtual set modifier on non-asset"

  | NoMatchingOperator (op, sig_) ->
    pp "No matches for operator %a(%a)"
      pp_operator op
      (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) sig_

  | MultipleMatchingOperator (op, sig_, sigs) ->
    pp "Multiple matches for operator %a(%a): %a"
      pp_operator op
      (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) sig_
      (Printer_tools.pp_list ", " (fun fmt sig_ ->
           Format.fprintf fmt "(%a) -> %a"
             (Printer_tools.pp_list " * " Printer_ast.pp_ptyp) sig_.osl_sig
             Printer_ast.pp_ptyp sig_.osl_ret)) sigs

  | NoMatchingFunction (f, sig_) ->
    pp "No matches for function %s(%a)" f
      (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) sig_

  | MultipleMatchingFunction (f, sig_, sigs) ->
    pp "Multiple matches for operator %s(%a): %a" f
      (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) sig_
      (Printer_tools.pp_list ", " (fun fmt sig_ ->
           Format.fprintf fmt "(%a) -> %a"
             (Printer_tools.pp_list " * " Printer_ast.pp_ptyp) (fst sig_)
             Printer_ast.pp_ptyp (snd sig_))) sigs

(* -------------------------------------------------------------------- *)
type argtype = [`Type of A.type_ | `Effect of ident]

(* -------------------------------------------------------------------- *)
let cmptypes =
  [ A.VTnat     ;
    A.VTint     ;
    A.VTrational;
    A.VTdate    ;
    A.VTduration;
    A.VTstring  ;
    A.VTaddress ;
    A.VTcurrency;
    A.VTbytes   ]

let grptypes =
  [ A.VTduration       ;
    A.VTcurrency       ]

let rgtypes =
  [ A.VTint      ;
    A.VTrational ]

let vt_comparable = List.filter Type.Michelson.is_comparable A.vts

(* -------------------------------------------------------------------- *)
let cmpsigs : (PT.operator * (A.vtyp list * A.vtyp)) list =
  let ops  = [PT.Gt; PT.Ge; PT.Lt; PT.Le] in
  let sigs = List.map (fun ty -> ([ty; ty], A.VTbool)) cmptypes in
  List.mappdt (fun op sig_ -> (PT.Cmp op, sig_)) ops sigs

let tsigs : (PT.operator * (A.vtyp list * A.vtyp)) list =
  let ops  = [PT.ThreeWayCmp] in
  let sigs = List.map (fun ty -> ([ty; ty], A.VTint)) cmptypes in
  List.mappdt (fun op sig_ -> (PT.Arith op, sig_)) ops sigs

let opsigs =
  let grptypes : (PT.operator * (A.vtyp list * A.vtyp)) list =
    let bops = List.map (fun x -> PT.Arith x) [PT.Plus ; PT.Minus] in
    let uops = List.map (fun x -> PT.Unary x) [PT.Uminus] in
    let bsig = List.map (fun ty -> ([ty; ty], ty)) grptypes in
    let usig = List.map (fun ty -> ([ty], ty)) grptypes in
    (List.mappdt (fun op sig_ -> (op, sig_)) bops bsig)
    @ (List.mappdt (fun op sig_ -> (op, sig_)) uops usig) in

  let rgtypes : (PT.operator * (A.vtyp list * A.vtyp)) list =
    let bops = (List.map (fun x -> PT.Arith x) [PT.Plus; PT.Minus; PT.Mult]) in
    let uops = (List.map (fun x -> PT.Unary x) [PT.Uminus]) in
    let bsig = List.map (fun ty -> ([ty; ty], ty)) rgtypes in
    let usig = List.map (fun ty -> ([ty], ty)) rgtypes in
    (List.mappdt (fun op sig_ -> (op, sig_)) bops bsig)
    @ (List.mappdt (fun op sig_ -> (op, sig_)) uops usig) in

  let ariths : (PT.operator * (A.vtyp list * A.vtyp)) list =
    [ PT.Arith PT.Modulo, ([A.VTint; A.VTint],           A.VTnat);
      PT.Arith PT.DivRat, ([A.VTrational; A.VTrational], A.VTrational);
      PT.Arith PT.DivEuc, ([A.VTint; A.VTint],           A.VTint) ] in

  let nat : (PT.operator * (A.vtyp list * A.vtyp)) list =
    [ PT.Arith PT.Plus       , ([A.VTnat; A.VTnat], A.VTnat) ;
      PT.Arith PT.Mult       , ([A.VTnat; A.VTnat], A.VTnat) ;
      PT.Arith PT.DivEuc     , ([A.VTnat; A.VTnat], A.VTnat) ;
      PT.Arith PT.ShiftLeft  , ([A.VTnat; A.VTnat], A.VTnat) ;
      PT.Arith PT.ShiftRight , ([A.VTnat; A.VTnat], A.VTnat) ] in

  let bools : (PT.operator * (A.vtyp list * A.vtyp)) list =
    let unas = List.map (fun x -> PT.Unary   x) [PT.Not] in
    let bins = List.map (fun x -> PT.Logical x) [PT.And; PT.Or; PT.Imply; PT.Equiv] in

    List.map (fun op -> (op, ([A.VTbool], A.VTbool))) unas
    @ List.map (fun op -> (op, ([A.VTbool; A.VTbool], A.VTbool))) bins in

  let others : (PT.operator * (A.vtyp list * A.vtyp)) list =
    [ PT.Arith PT.Plus   , ([A.VTdate    ; A.VTduration      ], A.VTdate    )  ;
      PT.Arith PT.Plus   , ([A.VTduration; A.VTdate          ], A.VTdate    )  ;
      PT.Arith PT.Plus   , ([A.VTint     ; A.VTduration      ], A.VTduration)  ;
      PT.Arith PT.Plus   , ([A.VTduration; A.VTint           ], A.VTduration)  ;
      PT.Arith PT.Minus  , ([A.VTint     ; A.VTduration      ], A.VTduration)  ;
      PT.Arith PT.Minus  , ([A.VTduration; A.VTint           ], A.VTduration)  ;
      PT.Arith PT.Minus  , ([A.VTdate    ; A.VTduration      ], A.VTdate    )  ;
      PT.Arith PT.Minus  , ([A.VTdate    ; A.VTdate          ], A.VTduration)  ;
      PT.Arith PT.Mult   , ([A.VTnat     ; A.VTcurrency      ], A.VTcurrency)  ;
      PT.Arith PT.Mult   , ([A.VTcurrency; A.VTnat           ], A.VTcurrency)  ;
      PT.Arith PT.Mult   , ([A.VTrational; A.VTcurrency      ], A.VTcurrency)  ;
      PT.Arith PT.Mult   , ([A.VTint     ; A.VTduration      ], A.VTduration)  ;
      PT.Arith PT.Mult   , ([A.VTrational; A.VTduration      ], A.VTduration)  ;
      PT.Arith PT.Mult   , ([A.VTduration; A.VTrational      ], A.VTduration)  ;
      PT.Arith PT.DivRat , ([A.VTduration; A.VTduration      ], A.VTrational)  ;
      PT.Arith PT.DivEuc , ([A.VTcurrency; A.VTcurrency      ], A.VTnat     )  ;
      PT.Arith PT.DivEuc , ([A.VTduration; A.VTduration      ], A.VTint     )  ;
      PT.Arith PT.DivEuc , ([A.VTcurrency; A.VTnat           ], A.VTcurrency)  ;
      PT.Arith PT.DivEuc , ([A.VTduration; A.VTint           ], A.VTduration)  ;
      PT.Arith PT.DivRat , ([A.VTcurrency; A.VTcurrency      ], A.VTrational)  ;
      PT.Arith PT.Modulo , ([A.VTcurrency; A.VTcurrency      ], A.VTcurrency)  ;
      PT.Arith PT.Plus   , ([A.VTstring  ; A.VTstring        ], A.VTstring  )  ;
      PT.Logical PT.Xor  , ([A.VTbool    ; A.VTbool          ], A.VTbool    )  ;
      PT.Logical PT.Xor  , ([A.VTnat     ; A.VTnat           ], A.VTnat     )  ;
      PT.Unary PT.Not    , ([A.VTnat                         ], A.VTint     )  ;
      PT.Unary PT.Not    , ([A.VTint                         ], A.VTint     )  ;
    ] in

  let bls_curves : (PT.operator * (A.vtyp list * A.vtyp)) list =
    (
      List.map (fun x -> [ PT.Arith PT.Plus,   ([x; x], x);
                           PT.Arith PT.Mult,   ([x; A.VTbls12_381_fr], x);
                           PT.Unary PT.Uminus, ([x], x)])
        [A.VTbls12_381_fr; A.VTbls12_381_g1; A.VTbls12_381_g2]
      |> List.flatten)
  in

  cmpsigs @ tsigs @ grptypes @ rgtypes @ ariths @ nat @ bools @ others @ bls_curves

let opsigs2 =
  let divmod : (PT.operator * (A.vtyp list * A.ptyp)) list =
    [ PT.Arith PT.DivMod  , ([A.VTnat; A.VTnat],           A.Toption (A.Ttuple [A.Tbuiltin A.VTnat; A.Tbuiltin A.VTnat])) ;
      PT.Arith PT.DivMod  , ([A.VTnat; A.VTint],           A.Toption (A.Ttuple [A.Tbuiltin A.VTint; A.Tbuiltin A.VTnat])) ;
      PT.Arith PT.DivMod  , ([A.VTint; A.VTnat],           A.Toption (A.Ttuple [A.Tbuiltin A.VTint; A.Tbuiltin A.VTnat])) ;
      PT.Arith PT.DivMod  , ([A.VTint; A.VTint],           A.Toption (A.Ttuple [A.Tbuiltin A.VTint; A.Tbuiltin A.VTnat])) ;
      PT.Arith PT.DivMod  , ([A.VTcurrency; A.VTnat],      A.Toption (A.Ttuple [A.Tbuiltin A.VTcurrency; A.Tbuiltin A.VTcurrency])) ;
      PT.Arith PT.DivMod  , ([A.VTcurrency; A.VTcurrency], A.Toption (A.Ttuple [A.Tbuiltin A.VTnat; A.Tbuiltin A.VTcurrency])) ] in
  divmod

let opsigs =
  let doit f (args, ret) =
    { osl_sig = List.map (fun x -> A.Tbuiltin x) args;
      osl_ret = f ret; } in
  List.map (snd_map (doit (fun x -> A.Tbuiltin x))) opsigs @
  List.map (snd_map (doit id)) opsigs2


(* -------------------------------------------------------------------- *)
type acttx = [
  | `Entry      of PT.entry_decl
  | `Transition of PT.transition_decl
]

type groups = {
  gr_archetypes  : (PT.lident * PT.exts)        loced list;
  gr_imports     : (PT.lident * PT.lident)      loced list;
  gr_states      : PT.enum_decl                 loced list;
  gr_enums       : (PT.lident * PT.enum_decl)   loced list;
  gr_assets      : PT.asset_decl                loced list;
  gr_records     : PT.record_decl               loced list;
  gr_events      : PT.record_decl               loced list;
  gr_vars        : PT.variable_decl             loced list;
  gr_funs        : PT.s_function                loced list;
  gr_acttxs      : acttx                        loced list;
  gr_specs       : PT.specification             loced list;
  gr_specfuns    : PT.specfun                   loced list;
  gr_specvars    : (PT.lident * PT.label_exprs) loced list;
  gr_specassets  : (PT.lident * PT.label_exprs) loced list;
  gr_secs        : PT.security                  loced list;
}

(* -------------------------------------------------------------------- *)
let globals = [
  ("balance"       , A.Cbalance     , A.vtcurrency);
  ("caller"        , A.Ccaller      , A.vtaddress);
  ("now"           , A.Cnow         , A.vtdate);
  ("source"        , A.Csource      , A.vtaddress);
  ("self_address"  , A.Cselfaddress , A.vtaddress);
  ("transferred"   , A.Ctransferred , A.vtcurrency);
  ("self_chain_id" , A.Cselfchainid , A.vtchainid);
  ("operations"    , A.Coperations  , A.Tlist (A.Toperation));
  ("metadata"      , A.Cmetadata    , A.Tbig_map (A.vtstring, A.vtbytes));
  ("level"         , A.Clevel       , A.vtnat);
  ("total_voting_power", A.Ctotalvotingpower, A.vtnat);
]

let statename = "state"


type ('args, 'rty) gmethod_ = {
  mth_name     : A.const;
  mth_place    : [`Both | `OnlyFormula | `OnlyExec ];
  mth_purity   : [`Pure | `Effect of A.container list];
  mth_totality : [`Total | `Partial];
  mth_map_type : [`Both | `Standard ] ;
  mth_sig      : 'args * 'rty option;
}

type mthstyp = [
  | `T of A.ptyp
]

type mthtyp = [
  | mthstyp
  | `The
  | `Pk
  | `OPk
  | `ThePkForAggregate
  | `Asset
  | `Coll
  | `SubColl
  | `Container
  | `PkOrAsset
  | `OptVal
  | `Cmp
  | `Pred  of bool
  | `RExpr of bool
  | `Ef    of bool
  | `Ref   of int
]

and mthatyp = [ `Fixed of mthtyp list | `Multi of mthtyp ]

type smethod_ = (mthstyp list, mthstyp) gmethod_
type method_  = (mthatyp     , mthtyp ) gmethod_

let methods : (string * method_) list =
  let cap  = A.[Collection; Aggregate; Partition]  in
  let capv = A.[Collection; Aggregate; Partition; AssetView]  in
  let    v = A.[AssetView] in
  let c    = A.[Collection] in
  let c_p  = A.[Collection; Partition] in

  let mk mth_name mth_place mth_purity mth_totality mth_map_type mth_sig =
    { mth_name; mth_place; mth_purity; mth_totality; mth_map_type; mth_sig; }
  in [
    ("empty"       , mk A.Cempty        `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [                   ], Some (`Coll)));
    ("singleton"   , mk A.Csingleton    `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [`The               ], Some (`Coll)));
    ("isempty"     , mk A.Cisempty      `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [                   ], Some (`T A.vtbool)));
    ("subsetof"    , mk A.Csubsetof     `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [`SubColl           ], Some (`T A.vtbool)));
    ("union"       , mk A.Cunion        `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [`Coll              ], Some (`Coll)));
    ("inter"       , mk A.Cinter        `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [`Coll              ], Some (`Coll)));
    ("diff"        , mk A.Cdiff         `OnlyFormula (`Pure       ) `Total   `Standard (`Fixed [`Coll              ], Some (`Coll)));
    ("add"         , mk A.Cadd          `Both        (`Effect cap ) `Total   `Both     (`Fixed [`ThePkForAggregate ], None));
    ("put"         , mk A.Cput          `Both        (`Effect c   ) `Total   `Both     (`Fixed [`ThePkForAggregate ], None));
    ("remove"      , mk A.Cremove       `Both        (`Effect cap ) `Total   `Both     (`Fixed [`Pk                ], None));
    ("clear"       , mk A.Cclear        `Both        (`Effect    v) `Total   `Standard (`Fixed [                   ], None));
    ("remove_if"   , mk A.Cremoveif     `Both        (`Effect cap ) `Total   `Standard (`Fixed [`Pred true         ], None));
    ("remove_all"  , mk A.Cremoveall    `Both        (`Effect cap ) `Total   `Standard (`Fixed [                   ], None));
    ("update"      , mk A.Cupdate       `Both        (`Effect c   ) `Total   `Both     (`Fixed [`Pk; `Ef true      ], None));
    ("update_all"  , mk A.Cupdateall    `Both        (`Effect capv) `Total   `Both     (`Fixed [`Ef true           ], None));
    ("add_update"  , mk A.Caddupdate    `Both        (`Effect c_p ) `Total   `Both     (`Fixed [`Pk; `Ef false     ], None));
    ("put_remove"  , mk A.CputRemove    `Both        (`Effect c   ) `Total   `Both     (`Fixed [`Pk; `OptVal       ], None));
    ("contains"    , mk A.Ccontains     `Both        (`Pure       ) `Total   `Both     (`Fixed [`Pk                ], Some (`T A.vtbool)));
    ("nth"         , mk A.Cnth          `Both        (`Pure       ) `Total   `Standard (`Fixed [`T A.vtnat         ], Some (`OPk)));
    ("select"      , mk A.Cselect       `Both        (`Pure       ) `Total   `Standard (`Fixed [`Pred true         ], Some (`SubColl)));
    ("sort"        , mk A.Csort         `OnlyExec    (`Pure       ) `Total   `Standard (`Multi (`Cmp               ), Some (`SubColl)));
    ("count"       , mk A.Ccount        `Both        (`Pure       ) `Total   `Standard (`Fixed [                   ], Some (`T A.vtnat)));
    ("sum"         , mk A.Csum          `Both        (`Pure       ) `Total   `Standard (`Fixed [`RExpr false       ], Some (`Ref 0)));
    ("head"        , mk A.Chead         `Both        (`Pure       ) `Total   `Standard (`Fixed [`T A.vtnat         ], Some (`SubColl)));
    ("tail"        , mk A.Ctail         `Both        (`Pure       ) `Total   `Standard (`Fixed [`T A.vtnat         ], Some (`SubColl)));
    ("to_container", mk A.CtoContainer  `Both        (`Pure       ) `Total   `Both     (`Fixed [                   ], Some (`Container)));
  ]

let methods = Mid.of_list methods

(* -------------------------------------------------------------------- *)
type opinfo = {
  op_name    : string;
  op_const   : A.const;
  op_partial : [`Partial | `Total];
  op_thety   : A.type_ option;
  op_sig     : A.type_ list;
  op_resty   : [`Self | `Ty of A.type_];
  op_restr   : Type.trestr Mint.t;
  op_filter  : (A.type_ list -> A.pterm list -> bool) option
}

let op ?op_filter op_name op_const op_partial op_thety op_sig op_resty op_restr =
  { op_name; op_const; op_partial; op_thety; op_sig; op_resty; op_restr; op_filter }

(* -------------------------------------------------------------------- *)
let coreops : opinfo list =
  (List.map
     (fun (x, y) -> op "abs" A.Cabs `Total None [x] y Mint.empty)
     [(A.vtint, `Ty A.vtnat); (A.vtrational, `Ty A.vtrational)])
  @ (List.map
       (fun (x, y) -> op x y `Total None [A.vtrational] (`Ty A.vtint) Mint.empty)
       ["floor", A.Cfloor ; "ceil", A.Cceil])
  @ (List.flatten (List.map (fun (name, cname) -> (
        List.map
          (fun x -> op name cname `Total None [x; x] (`Ty x) Mint.empty)
          vt_comparable))
      [("min", A.Cmin); ("max", A.Cmax)]))
  @ (List.map
       (fun x -> op "concat" A.Cconcat `Total None [x; x] (`Ty x) Mint.empty)
       [A.vtbytes; A.vtstring])
  @ (List.map
       (fun x -> op "concat" A.Cconcat `Total None [A.Tlist x] (`Ty x) Mint.empty)
       [A.vtbytes; A.vtstring])
  @ (List.map
       (fun x -> op "slice" A.Cslice `Total None [x; A.vtnat; A.vtnat] (`Ty (A.Toption x)) Mint.empty)
       [A.vtbytes; A.vtstring])
  @ (List.map
       (fun x -> op "length" A.Clength `Total None [x] (`Ty A.vtnat) Mint.empty)
       [A.vtstring; A.vtbytes])
  @ (List.map
       (fun x -> op "nat_to_string" A.Cnattostring `Total None [x] (`Ty A.vtstring) Mint.empty)
       [A.vtnat])
  @ [
    op "int_to_nat"  A.Cinttonat `Total None [A.vtint] (`Ty (A.Toption A.vtnat)) Mint.empty;
    op "int_to_date" A.Cinttodate `Total None [A.vtint] (`Ty A.vtdate) Mint.empty;
    op "mutez_to_nat" A.CmutezToNat `Total None [A.vtcurrency] (`Ty A.vtnat) Mint.empty
  ]


(* -------------------------------------------------------------------- *)
let optionops : opinfo list =
  let ty = A.Tnamed 0 in
  let top = A.Toption ty in
  [
    op "is_none"      A.Cisnone      `Total   (Some top) [] (`Ty A.vtbool) Mint.empty;
    op "is_some"      A.Cissome      `Total   (Some top) [] (`Ty A.vtbool) Mint.empty
  ]

(* -------------------------------------------------------------------- *)
let setops : opinfo list =
  let elemt = A.Tnamed 0 in
  let set   = A.Tset elemt in [
    op "add"      A.Csadd      `Total (Some set) [ elemt           ] `Self          Mint.empty;
    op "remove"   A.Csremove   `Total (Some set) [ elemt           ] `Self          Mint.empty;
    op "update"   A.Csupdate   `Total (Some set) [ A.vtbool; elemt ] `Self          Mint.empty;
    op "contains" A.Cscontains `Total (Some set) [ elemt           ] (`Ty A.vtbool) Mint.empty;
    op "length"   A.Cslength   `Total (Some set) [                 ] (`Ty A.vtnat ) Mint.empty;
  ]

(* -------------------------------------------------------------------- *)
let listops : opinfo list =
  let elemt = A.Tnamed 0 in
  let lst   = A.Tlist elemt in [
    op "prepend"  A.Cprepend  `Total   (Some lst) [elemt  ] `Self          Mint.empty;
    op "length"   A.Clength   `Total   (Some lst) [       ] (`Ty A.vtnat ) Mint.empty;
    op "contains" A.Ccontains `Total   (Some lst) [elemt  ] (`Ty A.vtbool) Mint.empty;
    op "nth"      A.Cnth      `Total   (Some lst) [A.vtnat] (`Ty (A.Toption elemt)) Mint.empty;
    op "reverse"  A.Creverse  `Total   (Some lst) [       ] `Self          Mint.empty;
    op "concat"   A.Cconcat   `Total   (Some lst) [lst    ] `Self          Mint.empty;
  ]

(* -------------------------------------------------------------------- *)
let mapops : opinfo list =
  let tkey = A.Tnamed 0 in
  let tval = A.Tnamed 1 in
  let map  = A.Tmap (tkey, tval) in [
    op "put"      A.Cmput      `Total   (Some map) [ tkey; tval ]           `Self                  Mint.empty;
    op "remove"   A.Cmremove   `Total   (Some map) [ tkey       ]           `Self                  Mint.empty;
    op "update"   A.Cmupdate   `Total   (Some map) [ tkey; A.Toption tval ] `Self                  Mint.empty;
    op "contains" A.Cmcontains `Total   (Some map) [ tkey       ]           (`Ty A.vtbool        ) Mint.empty;
    op "length"   A.Cmlength   `Total   (Some map) [            ]           (`Ty A.vtnat         ) Mint.empty;
  ]

(* -------------------------------------------------------------------- *)
let bigmapops : opinfo list =
  let tkey = A.Tnamed 0 in
  let tval = A.Tnamed 1 in
  let big_map  = A.Tbig_map (tkey, tval) in [
    op "put"      A.Cmput      `Total   (Some big_map) [ tkey; tval ]           `Self                  Mint.empty;
    op "remove"   A.Cmremove   `Total   (Some big_map) [ tkey       ]           `Self                  Mint.empty;
    op "update"   A.Cmupdate   `Total   (Some big_map) [ tkey; A.Toption tval ] `Self                  Mint.empty;
    op "contains" A.Cmcontains `Total   (Some big_map) [ tkey       ]           (`Ty A.vtbool        ) Mint.empty;
  ]

(* -------------------------------------------------------------------- *)
let iterablebigmapops : opinfo list =
  let tkey = A.Tnamed 0 in
  let tval = A.Tnamed 1 in
  let iterablebigmap  = A.Titerable_big_map (tkey, tval) in [
    op "put"      A.Cmput      `Total   (Some iterablebigmap) [ tkey; tval ]    `Self                  Mint.empty;
    op "remove"   A.Cmremove   `Total   (Some iterablebigmap) [ tkey       ]    `Self                  Mint.empty;
    op "contains" A.Cmcontains `Total   (Some iterablebigmap) [ tkey       ]    (`Ty A.vtbool        ) Mint.empty;
    op "length"   A.Cmlength   `Total   (Some iterablebigmap) [            ]    (`Ty A.vtnat         ) Mint.empty;
  ]

(* -------------------------------------------------------------------- *)
let cryptoops : opinfo list =
  List.map (fun (x, y) -> op x y `Total None [A.vtbytes] (`Ty A.vtbytes) Mint.empty)
    [("blake2b", A.Cblake2b);
     ("sha256" , A.Csha256 );
     ("sha512" , A.Csha512 );
     ("sha3"   , A.Csha3   );
     ("keccak" , A.Ckeccak )]

  @ [op "key_to_key_hash"      A.Ckeytokeyhash      `Total None [A.vtkey]                                                 (`Ty A.vtkeyhash           ) Mint.empty;
     op "check_signature"      A.Cchecksignature    `Total None [A.vtkey; A.vtsignature; A.vtbytes]                       (`Ty A.vtbool              ) Mint.empty;
     op "set_delegate"         A.Csetdelegate       `Total None [A.Toption A.vtkeyhash]                                   (`Ty A.Toperation          ) Mint.empty;
     op "key_hash_to_contract" A.Ckeyhashtocontract `Total None [A.vtkeyhash]                                             (`Ty (A.Tcontract A.vtunit)) Mint.empty;
     op "voting_power"         A.Cvotingpower       `Total None [A.vtkeyhash]                                             (`Ty A.vtnat               ) Mint.empty;
     op "contract_to_address"  A.Ccontracttoaddress `Total None [A.Tcontract (A.Tnamed 0)]                                (`Ty A.vtaddress           ) Mint.empty;
     op "key_to_address"       A.Ckeytoaddress      `Total None [A.vtkey]                                                 (`Ty A.vtaddress           ) Mint.empty;
    ]

(* -------------------------------------------------------------------- *)
let mathops : opinfo list =
  [
    op "sub_nat" A.Csubnat `Total (Some A.vtnat) [ A.vtnat ] (`Ty (A.Toption A.vtnat)) Mint.empty;
    op "sub_mutez" A.Csubmutez `Total (Some A.vtcurrency) [ A.vtcurrency ] (`Ty (A.Toption A.vtcurrency)) Mint.empty;
    op "greedy_and" A.Cgreedyand `Total (Some A.vtbool) [ A.vtbool ] (`Ty A.vtbool) Mint.empty;
    op "greedy_or" A.Cgreedyor `Total (Some A.vtbool) [ A.vtbool ] (`Ty A.vtbool) Mint.empty
  ]

(* -------------------------------------------------------------------- *)
let packops : opinfo list =
  [op "pack" A.Cpack `Total None [A.Tnamed 0] (`Ty A.vtbytes) (Mint.of_list [0, `MichelsonPackable])]

(* -------------------------------------------------------------------- *)
let opsops : opinfo list =
  [op "make_operation" A.Cmakeoperation `Total None
     [A.vtcurrency; A.Tcontract (A.Tnamed 0); A.Tnamed 0] (`Ty A.Toperation) Mint.empty]

(* -------------------------------------------------------------------- *)
let lambdaops : opinfo list = [
  op "exec_lambda" A.Cexec `Total (Some (A.Tlambda (A.Tnamed 0, A.Tnamed 1))) [A.Tnamed 0] (`Ty (A.Tnamed 1)) Mint.empty;
  op "apply_lambda" A.Capply `Total (Some (A.Tlambda (A.Ttuple [A.Tnamed 0; A.Tnamed 1], A.Tnamed 2))) [A.Tnamed 0] (`Ty (A.Tlambda (A.Tnamed 1, A.Tnamed 2))) Mint.empty;
]

(* -------------------------------------------------------------------- *)
let ticket_ops : opinfo list =
  [
    op "create_ticket" A.Ccreateticket `Total None[A.Tnamed 0; A.vtnat]
      (`Ty (A.Tticket (A.Tnamed 0))) Mint.empty;
    op "read_ticket" A.Creadticket `Total None [A.Tticket (A.Tnamed 0)]
      (`Ty (A.Ttuple [A.vtaddress; A.Tnamed 0; A.vtnat])) Mint.empty;
    op "split_ticket" A.Csplitticket `Total None [A.Tticket (A.Tnamed 0); A.vtnat; A.vtnat]
      (`Ty (A.Toption (A.Ttuple [A.Tticket (A.Tnamed 0); A.Tticket (A.Tnamed 0)]))) Mint.empty;
    op "join_tickets" A.Cjointickets `Total None [A.Tticket (A.Tnamed 0); A.Tticket (A.Tnamed 0)]
      (`Ty (A.Toption (A.Tticket (A.Tnamed 0)))) Mint.empty;
  ]

let bls_ops : opinfo list =
  [
    op "pairing_check" A.Cpairing_check `Total None
      [A.Tlist (A.Ttuple [A.vtbls12_381_g1; A.vtbls12_381_g2])] (`Ty A.vtbool) Mint.empty;
  ]

let timelock_ops : opinfo list =
  [
    op "open_chest" A.Copen_chest `Total None
      [A.vtchest_key; A.vtchest; A.vtnat] (`Ty (A.Tor (A.vtbytes, A.vtbool))) Mint.empty;
  ]

(* -------------------------------------------------------------------- *)
let allops : opinfo list =
  coreops @ optionops @ setops @ listops @ mapops @ bigmapops @ iterablebigmapops @
  cryptoops @ packops @ opsops @ lambdaops @
  ticket_ops @ bls_ops @ mathops @ timelock_ops

(* -------------------------------------------------------------------- *)
type importdecl = {
  id_name        : A.lident;
  id_path        : A.lident;
  id_content     : Michelson.obj_micheline;
  id_entrypoints : (ident * A.type_) list;
  id_views       : (ident * (A.type_ * A.type_)) list;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type assetdecl = {
  as_name   : A.lident;
  as_fields : fielddecl list;
  as_pkty   : A.ptyp;
  as_pk     : A.lident list;
  as_sortk  : A.lident list;
  as_bm     : A.map_kind;
  as_invs   : (A.lident option * A.pterm) list;
  as_state  : A.lident option;
  as_init   : (A.pterm list) list;
}
[@@deriving show {with_path = false}]

and fielddecl = {
  fd_name  : A.lident;
  fd_type  : A.ptyp;
  fd_dfl   : A.pterm option;
  fd_ghost : bool;
}

let get_field (x : ident) (decl : assetdecl) =
  List.Exn.find (fun fd -> x = L.unloc fd.fd_name) decl.as_fields

(* -------------------------------------------------------------------- *)
type recorddecl = {
  rd_name    : A.lident;
  rd_fields  : rfielddecl list;
  rd_packing : rpacking option;
}
[@@deriving show {with_path = false}]

and rfielddecl = {
  rfd_name  : A.lident;
  rfd_type  : A.ptyp;
  rfd_dfl   : A.pterm option;
}

and rpacking =
  | RLeaf of A.lident
  | RNode of rpacking list

let get_rfield (x : ident) (decl : recorddecl) =
  List.Exn.find (fun fd -> x = L.unloc fd.rfd_name) decl.rd_fields

(* -------------------------------------------------------------------- *)
type vardecl = {
  vr_name   : A.lident;
  vr_type   : A.ptyp;
  vr_kind   : [`Constant | `Variable | `Ghost | `Enum];
  vr_invs   : A.lident A.label_term list;
  vr_def    : (A.pterm * [`Inline | `Std]) option;
  vr_core   : A.const option;
}

(* -------------------------------------------------------------------- *)
type 'env ispecification = [
  | `Predicate     of A.lident * (A.lident * A.ptyp) list * A.pterm
  | `Definition    of A.lident * (A.lident * A.ptyp) * A.pterm
  | `Fails         of (A.lident * A.lident option * A.lident * A.ptyp * A.pterm) list
  | `Variable      of A.lident * A.pterm option
  | `Asset         of A.lident * A.pterm * (A.lident * A.pterm list) list * A.lident list
  | `Effect        of 'env * A.instruction
  | `Postcondition of A.lident * A.pterm * (A.lident * A.pterm list) list * A.lident list
]

(* -------------------------------------------------------------------- *)
type 'env fundecl = {
  fs_name  : A.lident;
  fs_kind  : A.fun_kind;
  fs_args  : (A.lident * A.ptyp) list;
  fs_retty : A.ptyp;
  fs_body  : A.instruction;
  fs_spec  : 'env ispecification list;
}

(* -------------------------------------------------------------------- *)
type preddecl = {
  pr_name  : A.lident;
  pr_args  : (A.lident * A.ptyp) list;
  pr_body  : A.pterm;
}

(* -------------------------------------------------------------------- *)
type txeffect = {
  tx_state  : A.lident;
  tx_when   : A.pterm option;
  tx_effect : A.instruction option;
}

type 'env tentrydecl = {
  ad_name   : A.lident;
  ad_args   : (A.lident * A.ptyp) list;
  ad_srcby  : (A.pterm option) loced list * A.pterm option;
  ad_callby : (A.pterm option) loced list * A.pterm option;
  ad_stateis: (A.lident * A.pterm option) option;
  ad_effect : [`Raw of A.instruction | `Tx of transition] option;
  ad_funs   : 'env fundecl option list;
  ad_csts   : (A.lident option * A.pterm * A.pterm option) list;
  ad_reqs   : (A.lident option * A.pterm * A.pterm option) list;
  ad_fais   : (A.lident option * A.pterm * A.pterm option) list;
  ad_spec   : 'env ispecification list;
  ad_actfs  : bool * A.pterm option;
}

and transition = A.sexpr * (A.lident * assetdecl) option * txeffect list

(* -------------------------------------------------------------------- *)
type statedecl = {
  sd_name  : A.lident;
  sd_state : bool;
  sd_ctors : ctordecl list;
  sd_init  : ident;
}

and ctordecl = A.lident * A.ptyp list * (A.lident option * A.pterm) list

let get_ctor (x : ident) (ctors : ctordecl list) =
  List.Exn.find (fun (y, _, _) -> unloc y = x) ctors

(* -------------------------------------------------------------------- *)
type definitiondecl = {
  df_name  : A.lident;
  df_arg   : A.lident * A.ptyp;
  df_asset : A.lident;
  df_body  : A.pterm;
}

(* -------------------------------------------------------------------- *)
let pterm_arg_as_pterm = function A.AExpr e -> Some e | _ -> None

(* -------------------------------------------------------------------- *)
let core_types = [
  ("unit"         , A.vtunit             );
  ("string"       , A.vtstring           );
  ("nat"          , A.vtnat              );
  ("int"          , A.vtint              );
  ("rational"     , A.vtrational         );
  ("bool"         , A.vtbool             );
  ("address"      , A.vtaddress          );
  ("date"         , A.vtdate             );
  ("tez"          , A.vtcurrency         );
  ("duration"     , A.vtduration         );
  ("signature"    , A.vtsignature        );
  ("key"          , A.vtkey              );
  ("key_hash"     , A.vtkeyhash          );
  ("bytes"        , A.vtbytes            );
  ("chain_id"     , A.vtchainid          );
  ("operation"    , A.Toperation         );
  ("bls12_381_fr" , A.vtbls12_381_fr     );
  ("bls12_381_g1" , A.vtbls12_381_g1     );
  ("bls12_381_g2" , A.vtbls12_381_g2     );
  ("never"        , A.vtnever            );
  ("chest"        , A.vtchest            );
  ("chest_key"    , A.vtchest_key        )
]

(* -------------------------------------------------------------------- *)
let ident_of_pname (x : ParseTree.pname) =
  match x with
  | PIdent x -> x
  | PCons    -> "$cons"
  | PNil     -> "$nil"
  | PSome    -> "$some"
  | PNone    -> "$none"
  | PLeft    -> "$left"
  | PRight   -> "$right"

(* -------------------------------------------------------------------- *)
module Env : sig
  type t

  type label_kind = [`Plain | `Code | `Loop of A.ptyp]

  type entry = [
    | `Label       of t * label_kind
    | `State       of statedecl
    | `StateByCtor of statedecl * A.lident
    | `Type        of A.ptyp
    | `Local       of A.ptyp * locvarkind
    | `Global      of vardecl
    | `Definition  of definitiondecl
    | `Asset       of assetdecl
    | `Record      of recorddecl
    | `Event       of recorddecl
    | `Entry       of t tentrydecl
    | `Function    of t fundecl
    | `Predicate   of preddecl
    | `Field       of ident * [`Asset | `Record | `Event]
    | `Context     of assetdecl * ident option
    | `Import      of importdecl
  ]

  and locvarkind = [`Standard | `Const | `Argument | `Pattern | `LoopIndex]

  type ecallback = error -> unit

  val create       : ecallback -> t
  val emit_error   : t -> error -> unit
  val name_free    : t -> ident -> [`Free | `Clash of Location.t option]
  val lookup_entry : t -> ident -> entry option
  val open_        : t -> t
  val close        : t -> t
  val inscope      : t -> (t -> t * 'a) -> t * 'a

  module Label : sig
    val lookup : t -> ident -> (t * label_kind) option
    val get    : t -> ident -> t * label_kind
    val exists : t -> ident -> bool
    val push   : t -> A.lident * label_kind -> t
  end

  module Type : sig
    val lookup  : t -> ident -> A.ptyp option
    val get     : t -> ident -> A.ptyp
    val exists  : t -> ident -> bool
    val push    : t -> (A.lident * A.ptyp) -> t
  end

  module Local : sig
    val lookup : t -> ident -> (ident * (A.ptyp * locvarkind)) option
    val get    : t -> ident -> (ident * (A.ptyp * locvarkind))
    val exists : t -> ident -> bool
    val push   : t -> ?kind:locvarkind -> A.lident * A.ptyp -> t
    val pushn  : t -> ?kind:locvarkind -> (A.lident * A.ptyp) list -> t
  end

  module Definition : sig
    val lookup : t -> ident -> definitiondecl option
    val get    : t -> ident -> definitiondecl
    val exists : t -> ident -> bool
    val push   : t -> definitiondecl -> t
  end

  module Var : sig
    val lookup : t -> ident -> vardecl option
    val get    : t -> ident -> vardecl
    val exists : t -> ident -> bool
    val push   : t -> vardecl -> t
  end

  module Function : sig
    val lookup : t -> ident -> t fundecl option
    val get    : t -> ident -> t fundecl
    val exists : t -> ident -> bool
    val push   : t -> t fundecl -> t
  end

  module Predicate : sig
    val lookup : t -> ident -> preddecl option
    val get    : t -> ident -> preddecl
    val exists : t -> ident -> bool
    val push   : t -> preddecl -> t
  end

  module State : sig
    val lookup : t -> ident -> statedecl option
    val get    : t -> ident -> statedecl
    val exists : t -> ident -> bool
    val byctor : t -> ident -> statedecl option
    val push   : t -> statedecl -> t
  end

  module Record : sig
    val lookup  : t -> ident -> recorddecl option
    val get     : t -> ident -> recorddecl
    val exists  : t -> ident -> bool
    val byfield : t -> ident -> (recorddecl * rfielddecl) option
    val push    : t -> recorddecl -> t
  end

  module Event : sig
    val lookup  : t -> ident -> recorddecl option
    val get     : t -> ident -> recorddecl
    val exists  : t -> ident -> bool
    val byfield : t -> ident -> (recorddecl * rfielddecl) option
    val push    : t -> recorddecl -> t
  end

  module Asset : sig
    val lookup  : t -> ident -> assetdecl option
    val get     : t -> ident -> assetdecl
    val exists  : t -> ident -> bool
    val byfield : t -> ident -> (assetdecl * fielddecl) option
    val push    : t -> assetdecl -> t
  end

  module Tentry : sig
    val lookup  : t -> ident -> t tentrydecl option
    val get     : t -> ident -> t tentrydecl
    val exists  : t -> ident -> bool
    val push    : t -> t tentrydecl -> t
  end

  module Context : sig
    val the  : ident
    val push : t -> ident -> t
  end

  module Import : sig
    val lookup  : t -> ident -> importdecl option
    val get     : t -> ident -> importdecl
    val push    : t -> importdecl -> t
  end
end = struct
  type ecallback = error -> unit

  type label_kind = [`Plain | `Code | `Loop of A.ptyp]

  type entry = [
    | `Label       of t * label_kind
    | `State       of statedecl
    | `StateByCtor of statedecl * A.lident
    | `Type        of A.ptyp
    | `Local       of A.ptyp * locvarkind
    | `Global      of vardecl
    | `Definition  of definitiondecl
    | `Asset       of assetdecl
    | `Record      of recorddecl
    | `Event       of recorddecl
    | `Entry       of t tentrydecl
    | `Function    of t fundecl
    | `Predicate   of preddecl
    | `Field       of ident * [`Asset | `Record | `Event ]
    | `Context     of assetdecl * ident option
    | `Import      of importdecl
  ]

  and locvarkind = [`Standard | `Const | `Argument | `Pattern | `LoopIndex]

  and t = {
    env_error    : ecallback;
    env_bindings : (Location.t option * entry) Mid.t;
    env_context  : assetdecl list;
    env_locals   : Sid.t;
    env_scopes   : Sid.t list;
  }

  let ctxtname = "the"

  let create ecallback : t =
    { env_error    = ecallback;
      env_bindings = Mid.empty;
      env_context  = [];
      env_locals   = Sid.empty;
      env_scopes   = []; }

  let emit_error (env : t) (e : error) =
    env.env_error e

  let name_free (env : t) (x : ident) =
    if x = ctxtname then `Clash None else

      Option.map fst (Mid.find_opt x env.env_bindings)
      |> Option.map_dfl (fun x -> `Clash x) `Free

  let lookup_entry (env : t) (name : ident) : entry option =
    if   name = ctxtname
    then begin
      match Mid.find_opt name env.env_bindings with
      | Some v -> Some (snd v)
      | _ -> Option.map (fun x -> `Context (x, None)) (List.ohead env.env_context)
    end
    else Option.map snd (Mid.find_opt name env.env_bindings)

  let lookup_gen (proj : entry -> 'a option) (env : t) (name : ident) : 'a option =
    Option.bind proj (lookup_entry env name)

  let push (env : t) ?(loc : Location.t option) (name : ident) (entry : entry) =
    let env = { env with
                env_bindings = Mid.add name (loc, entry) env.env_bindings } in

    match entry with
    | `Local _ -> { env with env_locals = Sid.add name env.env_locals }
    | _        -> env

  let open_ (env : t) =
    { env with
      env_locals = Sid.empty;
      env_scopes = env.env_locals :: env.env_scopes; }

  let close (env : t) =
    let lc, sc =
      match env.env_scopes with lc :: sc -> lc, sc | _ -> assert false in

    let bds =
      Sid.fold
        (fun x bds -> Mid.remove x bds) env.env_locals env.env_bindings in

    { env with env_bindings = bds; env_locals = lc; env_scopes = sc; }

  let inscope (env : t) (f : t -> t * 'a) =
    let env, aout = f (open_ env) in (close env, aout)

  module Label = struct
    let proj (entry : entry) =
      match entry with
      | `Label x    -> Some x
      | _           -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) ((name, kind) : A.lident * label_kind) =
      push env ~loc:(loc name) (unloc name) (`Label (env, kind))
  end

  module Type = struct
    let proj (entry : entry) =
      match entry with
      | `Type  x      -> Some x
      | `Asset decl   -> Some (A.Tasset decl.as_name)
      | `State decl   -> Some (A.Tenum decl.sd_name)
      | `Record decl  -> Some (A.Trecord decl.rd_name)
      | `Event  decl  -> Some (A.Tevent decl.rd_name)
      | _             -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) ((name, ty) : A.lident * A.ptyp) =
      push env ~loc:(loc name) (unloc name) (`Type ty)
  end

  module State = struct
    let proj (entry : entry) =
      match entry with
      | `State x -> Some x
      | _        -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let byctor (env : t) (name : ident) =
      match lookup_entry env name with
      | Some (`StateByCtor (decl, _)) -> Some decl
      | _ -> None

    let push (env : t) (decl : statedecl) =
      let env =
        List.fold_left
          (fun env (name, _, _) ->
             (push env ~loc:(loc name) (unloc name) (`StateByCtor (decl, name))))
          env decl.sd_ctors in
      push env (unloc decl.sd_name) (`State decl)
  end

  module Local = struct
    let proj = function `Local x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      Option.map (fun ty -> (name, ty)) (lookup_gen proj env name)

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) ?(kind = `Standard) ((x, ty) : A.lident * A.ptyp) =
      push env ~loc:(loc x) (unloc x) (`Local (ty, kind))

    let pushn (env : t) ?kind (xty : (A.lident * A.ptyp) list) =
      List.fold_left (push ?kind) env xty
  end

  module Definition = struct
    let proj = function `Definition x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (decl : definitiondecl) =
      push env ~loc:(loc decl.df_name) (unloc decl.df_name) (`Definition decl)
  end

  module Var = struct
    let proj = function
      | `Global x ->
        Some x

      | `Asset  a ->
        Some { vr_name = a.as_name;
               vr_type = A.Tcontainer (A.Tasset a.as_name, A.Collection);
               vr_kind = `Constant;
               vr_invs = [];
               vr_core = None;
               vr_def  = None; }

      | `StateByCtor (enum, ctor) ->
        Some { vr_name = ctor;
               vr_type = A.Tenum enum.sd_name;
               vr_kind = `Enum;
               vr_invs = [];
               vr_core = None;
               vr_def  = None; }

      | `Definition def ->
        Some { vr_name = def.df_name;
               vr_type = A.Tcontainer (A.Tasset def.df_asset, A.AssetView);
               vr_kind = `Ghost;
               vr_invs = [];
               vr_core = None;
               vr_def  = None; }

      | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (decl : vardecl) =
      push env ~loc:(loc decl.vr_name) (unloc decl.vr_name) (`Global decl)
  end

  module Function = struct
    let proj = function `Function x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (decl : t fundecl) =
      push env ~loc:(loc decl.fs_name) (unloc decl.fs_name) (`Function decl)
  end

  module Predicate = struct
    let proj = function `Predicate x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (decl : preddecl) =
      push env ~loc:(loc decl.pr_name) (unloc decl.pr_name) (`Predicate decl)
  end

  module Asset = struct
    let proj = function `Asset x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let byfield (env : t) (fname : ident) =
      Option.bind
        (function
          | `Field (nm, `Asset) ->
            let decl  = get env nm in
            let field = get_field fname decl in
            Some (decl, Option.get field)
          | _ -> None)
        (lookup_entry env fname)

    let push (env : t) ({ as_name = nm } as decl : assetdecl) : t =
      let env = push env ~loc:(loc nm) (unloc nm) (`Asset decl) in
      List.fold_left
        (fun env fd -> push env ~loc:(loc fd.fd_name)
            (unloc fd.fd_name) (`Field (unloc nm, `Asset)))
        env decl.as_fields
  end

  module Record = struct
    let proj = function `Record x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let byfield (env : t) (fname : ident) =
      Option.bind
        (function
          | `Field (nm, `Record) ->
            let decl  = get env nm in
            let field = get_rfield fname decl in
            Some (decl, Option.get field)
          | _ -> None)
        (lookup_entry env fname)

    let push (env : t) ({ rd_name = nm } as decl : recorddecl) : t =
      let env = push env ~loc:(loc nm) (unloc nm) (`Record decl) in
      List.fold_left
        (fun env fd -> push env ~loc:(loc fd.rfd_name)
            (unloc fd.rfd_name) (`Field (unloc nm, `Record)))
        env decl.rd_fields
  end

  module Event = struct
    let proj = function `Event x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let byfield (env : t) (fname : ident) =
      Option.bind
        (function
          | `Field (nm, `Event) ->
            let decl  = get env nm in
            let field = get_rfield fname decl in
            Some (decl, Option.get field)
          | _ -> None)
        (lookup_entry env fname)

    let push (env : t) ({ rd_name = nm } as decl : recorddecl) : t =
      let env = push env ~loc:(loc nm) (unloc nm) (`Event decl) in
      List.fold_left
        (fun env fd -> push env ~loc:(loc fd.rfd_name)
            (unloc fd.rfd_name) (`Field (unloc nm, `Event)))
        env decl.rd_fields
  end

  module Tentry = struct
    let proj = function `Entry x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (act : t tentrydecl) =
      push env ~loc:(loc act.ad_name) (unloc act.ad_name) (`Entry act)
  end

  module Context = struct
    let the : ident = ctxtname

    let push (env : t) (asset : ident) =
      let asset = Asset.get env asset in
      { env with
        env_context  = asset :: env.env_context;
        env_bindings = List.fold_left (fun bds fd ->
            Mid.add (unloc fd.fd_name)
              (None, `Context (asset, Some (unloc fd.fd_name))) bds
          ) env.env_bindings asset.as_fields; }

    (* let push_gen (env : t) (pty : A.type_) =
       { env with
        env_bindings =
        env.env_bindings
        List.fold_left (fun bds fd ->
            Mid.add (unloc fd.fd_name)
              (None, `Context (asset, Some (unloc fd.fd_name))) bds
          )  asset.as_fields; } *)
  end

  module Import = struct
    let proj = function `Import x -> Some x | _ -> None

    let lookup (env : t) (name : ident) : importdecl option =
      lookup_gen proj env name

    let get (env : t) (name : ident) : importdecl =
      Option.get (lookup env name)

    let push (env : t) (ipt : importdecl) : t =
      push env ~loc:(loc ipt.id_name) (unloc ipt.id_name) (`Import ipt)
  end

end

type env = Env.t

module Micheline : sig
  val parse      : string -> T.obj_micheline option
  val seek       : T.obj_micheline -> string -> T.obj_micheline option
  val seek_views : T.obj_micheline -> (string * T.obj_micheline * T.obj_micheline) list
  val split      : T.obj_micheline -> (string * T.obj_micheline) list
  val for_type   : T.obj_micheline -> A.ptyp

  val get_entrypoints  : T.obj_micheline -> (string * A.type_) list option
  val get_views        : T.obj_micheline -> (string * (A.type_ * A.type_)) list
  val get_storage_type : T.obj_micheline -> A.type_ option
end = struct

  let parse path =
    if Core.is_valid_path path
    then
      let ic = open_in path in
      let obj, _ = Gen_decompile.parse_micheline (FIChannel (path, ic)) in
      Some obj
    else None

  let seek (obj : T.obj_micheline) prim : T.obj_micheline option =
    match obj with
    | T.Oarray l -> begin
        let op = List.fold_left (fun accu x -> match x with | T.Oprim p when String.equal p.prim prim -> Some p | _ -> accu) None l in
        match op with
        | Some p -> Some (List.nth p.args 0)
        | _ -> None
      end
    | _ -> None

  let seek_views (obj : T.obj_micheline) : (string * T.obj_micheline * T.obj_micheline) list =
    match obj with
    | T.Oarray l ->
      List.fold_right (
        fun x accu ->
          match x with
          | T.Oprim {prim = "view"; args = Ostring id::i::r::_ } -> (id, i, r)::accu
          | _ -> accu
      ) l []
    | _ -> []

  let split (obj : T.obj_micheline) : (string * T.obj_micheline) list =
    let rec aux obj =
      match obj with
      | T.Oprim {prim = p; args = l} when String.equal p "or" -> List.map aux l |> List.flatten
      | T.Oprim {annots=[a]} -> [a, obj]
      | _ -> []
    in

    match obj with
    | T.Oprim {prim = p} when String.equal p "or" -> aux obj
    | T.Oprim p when not (List.is_empty p.annots) -> aux obj
    | _ -> ["%default", obj]

  let for_type (obj : T.obj_micheline) : A.ptyp =
    let normalize_type (t : A.ptyp) =
      let list_without_last l =
        match List.rev l with
        | _::t -> List.rev t
        | _ -> l
      in
      let rec aux t =
        match t with
        | A.Ttuple l ->
          let l = List.map aux l in
          let def = A.Ttuple l in
          if List.is_not_empty l
          then begin
            match List.last l with
            | A.Ttuple l2 -> A.Ttuple (list_without_last l @ l2)
            | _ -> def
          end
          else def
        | _ -> A.map_ptyp aux t
      in
      aux t
    in

    let rec aux (t : T.type_) =
      let f = aux in
      match t.node with
      | Tkey                   -> A.Tbuiltin VTkey
      | Tunit                  -> A.Tbuiltin VTunit
      | Tsignature             -> A.Tbuiltin VTsignature
      | Toption    t           -> A.Toption (f t)
      | Tlist      t           -> A.Tlist   (f t)
      | Tset       t           -> A.Tset    (f t)
      | Toperation             -> A.Toperation
      | Tcontract  t           -> A.Tcontract (f t)
      | Tpair      (lt, rt)    -> A.Ttuple [f lt; f rt]
      | Tor        (lt, rt)    -> A.Tor (f lt, f rt)
      | Tlambda    (at, rt)    -> A.Tlambda (f at, f rt)
      | Tmap       (kt, vt)    -> A.Tmap (f kt, f vt)
      | Tbig_map   (kt, vt)    -> A.Tbig_map (f kt, f vt)
      | Tchain_id              -> A.Tbuiltin VTchainid
      | Tint                   -> A.Tbuiltin VTint
      | Tnat                   -> A.Tbuiltin VTnat
      | Tstring                -> A.Tbuiltin VTstring
      | Tbytes                 -> A.Tbuiltin VTbytes
      | Tmutez                 -> A.Tbuiltin VTcurrency
      | Tbool                  -> A.Tbuiltin VTbool
      | Tkey_hash              -> A.Tbuiltin VTkeyhash
      | Ttimestamp             -> A.Tbuiltin VTdate
      | Taddress               -> A.Tbuiltin VTaddress
      | Tticket t              -> A.Tticket (f t)
      | Tsapling_transaction n -> A.Tsapling_transaction n
      | Tsapling_state       n -> A.Tsapling_state n
      | Tbls12_381_fr          -> A.Tbuiltin VTbls12_381_fr
      | Tbls12_381_g1          -> A.Tbuiltin VTbls12_381_g1
      | Tbls12_381_g2          -> A.Tbuiltin VTbls12_381_g2
      | Tnever                 -> A.Tbuiltin VTnever
      | Tchest                 -> A.Tbuiltin VTchest
      | Tchest_key             -> A.Tbuiltin VTchest_key
    in
    aux (T.to_type obj) |> normalize_type

  let get_entrypoints content =
    match seek content "parameter" with
    | Some parameters -> Some (List.map (fun (i, t) -> (i, for_type t)) (split parameters))
    | None -> None

  let get_views content = List.map (fun (i, t, r) -> (i, (for_type t, for_type r))) (seek_views content)

  let get_storage_type content =
    match seek content "storage" with
    | Some v -> Some (for_type v)
    | None -> None

end

let coreloc = { Location.dummy with loc_fname = "<stdlib>" }

let empty : env =
  let cb (lc, error) =
    let str : string = Format.asprintf "%a@." pp_error_desc error in
    let pos : Position.t list = [location_to_position lc] in
    Error.error_alert pos str (fun _ -> ());
  in

  let env = Env.create cb in

  let env =
    List.fold_left
      (fun env (name, ty) -> Env.Type.push env (mkloc coreloc name, ty))
      env core_types in

  let env =
    let mk vr_name vr_type vr_core =
      let def = A.Pconst vr_core in
      let def = A.mk_sp ~type_:vr_type  def in

      { vr_name; vr_type; vr_core = Some vr_core;
        vr_def  = Some (def, `Inline);
        vr_kind = `Constant;
        vr_invs = [];
      } in

    List.fold_left
      (fun env (name, const, ty) ->
         Env.Var.push env (mk (mkloc L.dummy name) ty const))
      env globals in

  env

(* --------------------------------------------------------------------- *)
let ty_of_init_ty (env : env) (ty : A.ptyp) =
  match ty with
  | A.Tcontainer (A.Tasset { pldesc = asset }, ctn) when Env.Asset.exists env asset ->
    let asset = Env.Asset.get env asset in
    let pk = List.map
        (fun x -> (Option.get (get_field (unloc x) asset)).fd_type) asset.as_pk in
    A.Tcontainer (Type.create_tuple pk, ctn)

  | _ -> ty

(* -------------------------------------------------------------------- *)
let check_and_emit_name_free (env : env) (x : A.lident) =
  match Env.name_free env (unloc x) with
  | `Free ->
    true

  | `Clash olc ->
    Env.emit_error env (loc x, NameIsAlreadyBound (unloc x, olc));
    false

(* --------------------------------------------------------------------- *)
let select_operator env ?(formula = false) ?(asset = false) loc (op, tys) =
  match op with
  | PT.Cmp (PT.Equal | PT.Nequal) -> begin
      let module E = struct exception NoEq end in

      try
        match tys with
        | [t1; t2] ->
          if not formula && (not (Type.support_eq t1) || not (Type.support_eq t2)) then
            raise E.NoEq;

          if not (Type.compatible ~autoview:false ~for_eq:true ~from_:t1 ~to_:t2) &&
             not (Type.compatible ~autoview:false ~for_eq:true ~from_:t2 ~to_:t1) then
            raise E.NoEq;

          Some ({ osl_sig = [t1; t2]; osl_ret = A.Tbuiltin A.VTbool; })

        | _ ->
          raise E.NoEq

      with E.NoEq ->
        Env.emit_error env (loc, NoMatchingOperator (op, tys)); None
    end

  | _ -> begin
      let ops =
        let filter (sig_ : opsig) =
          Type.sig_compatible ~from_:tys ~to_:sig_.osl_sig
        in List.filter filter (List.assoc_all op opsigs) in

      let ops =
        let extra =
          match asset, op, tys with
          | true, PT.Arith PT.Plus,
            [Tcontainer ((Tasset _) as aty, Partition) as rty;
             Tcontainer ((Tasset _) as sty, Collection)]
            when Type.compatible ~autoview:false ~for_eq:false ~from_:sty ~to_:aty
            -> [{ osl_sig = tys; osl_ret = rty }]

          | true, PT.Arith PT.Plus,
            [Tcontainer (Tasset aty, Aggregate) as rty; Tlist sty]

          | true, PT.Arith PT.Minus,
            [Tcontainer (Tasset aty, (Aggregate | Partition)) as rty; Tlist sty] ->

            let asset = Env.Asset.get env (unloc aty) in

            if Type.compatible ~autoview:false ~for_eq:false ~from_:sty ~to_:asset.as_pkty then
              [{ osl_sig = tys; osl_ret = rty }]
            else []

          | true, PT.Arith PT.Plus,
            [Tmap (kt, vt) as rty; (Tlist (Ttuple [kty; vty]) | Tset (Ttuple [kty; vty]))]
            when Type.compatible ~autoview:false ~for_eq:false ~from_:kty ~to_:kt
              && Type.compatible ~autoview:false ~for_eq:false ~from_:vty ~to_:vt ->
            [{ osl_sig = tys; osl_ret = rty }]

          | true, PT.Arith PT.Plus,
            [Tset kt as rty; (Tlist kty | Tset kty)]
            when Type.compatible ~autoview:false ~for_eq:false ~from_:kty ~to_:kt ->
            [{ osl_sig = tys; osl_ret = rty }]

          | true, PT.Arith PT.Minus,
            [(Tset kt | Tmap (kt, _)) as rty; (Tlist kty | Tset kty)]
            when Type.compatible ~autoview:false ~for_eq:false ~from_:kty ~to_:kt ->
            [{ osl_sig = tys; osl_ret = rty }]

          | _, _, _ -> []

        in ops @ extra
      in

      match ops with
      | [] ->
        Env.emit_error env
          (loc, NoMatchingOperator (op, tys));
        None

      | _::_::_ as sigs -> begin
          let module E = struct exception Bailout end in

          let mind =
            let ds =
              List.pmap (fun sig_ -> Type.sig_distance ~to_:sig_.osl_sig ~from_:tys) ops
            in if List.is_empty ds then None else Some (List.fold_left min max_int ds) in

          let ops =
            List.filter
              (fun sig_ ->
                 let d = Type.sig_distance ~to_:sig_.osl_sig ~from_:tys in
                 d = mind || Option.is_none d)
              ops in

          try
            let sig_ = match ops with [sig_] -> sig_ | _ -> raise E.Bailout in

            List.iter (fun sig2 ->
                if not (Type.sig_compatible ~from_:sig_.osl_sig ~to_:sig2.osl_sig) then
                  raise E.Bailout
              ) sigs;

            Some sig_

          with E.Bailout ->
            Env.emit_error env
              (loc, MultipleMatchingOperator (op, tys, sigs));
            None
        end

      | [sig_] ->
        Some sig_
    end

(* -------------------------------------------------------------------- *)
let rec valid_var_or_arg_type (ty : A.ptyp) =
  match ty with
  | Tnamed    _     -> assert false
  | Tasset    _     -> false
  | Trecord   _     -> true
  | Tenum     _     -> true
  | Tevent    _     -> true
  | Tbuiltin  _     -> true
  | Tset      ty    -> valid_var_or_arg_type ty
  | Tlist     ty    -> valid_var_or_arg_type ty
  | Tmap     (k, v) -> List.for_all valid_var_or_arg_type [k; v]
  | Tbig_map (k, v) -> List.for_all valid_var_or_arg_type [k; v]
  | Titerable_big_map (k, v) -> List.for_all valid_var_or_arg_type [k; v]
  | Tor      (l, r) -> List.for_all valid_var_or_arg_type [l; r]
  | Tlambda  (a, r) -> List.for_all valid_var_or_arg_type [a; r]
  | Ttuple    ty    -> List.for_all valid_var_or_arg_type ty
  | Toption   ty    -> valid_var_or_arg_type ty
  | Tcontract  _    -> true
  | Toperation      -> true
  | Ttrace     _    -> false

  | Tcontainer (_, A.AssetView)      -> true
  | Tcontainer (_, A.AssetKey)       -> true
  | Tcontainer (_, A.AssetValue)     -> true
  | Tcontainer (_, A.AssetContainer) -> true
  | Tcontainer (_,      _)           -> false

  | Tticket             ty -> valid_var_or_arg_type ty
  | Tsapling_state       _ -> true
  | Tsapling_transaction _ -> true

(* -------------------------------------------------------------------- *)
let for_container (_ : env) = function
  | PT.Aggregate       -> A.Aggregate
  | PT.Partition       -> A.Partition
  | PT.AssetContainer  -> A.AssetContainer
  | PT.AssetKey        -> A.AssetKey
  | PT.AssetValue      -> A.AssetValue
  | PT.AssetView       -> A.AssetView

(* -------------------------------------------------------------------- *)
let for_assignment_operator = function
  | PT.ValueAssign  -> A.ValueAssign
  | PT.PlusAssign   -> A.PlusAssign
  | PT.MinusAssign  -> A.MinusAssign
  | PT.MultAssign   -> A.MultAssign
  | PT.DivAssign    -> A.DivAssign
  | PT.AndAssign    -> A.AndAssign
  | PT.OrAssign     -> A.OrAssign

(* -------------------------------------------------------------------- *)
let tt_logical_operator (op : PT.logical_operator) =
  match op with
  | And   -> A.And
  | Or    -> A.Or
  | Xor   -> A.Xor
  | Imply -> A.Imply
  | Equiv -> A.Equiv

(* -------------------------------------------------------------------- *)
let tt_arith_operator (op : PT.arithmetic_operator) =
  match op with
  | Plus   -> A.Plus
  | Minus  -> A.Minus
  | Mult   -> A.Mult
  | DivEuc -> A.DivEuc
  | DivRat -> A.DivRat
  | Modulo -> A.Modulo
  | DivMod -> A.DivMod
  | ThreeWayCmp -> A.ThreeWayCmp
  | ShiftLeft  -> A.ShiftLeft
  | ShiftRight -> A.ShiftRight

(* -------------------------------------------------------------------- *)
let tt_cmp_operator (op : PT.comparison_operator) =
  match op with
  | Equal  -> A.Equal
  | Nequal -> A.Nequal
  | Gt     -> A.Gt
  | Ge     -> A.Ge
  | Lt     -> A.Lt
  | Le     -> A.Le

(* -------------------------------------------------------------------- *)
exception InvalidType

let for_type_exn ?pkey (env : env) =

  let rec doit ?(canasset = false) ((ty, _) : PT.type_t) : A.ptyp =
    match unloc ty with
    | Tref x -> begin
        match Env.Type.lookup env (unloc x) with
        | None ->
          Env.emit_error env (loc x, UnknownTypeName (unloc x));
          raise InvalidType
        | Some (A.Tasset _) when not canasset && Option.is_some pkey ->
          Env.emit_error env (loc x, UsePkeyOfInsteadOfAsset);
          raise InvalidType
        | Some ty -> ty
      end

    | Tcontainer (ty, AssetKey) -> begin
        match doit ~canasset:true ty with
        | A.Tasset x -> begin
            let decl = Env.Asset.get env (unloc x) in

            match pkey with
            | Some map when List.mem (unloc x) map ->
              Tnamed (List.index_of ((=) (unloc x)) map)

            | _ ->
              decl.as_pkty
          end

        | _ ->
          Env.emit_error env (loc (fst ty), NotAnAssetType);
          raise InvalidType
      end

    | Tcontainer (pty, ctn) ->
      let ty = doit ~canasset:true pty in

      if not (Type.is_asset ty) then
        Env.emit_error env (loc (fst pty), ContainerOfNonAsset);
      A.Tcontainer (ty, for_container env ctn)

    | Tset ty ->
      let t = doit ty in

      if not (Type.Michelson.is_comparable t) then
        Env.emit_error env (loc (fst ty), InvalidTypeForSet);

      A.Tset (doit ty)

    | Tlist ty ->
      A.Tlist (doit ty)

    | Tmap (k, v) ->
      let nk, nv = doit k, doit v in

      if not (Type.Michelson.is_comparable nk) then
        Env.emit_error env (loc (fst k), InvalidTypeForMapKey);

      if not (Type.Michelson.is_type nv) then
        Env.emit_error env (loc (fst v), InvalidTypeForMapValue);

      A.Tmap (nk, nv)

    | Tbig_map (k, v) ->
      let nk, nv = doit k, doit v in

      if not (Type.Michelson.is_comparable nk) then
        Env.emit_error env (loc (fst k), InvalidTypeForBigMapKey);

      if not (Type.Michelson.is_big_map_value nv) then
        Env.emit_error env (loc (fst v), InvalidTypeForBigMapValue);

      A.Tbig_map (nk, nv)

    | Titerable_big_map (k, v) ->
      let nk, nv = doit k, doit v in

      if not (Type.Michelson.is_comparable nk) then
        Env.emit_error env (loc (fst k), InvalidTypeForBigMapKey);

      if not (Type.Michelson.is_big_map_value nv) then
        Env.emit_error env (loc (fst v), InvalidTypeForBigMapValue);

      A.Titerable_big_map (nk, nv)

    | Tor (l, r) ->
      let nl, nr = doit l, doit r in

      if not (Type.Michelson.is_type nl) then
        Env.emit_error env (loc (fst l), InvalidTypeForOrLeft);

      if not (Type.Michelson.is_type nr) then
        Env.emit_error env (loc (fst r), InvalidTypeForOrRight);

      A.Tor (nl, nr)

    | Tlambda (a, r) ->
      let na, nr = doit a, doit r in

      if not (Type.Michelson.is_type na) then
        Env.emit_error env (loc (fst a), InvalidTypeForLambdaArgument);

      if not (Type.Michelson.is_type nr) then
        Env.emit_error env (loc (fst r), InvalidTypeForLambdaReturn);

      A.Tlambda (na, nr)

    | Ttuple tys ->
      A.Ttuple (List.map doit tys)

    | Toption ty ->
      A.Toption (doit ty)

    | Tcontract ty ->
      A.Tcontract (doit ty)

    | Tticket ty ->
      A.Tticket (doit ty)

    | Tsapling_state n -> begin
        if (Big_int.lt_big_int n Big_int.zero_big_int) || (Big_int.ge_big_int n (Big_int.big_int_of_int 65536))
        then (Env.emit_error env (loc ty, InvalidValueForMemoSize));
        A.Tsapling_state (Big_int.int_of_big_int n)
      end

    | Tsapling_transaction n -> begin
        if (Big_int.lt_big_int n Big_int.zero_big_int) || (Big_int.ge_big_int n (Big_int.big_int_of_int 65536))
        then (Env.emit_error env (loc ty, InvalidValueForMemoSize));
        A.Tsapling_transaction (Big_int.int_of_big_int n)
      end


  in fun ty -> doit ty

let for_type ?pkey (env : env) (ty : PT.type_t) : A.ptyp option =
  try Some (for_type_exn ?pkey env ty) with InvalidType -> None

(* -------------------------------------------------------------------- *)
let for_asset_type (env : env) (ty : PT.type_t) : A.lident option =
  match Option.map Type.as_asset (for_type env ty) with
  | None ->
    None
  | Some None ->
    Env.emit_error env (loc (fst ty), NotAnAssetType);
    None
  | Some (Some x) ->
    Some x

(* -------------------------------------------------------------------- *)
let for_asset_keyof_type (env : env) (ty : PT.type_t) : A.lident option =
  match unloc (fst ty) with
  | PT.Tcontainer(t, AssetKey) ->
    for_asset_type env t
  | _ ->
    Env.emit_error env (loc (fst ty), NotAKeyOfType);
    None

(* -------------------------------------------------------------------- *)
let for_literal (env : env) (_ety : A.type_ option) (topv : PT.literal loced) : A.bval =
  let mk_sp type_ node = A.mk_sp ~loc:(loc topv) ~type_ node in

  let get_tz_value k tz =
    try
      let res = Core.string_to_big_int_tz k tz in
      let max = Big_int.big_int_of_string "9223372036854775807" in
      if Big_int.gt_big_int res max
      then Env.emit_error env (loc topv, InvalidTezValueOverflow);
      res
    with _
      -> Env.emit_error env (loc topv, InvalidValueForCurrency);
      Big_int.zero_big_int
  in

  match unloc topv with
  | Lbool b ->
    mk_sp A.vtbool (A.BVbool b)

  | Lint i -> mk_sp A.vtint (A.BVint i)

  | Lnat i -> mk_sp A.vtnat (A.BVnat i)

  | Ldecimal str -> begin
      let n, d = Core.decimal_string_to_rational str in
      mk_sp A.vtrational (A.BVrational (n, d))
    end

  | Lstring s ->
    if (not (Core.is_valid_string s))
    then  Env.emit_error env (loc topv, InvalidStringValue);
    mk_sp A.vtstring (A.BVstring s)

  | Ltz tz ->
    mk_sp (A.vtcurrency) (A.BVcurrency (A.Utz, get_tz_value Ktz tz))

  | Lmtz tz ->
    mk_sp (A.vtcurrency) (A.BVcurrency (A.Utz, get_tz_value Kmtz tz))

  | Lutz tz ->
    mk_sp (A.vtcurrency) (A.BVcurrency (A.Utz, get_tz_value Kutz tz))

  | Laddress a ->
    mk_sp A.vtaddress (A.BVaddress a)

  | Lduration d ->
    mk_sp A.vtduration (A.BVduration (Core.string_to_duration d))

  | Ldate d ->
    mk_sp A.vtdate (A.BVdate (Core.string_to_date d))

  | Lbytes s ->
    mk_sp A.vtbytes (A.BVbytes (s))

  | Lpercent n -> begin
      let n, d = Core.string_to_big_int_percent n in
      mk_sp A.vtrational (A.BVrational (n, d))
    end

(* -------------------------------------------------------------------- *)
type imode_t = [`Ghost | `Concrete]
type ekind   = [`Expr of imode_t | `Formula of bool]

type emode_t = {
  em_kind : ekind;
  em_pred : bool;
}

let is_expr_kind (kind : ekind) =
  match kind with `Expr _ -> true | _ -> false

let is_form_kind (kind : ekind) =
  match kind with `Formula _ -> true | _ -> false

let expr_mode imode =
  { em_kind = `Expr imode; em_pred = false; }

let form_mode (invariant : bool) =
  { em_kind = `Formula  invariant ; em_pred = false; }

(* -------------------------------------------------------------------- *)
let decompile_match_with kd (bsm : (A.pattern * 'a) list) =
  let find (name : string) (n : int) =
    List.find_map (fun ((ptn, v) : A.pattern * _) ->
        match ptn.A.node with
        | A.Mconst (x, args) when unloc x = name && n = List.length args ->
          Some (args, v)
        | A.Mwild ->
          Some (List.init n (fun _ -> Location.mkloc dummy "_"), v)
        | _ -> None)
      bsm in

  let module E = struct exception Bailout end in

  let find (name : string) (n : int) =
    Option.get_exn E.Bailout (find name n) in

  try
    match kd with
    | `List _ ->
      let (x, xs), bcons = fst_map (List.as_seq2 %> Option.get) (find "$cons" 2) in
      let _, bnil        = find "$nil" 0 in

      Some (`List ((x, xs, bcons), bnil))

    | `Option _ ->
      let x, bsome = fst_map (List.as_seq1 %> Option.get) (find "$some" 1) in
      let _, bnone = find "$none" 0 in

      Some (`Option ((x, bsome), bnone))

    | `Or _ ->
      let (xl, bl) = fst_map (List.as_seq1 %> Option.get) (find "$left"  1) in
      let (xr, br) = fst_map (List.as_seq1 %> Option.get) (find "$right" 1) in

      Some (`Or ((xl, bl), (xr, br)))

    | `Enum -> raise E.Bailout

  with E.Bailout -> None

(* -------------------------------------------------------------------- *)
type capture = {
  cp_global : bool;
  cp_local  : [
    | `Only of Sid.t
    | `Yes  of ((L.t * A.ptyp) Mid.t ref) option
  ];
}

let capture0 : capture = {
  cp_global = true; cp_local = `Yes None;
}

let extend_capture (capture : capture) (xs : ident list) =
  let cp_local =
    match capture.cp_local with
    | `Yes _ -> capture.cp_local
    | `Only ids -> `Only (List.fold_right Sid.add xs ids)
  in { capture with cp_local; }

(* -------------------------------------------------------------------- *)
let rec for_xexpr
    (mode : emode_t) ?autoview ?(capture = capture0)
    (env : env) ?(ety : A.ptyp option) (tope : PT.expr)
  =
  let for_xexpr ?(capture = capture) e =
    for_xexpr mode ~capture e in

  let module E = struct exception Bailout end in

  let bailout = fun () -> raise E.Bailout in

  let mk_sp type_ node = A.mk_sp ~loc:(loc tope) ?type_ node in
  let dummy type_ : A.pterm = mk_sp type_ (A.Pvar (VTnone, Vnone, mkloc (loc tope) "<error>")) in

  let doit () =
    match unloc tope with
    | Eterm ((vset, pvt), x) -> begin
        let vt, subenv =
          match pvt with
          | Some VLBefore ->
            A.VTbefore, env

          | Some (VLIdent lbl) -> begin
              match Env.Label.lookup env (unloc lbl) with
              | None ->
                Env.emit_error env (loc lbl, UnknownLabel (unloc lbl));
                A.VTnone, env
              | Some (subenv, `Code) ->
                A.VTat (unloc lbl), subenv
              | Some (_, _) ->
                Env.emit_error env (loc lbl, NonCodeLabel (unloc lbl));
                A.VTnone, env
            end

          | None ->
            A.VTnone, env
        in

        let vt =
          match vt, mode.em_kind with
          | A.VTnone  , _
          | A.VTat _  , `Formula true
          | A.VTbefore, `Formula _    -> vt

          | _, `Expr _ ->
            Env.emit_error env (loc tope, BeforeOrLabelInExpr);
            A.VTnone

          | _, `Formula _ ->
            Env.emit_error env (loc tope, LabelInNonInvariant);
            A.VTnone
        in


        let lk = Env.lookup_entry subenv (unloc x) in

        begin match lk, vset with
          | None, _ | _, None | Some (`Asset _), _ -> ()
          | Some _, Some _ ->
            Env.emit_error env (loc tope, VSetOnNonAsset)
        end;

        if is_expr_kind mode.em_kind && Option.is_some vset then
          Env.emit_error env (loc tope, VSetInExpr);

        let vs =
          match vset with
          | None           -> A.Vnone
          | Some VSAdded   -> A.Vadded
          | Some VSRemoved -> A.Vremoved
          | Some VSUnmoved -> A.Vunmoved
        in

        match lk with
        | Some (`Local (xty, _)) ->
          let vt =
            if pvt = Some VLBefore then begin
              Env.emit_error env (loc tope, BeforeIrrelevant `Local); A.VTnone
            end else vt in

          begin match capture.cp_local with
            | `Only m when not (Sid.mem (unloc x) m)  ->
              Env.emit_error env (loc tope, CannotCaptureVariables);
            | `Yes (Some lmap) ->
              lmap := Mid.add (unloc x) (loc x, xty) !lmap
            | `Only _ | `Yes None ->
              () end;

          mk_sp (Some xty) (A.Pvar (vt, vs, x))

        | Some (`Global decl) -> begin
            if not capture.cp_global then
              Env.emit_error env (loc tope, CannotCaptureVariables);

            begin match mode.em_kind, decl.vr_kind with
              | `Expr `Concrete, `Ghost ->
                Env.emit_error env (loc tope, InvalidShadowVariableAccess)
              | _, _ -> ()
            end;

            match decl.vr_def with
            | Some (body, `Inline) ->
              { body with loc = loc tope }
            | _ ->
              mk_sp (Some decl.vr_type) (A.Pvar (vt, vs, x))
          end

        | Some (`Asset decl) ->
          let typ = A.Tcontainer ((A.Tasset decl.as_name), A.Collection) in
          mk_sp (Some typ) (A.Pvar (vt, vs, x))

        | Some (`Definition decl) ->
          let typ = A.Tcontainer ((A.Tasset decl.df_asset), A.AssetView) in
          mk_sp (Some typ) (A.Pvar (vt, vs, x))

        | Some (`StateByCtor (decl, ctor)) ->
          let vt =
            if pvt = Some VLBefore then begin
              Env.emit_error env (loc tope, BeforeIrrelevant `State); A.VTnone
            end else vt in

          let ctor = Option.get (get_ctor (unloc ctor) decl.sd_ctors) in

          if not (List.is_empty (proj3_2 ctor)) then begin
            Env.emit_error env
              (loc x, InvalidNumberOfArguments (0, List.length (proj3_2 ctor)));
            bailout ()
          end;

          let typ = A.Tenum decl.sd_name in
          mk_sp (Some typ) (A.Pvar (vt, vs, x))

        | Some (`Context (asset, ofield)) -> begin
            let atype = A.Tasset asset.as_name in
            let var   = mkloc (loc tope) Env.Context.the in
            let the   = mk_sp (Some atype) (A.Pvar (vt, Vnone, var)) in

            match ofield with
            | None ->
              the
            | Some fname ->
              let fty = (Option.get (get_field fname asset)).fd_type in
              mk_sp (Some fty) (A.Pdot (the, mkloc (loc tope) fname))
          end

        | _ ->
          Env.emit_error env (loc x, UnknownLocalOrVariable (unloc x));
          bailout ()
      end

    | Eliteral v ->
      let v = for_literal env ety (mkloc (loc tope) v) in
      mk_sp v.A.type_ (A.Plit v)

    | Earray [] -> begin
        match ety with
        | Some (A.Tcontainer (_, _))
        | Some (A.Tset _ | A.Tlist _ | A.Tmap _ | A.Tbig_map _ | A.Titerable_big_map _) ->
          mk_sp ety (A.Parray [])

        | _ ->
          Env.emit_error env (loc tope, CannotInferCollectionType);
          bailout ()
      end

    | Earray (e :: es) -> begin
        let elty = Option.bind Type.as_content_array ety in
        let e    = for_xexpr env ?ety:elty e in
        let elty = if Option.is_some e.A.type_ then e.A.type_ else elty in
        let es   = List.map (fun e -> for_xexpr env ?ety:elty e) es in

        match ety, elty with
        | Some (A.Tcontainer (_, k)), Some ty ->
          mk_sp (Some (A.Tcontainer (ty, k))) (A.Parray (e :: es))

        | None, Some ((A.Tasset _) as ty) ->
          mk_sp (Some (A.Tcontainer (ty, A.Collection))) (A.Parray (e :: es))

        | Some Tset _, Some ty ->
          mk_sp (Some (A.Tset ty)) (A.Parray (e :: es))

        | Some Tmap _, Some ty ->
          let k, v  =
            match ty with
            | Ttuple [k; v] -> (k, v)
            | _ -> (Env.emit_error env (loc tope, InvalidMapType); bailout ())
          in
          mk_sp (Some (A.Tmap (k, v))) (A.Parray (e :: es))

        | Some Tbig_map _, Some ty ->
          let k, v  =
            match ty with
            | Ttuple [k; v] -> (k, v)
            | _ -> (Env.emit_error env (loc tope, InvalidMapType); bailout ())
          in
          mk_sp (Some (A.Tbig_map (k, v))) (A.Parray (e :: es))

        | Some Titerable_big_map _, Some ty ->
          let k, v  =
            match ty with
            | Ttuple [k; v] -> (k, v)
            | _ -> (Env.emit_error env (loc tope, InvalidMapType); bailout ())
          in
          mk_sp (Some (A.Titerable_big_map (k, v))) (A.Parray (e :: es))

        | _, Some ty ->
          mk_sp (Some (A.Tlist ty)) (A.Parray (e :: es))

        | _ ->
          Env.emit_error env (loc tope, CannotInferCollectionType);
          bailout ()
      end

    | Erecord fields -> begin
        let module E = struct
          type state = {
            hasupdate : bool;
            fields    : ident list;
            anon      : bool;
          }

          let state0 = {
            hasupdate = false; fields = []; anon = false;
          }
        end in

        let is_update = function
          | (None | Some (PT.ValueAssign, _)) -> false
          |  _ -> true in

        let infos = List.fold_left (fun state (fname, _) ->
            E.{ hasupdate = state.hasupdate || is_update fname;
                fields    = Option.fold
                    (fun names (_, name)-> unloc name :: names)
                    state.fields fname;
                anon      = state.anon || Option.is_none fname; })
            E.state0 fields in

        let get_target_field_type = function
          | A.Tcontainer (Tasset an, Aggregate) -> begin
              let asset = Env.Asset.get env (unloc an) in
              A.Tlist asset.as_pkty
            end
          | t -> t
        in

        if infos.E.hasupdate then
          Env.emit_error env (loc tope, OpInRecordLiteral);

        if infos.E.anon && not (List.is_empty (infos.E.fields)) then begin
          Env.emit_error env (loc tope, MixedAnonInAssetOrRecordLiteral);
          bailout ()
        end;

        if infos.E.anon || List.is_empty fields then
          let dfields =
            match ety with
            | Some (A.Tasset asset) ->
              let asset = Env.Asset.get env (unloc asset) in
              List.pmap
                (fun fd -> if fd.fd_ghost then None else Some fd.fd_type)
                asset.as_fields

            | Some (A.Trecord record) ->
              let record = Env.Record.get env (unloc record) in
              List.map (fun fd -> fd.rfd_type) record.rd_fields

            | Some (A.Tevent record) ->
              let event = Env.Event.get env (unloc record) in
              List.map (fun fd -> fd.rfd_type) event.rd_fields

            | _ ->
              Env.emit_error env (loc tope, CannotInferAnonAssetOrRecord);
              bailout () in

          let ne = List.length fields in
          let ng = List.length dfields in

          if ne <> ng then begin
            Env.emit_error env (loc tope, InvalidFieldsCountInAssetOrRecordLiteral);
            bailout ()
          end;

          let fields =
            List.map2 (fun (_, fe) ty ->
                for_xexpr env ~ety:(get_target_field_type ty) fe
              ) fields dfields;
          in mk_sp ety (A.Precord fields)

        else begin
          let fmap =
            List.fold_left (fun fmap (fname, e) ->
                let fname = unloc (snd (Option.get fname)) in

                Mid.update fname (function
                    | None when Option.is_some (Env.Asset.byfield env fname) -> begin
                        let asset, fd = Option.get (Env.Asset.byfield env fname) in
                        if fd.fd_ghost then
                          Env.emit_error env (loc tope, CannotInitShadowField);
                        Some ((Some (`Asset (unloc asset.as_name), fd.fd_type), [e]))
                      end

                    | None when Option.is_some (Env.Record.byfield env fname) -> begin
                        let record, fd = Option.get (Env.Record.byfield env fname) in
                        Some ((Some (`Record (unloc record.rd_name), fd.rfd_type), [e]))
                      end

                    | None when Option.is_some (Env.Event.byfield env fname) -> begin
                        let record, fd = Option.get (Env.Event.byfield env fname) in
                        Some ((Some (`Event (unloc record.rd_name), fd.rfd_type), [e]))
                      end

                    | None ->
                      Env.emit_error env (loc tope, UnknownFieldName fname);
                      Some (None, [e])

                    | Some (src, es) ->
                      if List.length es = 1 then begin
                        let err = DuplicatedFieldInAssetOrRecordLiteral fname in
                        Env.emit_error env (loc tope, err)
                      end; Some (src, e :: es)) fmap
              ) Mid.empty fields
          in

          let sources =
            List.pmap
              (fun (_, (src, _)) -> Option.map fst src)
              (Mid.bindings fmap) in
          let sources = List.undup (fun x -> x) sources in

          let fields =
            fmap |> Mid.map (fun (src, es) ->
                let ety = Option.map (snd %> get_target_field_type) src in
                es |> List.map (fun e -> for_xexpr env ?ety e)) in

          let record =
            match sources with
            | [] ->
              bailout ()

            | _ :: _ :: _ ->
              let err =
                let for1 = function `Record x | `Event x | `Asset x -> x in
                MixedFieldNamesInAssetOrRecordLiteral (List.map for1 sources) in
              Env.emit_error env (loc tope, err); bailout ()

            | [src] ->
              let sfields, rty =
                match src with
                | `Asset aname ->
                  let asset   = Env.Asset.get env aname in
                  let sfields =
                    List.map
                      (fun fd -> fd.fd_name, fd.fd_type, fd.fd_dfl)
                      asset.as_fields
                  in (sfields, A.Tasset asset.as_name)

                | `Record rname ->
                  let record = Env.Record.get env rname in
                  let sfields =
                    List.map
                      (fun fd -> fd.rfd_name, fd.rfd_type, fd.rfd_dfl)
                      record.rd_fields
                  in (sfields, A.Trecord record.rd_name)

                | `Event rname ->
                  let record = Env.Event.get env rname in
                  let sfields =
                    List.map
                      (fun fd -> fd.rfd_name, fd.rfd_type, fd.rfd_dfl)
                      record.rd_fields
                  in (sfields, A.Tevent record.rd_name)
              in

              let fields =
                List.map (fun ({ pldesc = fd_name }, fd_type, fd_dfl) ->
                    match Mid.find_opt fd_name fields with
                    | None -> begin
                        match fd_dfl with
                        | None ->
                          let err = MissingFieldInAssetOrRecordLiteral fd_name in
                          Env.emit_error env (loc tope, err); dummy (Some fd_type)
                        | Some dfl -> dfl
                      end
                    | Some thisf ->
                      List.hd (List.rev thisf)
                  ) sfields
              in mk_sp (Some rty) (A.Precord fields)

          in record
        end
      end

    | Erecupdate (e, upd) -> begin
        let e = for_xexpr env e in

        let fields, isasset =
          match e.A.type_ with
          | Some Trecord { pldesc = rname } -> begin
              let recd = Env.Record.get env rname in
              let fields = List.map
                  (fun fd -> unloc fd.rfd_name, fd.rfd_type) recd.rd_fields in
              (fields, None)
            end

          | Some Tevent { pldesc = rname } -> begin
              let recd = Env.Event.get env rname in
              let fields = List.map
                  (fun fd -> unloc fd.rfd_name, fd.rfd_type) recd.rd_fields in
              (fields, None)
            end

          | Some Tasset { pldesc = aname } -> begin
              let asset = Env.Asset.get env aname in
              let fields = List.map
                  (fun fd -> unloc fd.fd_name, fd.fd_type) asset.as_fields in
              (fields, Some (List.map unloc asset.as_pk))
            end

          | _ ->
            if Option.is_some e.A.type_ then
              Env.emit_error env (loc tope, RecordUpdateOnNonRecordOrAsset);
            List.iter (fun (_, e) -> ignore (for_xexpr env e : A.pterm)) upd;
            bailout () in

        if Option.is_some isasset && not (is_form_kind mode.em_kind) then begin
          Env.emit_error env (loc tope, AssetUpdateInNonFormula);
          List.iter (fun (_, e) -> ignore (for_xexpr env e : A.pterm)) upd;
          bailout ()
        end;

        let upd =
          let seen = ref Sstr.empty in

          let for1 (x, ue) =
            if Sstr.mem (unloc x) !seen then
              Env.emit_error env (loc x, RecordUpdateDuplicatedFieldName (unloc x));

            if Option.get_dfl false (Option.map (List.mem (unloc x)) isasset) then
              Env.emit_error env (loc x, RecordUpdateOnPKey);

            let aout =
              match List.assoc_opt (unloc x) fields with
              | None ->
                Env.emit_error env (loc x, RecordUpdateWithInvalidFieldName);
                ignore (for_xexpr env ue : A.pterm);
                None
              | Some ety ->
                let ue = for_xexpr env ~ety ue in
                if Sstr.mem (unloc x) !seen then None else Some (x, ue) in

            seen := Sstr.add (unloc x) !seen; aout

          in List.pmap for1 upd in

        mk_sp e.A.type_ (A.Precupdate (e, upd))
      end

    | Etuple es -> begin
        let etys =
          match Option.bind Type.as_tuple ety with
          | Some etys when List.length etys = List.length es ->
            List.map Option.some etys
          | _ ->
            List.make (fun _ -> None) (List.length es) in

        let es = List.map2 (fun ety e -> for_xexpr env ?ety e) etys es in
        let ty = Option.get_all (List.map (fun x -> x.A.type_) es) in
        let ty = Option.map (fun x -> A.Ttuple x) ty in

        mk_sp ty (A.Ptuple es)
      end

    | Esqapp (e, pk) -> begin
        let ee = for_xexpr env e in
        match ee.type_ with
        | Some (A.Ttuple lt) -> begin
            let pk = for_xexpr ?ety:(Some A.vtnat) env pk in
            let idx : Core.big_int =
              match pk.node with
              | A.Plit ({node = A.BVnat idx}) -> idx
              | _ -> Env.emit_error env (pk.loc, InvalidExprressionForTupleAccess); Big_int.zero_big_int
            in
            let i =
              if      Big_int.lt_big_int idx Big_int.zero_big_int
                   || Big_int.ge_big_int idx (Big_int.big_int_of_int (List.length lt))
              then (Env.emit_error env (pk.loc, IndexOutOfBoundForTuple); 0)
              else (Big_int.int_of_big_int idx)
            in
            mk_sp (Some (List.nth lt i)) (A.Ptupleaccess (ee, idx))
          end
        | Some (A.Tmap     (kt, vt))
        | Some (A.Tbig_map (kt, vt))
        | Some (A.Titerable_big_map (kt, vt)) -> begin
            let pk = for_xexpr ?ety:(Some kt) env pk in
            let rty = Some (A.Toption vt) in
            mk_sp
              rty
              (A.Pcall (None, A.Cconst A.Cmgetopt, [], [A.AExpr ee; A.AExpr pk]))
          end
        | _ -> begin
            let e, asset = for_asset_collection_expr mode env (`Parsed e) in
            let pkty = asset |> Option.map (fun (asset, _) -> asset.as_pkty) in
            let pk = for_xexpr ?ety:pkty env pk in

            let aoutty = Option.map (fun (asset, _) ->
                let a =  A.Tasset asset.as_name in a, A.Toption (A.Tcontainer(a, AssetValue))) asset in
            let aoutty = aoutty |> Option.map (fun (aoutty, aouttyv) ->
                match mode.em_kind with
                | `Expr    _ -> aouttyv
                | `Formula _ -> A.Toption aoutty)in

            asset |> Option.iter (
              fun x ->
                x
                |> snd
                |> (fun c -> match c with | A.Collection -> () | _ -> Env.emit_error env (loc tope, InvalidAssetGetContainer(c)))
            );

            mk_sp
              aoutty
              (A.Pcall (Some e, A.Cconst A.Cgetopt, [], [A.AExpr pk]))
          end
      end

    | Edot (pe, x) -> begin
        let rec aux (e : A.pterm) =
          match e with
          | {type_ = None} ->
            bailout ()

          | {type_ = Some (A.Tasset asset) } -> begin
              let asset = Env.Asset.get env (unloc asset) in

              match get_field (unloc x) asset with
              | None ->
                let err = UnknownField (unloc asset.as_name, unloc x) in
                Env.emit_error env (loc x, err); bailout ()

              | Some { fd_type = fty; fd_ghost = ghost } ->
                if ghost && not (is_form_kind mode.em_kind) then
                  Env.emit_error env (loc x, InvalidShadowFieldAccess);
                mk_sp (Some fty) (A.Pdot (e, x))
            end

          | {type_ = Some (A.Trecord record)} -> begin
              let record = Env.Record.get env (unloc record) in

              match get_rfield (unloc x) record with
              | None ->
                let err = UnknownField (unloc record.rd_name, unloc x) in
                Env.emit_error env (loc x, err); bailout ()

              | Some { rfd_type = fty } ->
                mk_sp (Some fty) (A.Pdot (e, x))
            end

          | {type_ = Some (A.Toption (A.Tcontainer (A.Tasset asset, AssetValue))); node = A.Pcall (Some e, A.Cconst A.Cgetopt, [], [A.AExpr pk])} -> begin
              mk_sp
                (Some (A.Tcontainer (A.Tasset asset, AssetValue)))
                (A.Pcall (Some e, A.Cconst A.Cget, [], [A.AExpr pk]))
              |> aux
            end

          | {type_ = Some (A.Tcontainer (A.Tasset asset, AssetValue))} -> begin
              (* TODO: reject if the field is or contains pk *)
              let asset = Env.Asset.get env (unloc asset) in

              match get_field (unloc x) asset with
              | None ->
                let err = UnknownField (unloc asset.as_name, unloc x) in
                Env.emit_error env (loc x, err); bailout ()

              | Some { fd_type = fty; fd_ghost = ghost } ->
                if ghost && not (is_form_kind mode.em_kind) then
                  Env.emit_error env (loc x, InvalidShadowFieldAccess);
                mk_sp (Some fty) (A.Pdot (e, x))
            end

          | {type_ = Some ty} ->
            Env.emit_error env (loc pe, AssetOrRecordExpected ty);
            bailout ()
        in
        let e = for_xexpr env pe in
        aux e
      end

    | Equestiondot (pe, x) -> begin
        let rec aux (e : A.pterm) =
          match e with
          | {type_ = None} ->
            bailout ()

          | {type_ = Some (A.Tasset asset)} -> begin
              let asset = Env.Asset.get env (unloc asset) in

              match get_field (unloc x) asset with
              | None ->
                let err = UnknownField (unloc asset.as_name, unloc x) in
                Env.emit_error env (loc x, err); bailout ()

              | Some { fd_type = fty; fd_ghost = ghost } ->
                if ghost && not (is_form_kind mode.em_kind) then
                  Env.emit_error env (loc x, InvalidShadowFieldAccess);
                mk_sp (Some (A.Toption fty)) (A.Pquestiondot (e, x))
            end

          | {type_ = Some (A.Toption (A.Tcontainer (A.Tasset asset, AssetValue))); node = A.Pcall (Some e, A.Cconst A.Cgetopt, [], [A.AExpr pk])} -> begin
              mk_sp
                (Some (A.Tcontainer (A.Tasset asset, AssetValue)))
                (A.Pcall (Some e, A.Cconst A.Cget, [], [A.AExpr pk]))
              |> aux
            end

          | {type_ = Some (A.Tcontainer (A.Tasset asset, AssetValue))} -> begin
              (* TODO: reject if the field is or contains pk *)
              let asset = Env.Asset.get env (unloc asset) in

              match get_field (unloc x) asset with
              | None ->
                let err = UnknownField (unloc asset.as_name, unloc x) in
                Env.emit_error env (loc x, err); bailout ()

              | Some { fd_type = fty; fd_ghost = ghost } ->
                if ghost && not (is_form_kind mode.em_kind) then
                  Env.emit_error env (loc x, InvalidShadowFieldAccess);
                mk_sp (Some (A.Toption fty)) (A.Pquestiondot (e, x))
            end

          | {type_ = Some ty} ->
            Env.emit_error env (loc pe, AssetOrRecordExpected ty);
            bailout ()
        in
        let e = for_xexpr env pe in
        aux e
      end

    | Eternary (c, a, b) -> begin
        let c = for_xexpr env c in
        let env_a = match c with
          | { node = A.Pcall (Some _, A.Cconst (A.Cget), [], [AExpr _]); type_ = Some (A.Tcontainer (Tasset an, AssetValue))} -> Some (Env.Context.push env (unloc an))
          | { type_ = Some (A.Tbuiltin VTbool)} -> Some env
          | { type_ = Some (A.Toption xty)} -> Some (Env.Local.push env (dumloc "the", xty))
          | _ -> None
        in
        let a = for_xexpr (match env_a with | Some env -> env | _ -> env) a in
        let b = for_xexpr env b ?ety:a.type_ in
        let ty, es = join_expr ?autoview env ety [a; b] in
        let a, b = Option.get (List.as_seq2 es) in
        mk_sp ty (A.Pternary (c, a, b))
      end

    | Emulticomp (e, l) ->
      let e = for_xexpr env e in
      let l = List.map (snd_map (for_xexpr env)) l in

      let _, aout =
        List.fold_left_map (fun e ({ pldesc = op }, e') ->
            match e.A.type_, e'.A.type_ with
            | Some ty, Some ty' -> begin
                let aout =
                  Option.map (fun sig_ ->
                      let e, e' =
                        Option.get
                          (List.as_seq2
                             (List.map2
                                (fun ty e -> cast_expr env (Some ty) e)
                                sig_.osl_sig [e; e'])) in
                      let term = A.Pcomp (tt_cmp_operator op, e, e') in
                      mk_sp (Some sig_.osl_ret) term
                    ) (select_operator env (loc tope) (PT.Cmp op, [ty; ty'])
                         ~formula:(is_form_kind mode.em_kind))
                in (e', aout)
              end

            | _, _ ->
              e', None)
          e l in

      begin match List.pmap (fun x -> x) aout with
        | [] ->
          let lit = A.mk_sp ~type_:A.vtbool ~loc:(loc tope) (A.BVbool true) in
          mk_sp (Some A.vtbool) (A.Plit lit)

        | e :: es ->
          List.fold_left (fun e e' ->
              (mk_sp (Some A.vtbool) (A.Plogical (tt_logical_operator And, e, e'))))
            e es
      end

    | Eapp (Foperator { pldesc = op }, args) -> begin
        let args = List.map (for_xexpr env) args in

        if List.exists (fun arg -> Option.is_none arg.A.type_) args then
          bailout ();

        let aty = List.map (fun a -> Option.get a.A.type_) args in
        let sig_ =
          Option.get_fdfl
            (fun () -> bailout ())
            (select_operator env (loc tope) (op, aty) ~formula:(is_form_kind mode.em_kind)) in

        let args =
          List.map2
            (fun ty e -> cast_expr ~autoview:false env (Some ty) e)
            sig_.osl_sig args in

        let aout =
          match op with
          | Logical op ->
            let a1, a2 = Option.get (List.as_seq2 args) in
            A.Plogical (tt_logical_operator op, a1, a2)

          | Unary op -> begin
              let a1 = Option.get (List.as_seq1 args) in

              match
                match op with
                | PT.Not    -> `Not
                | PT.Uminus -> `UArith (A.Uminus)
              with
              | `Not ->
                A.Pnot a1

              | `UArith op ->
                A.Puarith (op, a1)
            end

          | Arith op ->
            let a1, a2 = Option.get (List.as_seq2 args) in
            A.Parith (tt_arith_operator op, a1, a2)

          | Cmp op ->
            let a1, a2 = Option.get (List.as_seq2 args) in
            A.Pcomp (tt_cmp_operator op, a1, a2)

        in mk_sp (Some (sig_.osl_ret)) aout
      end

    | Eapp (Fident f, args) when Env.Predicate.exists env (unloc f) ->
      if not (is_form_kind mode.em_kind) then begin
        Env.emit_error env (loc tope, PredicateCallInExpr);
        bailout ()
      end;

      let pred = Env.Predicate.get env (unloc f) in
      let args = match args with [{ pldesc = Etuple args }] -> args | _ -> args in

      let tyargs =
        if List.length args <> List.length pred.pr_args then begin
          let na = List.length args and ne = List.length pred.pr_args in
          Env.emit_error env (loc tope, InvalidNumberOfArguments (na, ne));
          List.make (fun _ -> None) ne
        end else List.map (fun (_, ty) -> Some ty) pred.pr_args in

      let args = List.map2 (fun ety e -> for_xexpr env ?ety e) tyargs args in
      let args = List.map  (fun x -> A.AExpr x) args in

      mk_sp (Some A.vtbool) (A.Pcall (None, A.Cid f, [], args))

    | Eapp (Fident {pldesc = "create_contract"}, [path; okh; amount; arg_storage]) -> begin
        let okh = for_xexpr ~ety:(A.Toption A.vtkeyhash) env okh in
        let amount = for_xexpr ~ety:(A.vtcurrency) env amount in

        let path =
          match path with
          | {pldesc = PT.Eliteral(Lstring v) } -> mkloc (loc path) v
          | _ -> (Env.emit_error env (loc path, StringLiteralExpected) ; bailout ())
        in

        let content =
          match Micheline.parse (unloc path) with
          | Some v -> v
          | None -> (Env.emit_error env (loc path, FileNotFound (unloc path)); bailout ())
        in

        let storage_type =
          match Micheline.get_storage_type content with
          | Some v -> v
          | None -> (Env.emit_error env (loc path, InvalidTzFile) ; bailout ())
        in

        let arg_storage = for_xexpr ~ety:storage_type env arg_storage in

        mk_sp (Some (A.Ttuple [A.Toperation; A.vtaddress])) (A.Pcreatecontract ({ms_content = content }, okh, amount, arg_storage))
      end

    | Eapp (Fident f, args) when Env.Function.exists env (unloc f) ->
      let fun_ = Env.Function.get env (unloc f) in
      let args = match args with [{ pldesc = Etuple args }] -> args | _ -> args in

      let tyargs =
        if List.length args <> List.length fun_.fs_args then begin
          let na = List.length args and ne = List.length fun_.fs_args in
          Env.emit_error env (loc tope, InvalidNumberOfArguments (ne, na));
          List.make (fun _ -> None) na
        end else List.map (fun (_, ty) -> Some ty) fun_.fs_args in

      let args = List.map2 (fun ety e -> for_xexpr env ?ety e) tyargs args in
      let args = List.map  (fun x -> A.AExpr x) args in

      mk_sp (Some fun_.fs_retty) (A.Pcall (None, A.Cid f, [], args))

    | Eapp (Fident f, args) when Option.is_some (Env.State.byctor env (unloc f)) ->
      let decl = Option.get (Env.State.byctor env (unloc f)) in
      let _, cty, _ = Option.get (get_ctor (unloc f) decl.sd_ctors) in
      let args = match args with [{ pldesc = Etuple args }] -> args | _ -> args in

      let tyargs =
        if List.length args <> List.length cty then begin
          let na = List.length args and ne = List.length cty in
          Env.emit_error env (loc tope, InvalidNumberOfArguments (ne, na));
          List.make (fun _ -> None) na
        end else List.map Option.some cty in

      let args = List.map2 (fun ety e -> for_xexpr env ?ety e) tyargs args in
      let args = List.map  (fun x -> A.AExpr x) args in

      let typ = A.Tenum decl.sd_name in
      mk_sp (Some typ) (A.Pcall (None, A.Cid f, [], args))

    | Eapp (Fident f, args) -> begin
        let args = List.map (for_xexpr env) args in

        if List.exists (fun arg -> Option.is_none arg.A.type_) args then
          bailout ();

        let aty = List.map (fun a -> Option.get a.A.type_) args in

        match unloc f with
        | "sapling_empty_state" -> begin
            match aty, args with
            | [A.Tbuiltin VTnat], [ { node = A.Plit {node = A.BVnat n; _} } as arg ] ->
              if (Big_int.lt_big_int n Big_int.zero_big_int) || (Big_int.ge_big_int n (Big_int.big_int_of_int 65536))
              then (Env.emit_error env (loc tope, InvalidValueForMemoSize); bailout ());
              let i = Big_int.int_of_big_int n in
              mk_sp (Some (A.Tsapling_state i)) (A.Pcall (None, A.Cconst A.Csapling_empty_state, [], [A.AExpr arg]))
            | [A.Tbuiltin VTnat], _ ->
              Env.emit_error env (loc tope, InvalidSaplingEmptyStateArg);
              bailout ()
            | _ ->
              Env.emit_error env (loc tope, NoMatchingFunction (unloc f, aty));
              bailout ()
          end

        | "sapling_verify_update" -> begin
            match aty, args with
            | [A.Tsapling_transaction n1; A.Tsapling_state n2], [arg1; arg2] ->
              if n1 <> n2
              then (Env.emit_error env (loc tope, DifferentMemoSizeForSaplingVerifyUpdate(n1, n2)); bailout ());
              mk_sp (Some (A.Toption (A.Ttuple [A.vtbytes; A.vtint; A.Tsapling_state n1]))) (A.Pcall (None, A.Cconst A.Csapling_verify_update, [], [A.AExpr arg1; A.AExpr arg2]))
            | _ ->
              Env.emit_error env (loc tope, NoMatchingFunction (unloc f, aty));
              bailout ()
          end
        | _ -> begin

            let cd = List.pmap (select_mop mode.em_kind (unloc f) aty args) allops in
            let cd = List.sort (fun (i, _) (j, _) -> compare i j) cd in
            let cd =
              let i0 = Option.get_dfl (-1) (Option.map fst (List.ohead cd)) in
              List.map snd (List.filter (fun (i, _) -> i = i0) cd) in

            match cd with
            | [] ->
              Env.emit_error env (loc tope, NoMatchingFunction (unloc f, aty));
              bailout ()
            | _::_::_ ->
              Env.emit_error env
                (loc tope, MultipleMatchingFunction (unloc f, aty, List.map proj3_3 cd));
              bailout ()
            | [cname, _, (_, rty)] ->
              let args = List.map (fun x -> A.AExpr x) args in
              mk_sp (Some rty) (A.Pcall (None, A.Cconst cname, [], args))
          end
      end

    | Eappt (Foperator _, _, _) -> assert false

    | Eappt (Fident id, ts, args) -> begin
        match unloc id, ts, args with
        | "address_to_contract", [t], [a] -> begin
            let ety = for_type env t in
            let ty = match ety with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForAddressToContract); bailout() in
            let a = for_xexpr env ~ety:(A.vtaddress) a in
            mk_sp (Some (A.Toption (A.Tcontract ty))) (A.Pcall (None, A.Cconst Caddresstocontract, [ty], [AExpr a]))
          end

        | "make_set", [t], [a] -> begin
            let ety = for_type env t in
            let ty = match ety with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout() in
            for_xexpr env ~ety:(A.Tset ty) a
          end
        | "make_list", [t], [a] -> begin
            let ety = for_type env t in
            let ty = match ety with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout() in
            for_xexpr env ~ety:(A.Tlist ty) a
          end
        | "make_map", [kt; vt], [a] -> begin
            let ekty = for_type env kt in
            let kty = match ekty with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout() in
            let evty = for_type env vt in
            let vty = match evty with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout() in
            for_xexpr env ~ety:(A.Tmap (kty, vty)) a
          end
        | "make_big_map", [kt; vt], [a] -> begin
            let ekty = for_type env kt in
            let kty = match ekty with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout() in
            let evty = for_type env vt in
            let vty = match evty with | Some ty -> ty | None -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout() in
            for_xexpr env ~ety:(A.Tbig_map (kty, vty)) a
          end
        | "make_asset", [ta], [k; v] -> begin
            let eta = for_type env ta in
            let asset =
              match eta with
              | Some (A.Tasset an) -> Env.Asset.get env (unloc an)
              | _ -> Env.emit_error env (loc tope, InvalidTypeForMake); bailout()
            in
            let vk = for_xexpr ~ety:asset.as_pkty env k in
            let vv = for_xexpr env v in
            mk_sp eta (A.Pcall (None, A.Cconst CmakeAsset, [Option.get eta], [AExpr vk; AExpr vv]))
          end
        | _ -> Env.emit_error env (loc tope, NoMatchingFunction (unloc id, [])); bailout ()
      end

    | Emethod (the, m, args) -> begin
        match unloc the, args with
        | Eapp (Fident cn, [a]), [arg] when Option.is_some (Env.Import.lookup env (unloc cn)) -> begin
            let view_id = unloc m in
            let importdecl = Env.Import.get env (unloc cn) in
            let a = for_xexpr ~ety:A.vtaddress env a in

            let at, rt =
              match List.assoc_opt view_id importdecl.id_views with
              | None ->
                Env.emit_error env (loc m, UnknownView view_id);
                bailout ()
              | Some (at, rt) -> (at, rt) in

            let arg = for_xexpr ~ety:at env arg in

            mk_sp (Some rt) (A.Pcall (None, A.Cconst CimportCallView, [rt], [AIdent m; AExpr a; AExpr arg]))
          end
        | _ -> begin
            let type_of_mthtype asset amap = function
              | `T typ     -> Some typ
              | `The       -> Some (A.Tasset asset.as_name)
              | `Asset     -> Some (A.Tasset asset.as_name)
              | `Coll      -> Some (A.Tcontainer (A.Tasset asset.as_name, A.Collection))
              | `SubColl   -> Some (A.Tcontainer (A.Tasset asset.as_name, A.AssetView))
              | `Container -> Some (A.Tcontainer (A.Tasset asset.as_name, A.AssetContainer))
              | `OptVal    -> Some (A.Toption (A.Tcontainer (A.Tasset asset.as_name, A.AssetValue)))
              | `Ref i     -> Mint.find_opt i amap
              | `Pk        -> Some (asset.as_pkty)
              | `OPk       -> Some (A.Toption asset.as_pkty)
              | `PkOrAsset -> begin
                  match mode.em_kind with
                  | `Formula _ -> Some (A.Tasset asset.as_name)
                  | _ ->  Some (asset.as_pkty)
                end
              | _        -> assert false in

            (* match unloc pthe with
               | PT.Eapp ((Fident import_id), [addr]) when (Option.is_some (Env.Import.lookup env (unloc import_id))) -> begin
                let importdecl = Env.Import.get env (unloc import_id) in
                let a  = for_expr kind env ~ety:A.vtaddress addr in

                let it, rt =
                  match List.assoc_opt (unloc m) import.id_views with
                  | None ->
                    Env.emit_error env (loc en, UnknownEntry (unloc en));
                    bailout ()
                  | Some v -> v in

                a
               end *)

            let the = for_xexpr env the in

            match the.A.type_ with
            | None -> bailout () | Some ty ->

              match Type.as_asset_collection ty with
              | Some _ -> begin
                  let infos = for_gen_method_call mode env (loc tope) (`Typed the, m, args) in
                  let the, (asset, c), method_, args, amap = Option.get_fdfl bailout infos in
                  let rty = Option.bind (type_of_mthtype asset amap) (snd method_.mth_sig) in


                  if Option.is_none rty then
                    Env.emit_error env (loc tope, VoidMethodInExpr);

                  begin match method_.mth_place, mode.em_kind with
                    | `OnlyExec, `Formula _ ->
                      Env.emit_error env (loc tope, InvalidMethodInFormula)
                    | `OnlyFormula, (`Expr _) ->
                      Env.emit_error env (loc tope, InvalidMethodInExec)
                    | _, _ ->
                      ()
                  end;

                  begin match method_.mth_purity, mode.em_kind with
                    | `Effect _, `Formula _ ->
                      Env.emit_error env (loc tope, UnpureInFormula)
                    | `Effect allowed, _ when not (List.mem c allowed) ->
                      Env.emit_error env (loc tope, InvalidEffectForCtn (c, allowed))
                    | _, _ ->
                      ()
                  end;

                  begin match method_.mth_map_type, ty with
                    | `Standard, Tcontainer (Tasset _, Collection) when (match asset.as_bm with | A.MKBigMap -> true | _ -> false) && not (is_form_kind mode.em_kind) ->
                      Env.emit_error env (loc tope, InvalidMethodWithBigMap (unloc m))
                    | _ -> ()
                  end;

                  let rty =
                    match method_.mth_totality, mode.em_kind with
                    | `Partial, `Formula _ ->
                      Option.map (fun x -> A.Toption x) rty
                    | _, _ ->
                      rty in

                  mk_sp rty (A.Pcall (Some the, A.Cconst method_.mth_name, [], args))
                end

              | None ->
                let infos =
                  for_api_call ~mode ~capture ?autoview env (`Typed the, m, args) in
                let the, (name, _, sig_), args = Option.get_fdfl bailout infos in
                let args = List.map (fun x -> A.AExpr x) args in
                mk_sp (Some (snd sig_)) (A.Pcall (None, A.Cconst name, [], A.AExpr the :: args))
          end
      end
    | Eif (c, et, Some ef) ->
      let c      = for_xexpr env ~ety:A.vtbool c in
      let et     = for_xexpr env et in
      let ef     = for_xexpr env ef in
      let ty, es = join_expr ?autoview env ety [et; ef] in
      let et, ef = Option.get (List.as_seq2 es) in
      mk_sp ty (A.Pif (c, et, ef))

    | Eletin (x, ty, e1, e2, oe) ->
      let ty  = Option.bind (for_type env) ty in
      let e   = for_xexpr env ?ety:ty e1 in
      let bty =
        if Option.is_some oe then
          Option.bind (fun bty ->
              match bty with
              | A.Toption bty -> Some bty
              | _ -> Env.emit_error env (loc tope, LetInElseOnNonOption); None
            ) e.A.type_
        else e.A.type_ in

      let env, body =
        let _ : bool = check_and_emit_name_free env x in
        Env.inscope env (fun env ->
            let env =
              Option.fold (fun env bty ->
                  Env.Local.push env (x, bty)) env bty
            in env, for_xexpr env e2) in

      let oe = Option.map (fun oe -> for_xexpr env ?ety:body.A.type_ oe) oe in

      mk_sp body.A.type_ (A.Pletin (x, e, ty, body, oe))

    | Eoption oe -> begin
        match oe with
        | ONone None ->
          let ty = Option.bind Type.as_option ety in

          if Option.is_none ty then
            Env.emit_error env (loc tope, CannotInfer);
          mk_sp (Option.map (fun ty -> A.Toption ty) ty) A.Pnone

        | ONone (Some ty) ->
          let ty = for_type env ty in
          mk_sp (Option.map (fun ty -> A.Toption ty) ty) A.Pnone

        | OSome oe ->
          let oe = for_xexpr env oe in
          mk_sp
            (Option.map (fun ty -> A.Toption ty) oe.A.type_)
            (A.Psome oe)
      end

    | Eor (Oleft (_, t, x)) ->
      let x  = for_xexpr env x in
      let ty = for_type_exn env t in

      mk_sp
        (Some (A.Tor (Option.get_dfl A.vtunit x.type_, ty)))
        (A.Pleft (ty, x))

    | Eor (Oright (t, _, x)) ->
      let x  = for_xexpr env x in
      let ty = for_type_exn env t in
      mk_sp
        (Some (A.Tor (ty, Option.get_dfl A.vtunit x.type_)))
        (A.Pright (ty, x))

    | Elambda (prt, pid, pat, pe) -> begin
        let rt, at =
          match Option.bind Type.as_lambda ety with
          | None -> None, None
          | Some (at, rt) -> Some rt, Some at in

        let rt =
          match Option.bind (for_type env) prt with
          | Some _ as rt -> rt
          | _ -> rt in

        let at =
          match Option.bind (for_type env) pat with
          | Some _ as at -> at
          | _ -> at in

        let capture = {
          cp_global = false;
          cp_local  = `Only (Sid.singleton (unloc pid));
        } in

        let _, e = Env.inscope env (fun env ->
            let env =
              match at with
              | None ->
                Env.emit_error env (loc pid, CannotInfer);
                env
              | Some at ->
                Env.Local.push env (pid, at) in
            env, for_xexpr env ~capture ?ety:rt pe) in

        let oty =
          match rt, at with
          | Some rt, Some at -> Some (A.Tlambda (at, rt))
          | _, _ -> None in

        mk_sp oty
          (A.Plambda (Option.get_dfl A.vtunit at, pid, Option.get_dfl A.vtunit rt, e))
      end

    | Ematchwith (e, bs) -> begin
        match for_gen_matchwith mode capture env (loc tope) e bs with
        | None -> bailout () | Some (kd, ctors, me, (wd, bsm, args), es) ->
          let es = List.map2 (fun e xs ->
              let env = Env.Local.pushn ~kind:`Pattern env xs in
              let capture =
                extend_capture capture (List.map (fun (x, _) -> unloc x) xs)
              in for_xexpr ~capture env e) es args in
          let bty, es = join_expr env ety es in

          let aout = List.pmap (fun (cname, _, _) ->
              let bse  =
                match Mstr.find (unloc cname) bsm, wd with
                | Some i, _ ->
                  Some (List.nth es i, List.map fst (List.nth args i))
                | None, Some _ ->
                  None
                | None, None ->
                  Some (dummy bty, [])
              in bse |> Option.map (fun (bse, args) ->
                  (A.mk_sp (A.Mconst (cname, args)), bse))
            ) ctors in

          let aout =
            Option.fold
              (fun aout extra -> aout @ [A.mk_sp A.Mwild, extra])
              aout (Option.map (List.nth es) wd) in

          let aout =
            match decompile_match_with kd aout with
            | Some (`List ((x, xs, bcons), bnil)) ->
              A.Pmatchlist (me, x, xs, bcons, bnil)

            | Some (`Or ((xl, bl), (xr, br))) ->
              A.Pmatchor (me, xl, bl, xr, br)

            | Some (`Option ((x, bsome), bnone)) ->
              A.Pmatchoption (me, x, bsome, bnone)

            | None -> A.Pmatchwith (me, aout)

          in mk_sp bty aout
      end

    | Efold (pinit, x, pe)-> begin
        let init = for_xexpr env pinit in

        let oty = init.type_ |> Option.bind (fun ty ->
            let oty = Type.as_or ty in
            if Option.is_none oty then
              Env.emit_error env (loc pinit, InvalidFoldInit ty);
            oty) in

        let ety = Option.map (fun (lt, rt) -> A.Tor (lt, rt)) oty in
        let lt, rt = Option.map fst oty, Option.map snd oty in

        let e =
          let env =
            Option.fold
              (fun env lt -> Env.Local.push ~kind:`LoopIndex env (x, lt))
              env lt
          in
          let capture =
            extend_capture capture (List.map (fun (x, _) -> unloc x) [(x, lt)])
          in
          for_xexpr ~capture env ?ety pe in

        let rty =
          match mode.em_kind with
          | `Expr    _ -> rt
          | `Formula _ -> Option.map (fun ty -> A.Toption ty) rt in

        mk_sp rty (A.Pfold (init, x, e))
      end

    | Emap (plst, x, pbody) -> begin
        let lst = for_xexpr env plst in

        let f_as, f_ty =
          match lst.type_ with
          | Some (A.Tlist _)      -> (fun t -> Type.as_list t), (fun ty -> A.Tlist ty)
          | Some (A.Toption _)    -> (fun t -> Type.as_option t), (fun ty -> A.Toption ty)
          | Some (A.Tmap (kt, _)) -> (fun t -> Type.as_map t |> Option.map (fun (x, y) -> A.Ttuple [x; y])), (fun ty -> A.Tmap (kt, ty))
          | _ -> (fun _ -> None), (fun ty -> ty)
        in

        let oty = lst.type_ |> Option.bind (fun ty ->
            let oty = f_as ty in
            if Option.is_none oty then
              Env.emit_error env (loc plst, InvalidTypeForMapOperator ty);
            oty) in

        let body =
          let env =
            Option.fold
              (fun env ty -> Env.Local.push ~kind:`LoopIndex env (x, ty))
              env oty
          in for_xexpr env pbody in

        let rty = Option.map f_ty body.type_ in

        mk_sp rty (A.Pmap (lst, x, body))
      end

    | Equantifier (qt, x, xty, body) -> begin
        if not (is_form_kind mode.em_kind) then begin
          Env.emit_error env (loc tope, BindingInExpr);
          bailout ()
        end else
          match
            match xty with
            | PT.Qcollection xe ->
              let ast, xe = for_asset_collection_expr mode env (`Parsed xe) in
              Option.map (fun (ad, _) -> (Some ast, A.Tasset ad.as_name)) xe
            | PT.Qtype ty ->
              let ty = for_type env ty in
              Option.map (fun ty -> (None, ty)) ty
          with
          | None -> bailout () | Some (ast, xty) ->

            let _, body =
              Env.inscope env (fun env ->
                  let _ : bool = check_and_emit_name_free env x in
                  let env = Env.Local.push env (x, xty) in
                  env, for_formula env body) in

            let qt =
              match qt with
              | PT.Forall -> A.Forall
              | PT.Exists -> A.Exists in

            mk_sp (Some A.vtbool) (A.Pquantifer (qt, x, (ast, xty), body))
      end

    | Eunpack (ty, e) ->
      let ty = for_type env ty in
      let e  = for_xexpr env ~ety:A.vtbytes e in

      Option.iter (fun ty ->
          if not (Type.Michelson.is_packable ty) then
            Env.emit_error env (loc tope, PackUnpackOnNonPrimitive)) ty;

      mk_sp
        (Option.map (fun ty -> A.Toption ty) ty)
        (A.Pcall (None, A.Cconst A.Cunpack, [], [AExpr e]))

    | Enothing
    | Eunit ->
      let lit = A.mk_sp ~type_:A.vtunit ~loc:(loc tope) (A.BVunit) in
      mk_sp (Some A.vtunit) (A.Plit lit)

    | Eself name when is_expr_kind mode.em_kind -> begin
        let decl =
          match Env.Tentry.lookup env (unloc name) with
          | None ->
            Env.emit_error env (loc name, UnknownEntry (unloc name));
            bailout ()
          | Some decl -> decl in

        let rty = Type.create_tuple (List.map snd decl.ad_args) in

        mk_sp (Some (A.Tcontract rty)) (A.Pself name)
      end

    | Eentrypoint (ty, a, b) -> begin
        let ty = for_type_exn env ty in
        let a  = for_xexpr env ~ety:A.vtstring a in
        let b  = for_xexpr env ~ety:A.vtaddress b in

        if not (Type.Michelson.is_type ty) then
          Env.emit_error env (loc tope, InvalidTypeForEntrypoint);

        let id =
          match a.node with
          | A.Plit { node = (BVstring str); _ } -> mkloc a.loc str
          | _ -> (Env.emit_error env (a.loc, StringLiteralExpected); bailout ())
        in

        mk_sp
          (Some (A.Toption (A.Tcontract ty)))
          (A.Pcall (None, A.Cconst CgetEntrypoint, [ty], [AIdent id; AExpr b]))
      end

    | Ecallview (ty, a, b, c) -> begin
        let ty = for_type_exn env ty in
        let a  = for_xexpr env ~ety:A.vtaddress a in
        let b  = for_xexpr env ~ety:A.vtstring b in
        let c  = for_xexpr env c in

        if not (Type.Michelson.is_type ty) then
          Env.emit_error env (loc tope, InvalidTypeForCallview);

        let id =
          match b.node with
          | A.Plit { node = (BVstring str); _ } -> mkloc b.loc str
          | _ -> (Env.emit_error env (b.loc, StringLiteralExpected); bailout ())
        in

        let rt = A.Toption ty in
        mk_sp
          (Some rt)
          (A.Pcall (None, A.Cconst CcallView, [ty], [AIdent id; AExpr a; AExpr c]))
      end
    | Eself      _
    | Evar       _
    | Evaropt    _
    | Efail      _
    | Efailsome  _
    | Eassert    _
    | Elabel     _
    | Eassign    _
    | Eassignopt _
    | Edofailif  _
    | Efor       _
    | Eiter      _
    | Ewhile     _
    | Eif        _
    | Edorequire _
    | Ereturn    _
    | Eseq       _
    | Etransfer  _
    | Eemit      _
    | Eany
    | Einvalid ->
      Env.emit_error env (loc tope, InvalidExpression);
      bailout ()

  in

  try
    cast_expr ?autoview env ety (doit ())

  with E.Bailout -> dummy ety

(* -------------------------------------------------------------------- *)
and cast_expr ?(autoview = false) (env : env) (to_ : A.ptyp option) (e : A.pterm) =
  let to_ =
    if not autoview then to_ else begin
      match e.A.type_, to_ with
      | Some (A.Tcontainer (asset, ctn)), None when ctn <> A.AssetView ->
        Some (A.Tcontainer (asset, A.AssetView))
      | _, _ -> to_
    end
  in

  match to_, e with
  | Some (A.Tlist xty as to_),
    { type_ = Some (A.Tcontainer (A.Tasset asset, A.AssetView) as from_) } ->

    let decl = Env.Asset.get env (unloc asset) in

    if not (Type.equal xty decl.as_pkty) then
      Env.emit_error env (e.loc, IncompatibleTypes (from_, to_));
    A.mk_sp ~loc:e.loc ~type_:to_ (A.Pcast (from_, to_, e))

  | Some to_, { type_ = Some from_ } ->
    if not (Type.compatible ~autoview ~for_eq:false ~from_ ~to_) then
      Env.emit_error env (e.loc, IncompatibleTypes (from_, to_));
    if not (Type.equal from_ to_) then
      A.mk_sp ~loc:e.loc ~type_:to_ (A.Pcast (from_, to_, e))
    else e

  | _, _ ->
    e

(* -------------------------------------------------------------------- *)
and join_expr ?autoview (env : env) (ety : A.ptyp option) (es : A.pterm list) =
  match ety with
  | Some _ ->
    (ety, List.map (cast_expr ?autoview env ety) es)

  | _ -> begin
      match Type.join (List.pmap (fun e -> e.A.type_) es) with
      | None ->
        (None, es)
      | Some _ as ty ->
        (ty, List.map (cast_expr ?autoview env ty) es)
    end

(* -------------------------------------------------------------------- *)
and for_gen_matchwith (mode : emode_t) (capture : capture) (env : env) theloc pe bs =
  let me = for_xexpr mode ~capture env pe in

  let ctors =
    match me.A.type_ with
    | None ->
      None

    | Some (A.Tenum x) ->
      Some (`Enum, (Env.State.get env (unloc x)).sd_ctors)

    | Some (A.Tlist ty) ->
      Some (`List ty, [
          (mkloc Location.dummy (ident_of_pname PCons), [ty; A.Tlist ty], []);
          (mkloc Location.dummy (ident_of_pname PNil ), [], []);
        ])

    | Some (A.Toption ty) ->
      Some (`Option ty, [
          (mkloc Location.dummy (ident_of_pname PSome), [ty], []);
          (mkloc Location.dummy (ident_of_pname PNone), []  , []);
        ])

    | Some (A.Tor (ty1, ty2)) ->
      Some (`Or (ty1, ty2), [
          (mkloc Location.dummy (ident_of_pname PLeft ), [ty1], []);
          (mkloc Location.dummy (ident_of_pname PRight), [ty2], []);
        ])

    | Some _ ->
      Env.emit_error env (loc pe, NotAnEnumType); None in

  match ctors with
  | None ->
    None

  | Some (kd, ctors) ->
    let bsm  = List.map (fun (ct, _, _) -> (unloc ct, None)) ctors in
    let bsm  = Mstr.of_list bsm in

    let module E = struct exception Bailout end in

    let (wd, bsm), args = List.fold_left_mapi (fun bse ((_, bsm0) as bsm) (pts, _) ->
        let (wd, bsm), args =
          List.fold_left_map (fun (wd, bsm) pt ->
              try
                match unloc pt with
                | PT.Pref (pid, args) -> begin
                    let pid = ident_of_pname (unloc pid) in

                    match Mstr.find_opt pid bsm with
                    | None ->
                      Env.emit_error env (loc pt, AlienPattern);
                      raise E.Bailout

                    | Some bd -> begin
                        let _, cargs, _ =
                          List.find (fun (ct, _, _) -> unloc ct = pid) ctors in

                        let ng = List.length args in
                        let ne = List.length cargs in

                        if ng <> ne then begin
                          Env.emit_error env (loc pt, InvalidNumberOfArguments (ne, ng));
                          raise E.Bailout
                        end;
                        if Option.is_some bd || Option.is_some wd then begin
                          Env.emit_error env (loc pt, UselessPattern);
                          raise E.Bailout
                        end;
                        (wd, Mstr.add pid (Some bse) bsm), Some (List.combine args cargs)
                      end
                  end

                | PT.Pwild -> begin
                    match wd with
                    | None when Mstr.exists (fun _ v -> Option.is_none v) bsm ->
                      (Some bse, bsm), Some []

                    | _ ->
                      Env.emit_error env (loc pt, UselessPattern);
                      raise E.Bailout
                  end

              with E.Bailout -> (wd, bsm), None) bsm pts in

        let bsm, args =
          try
            let args =
              match List.pmap (fun x -> x) args with
              | [] -> [] | [xtys] -> xtys | xtys :: rem ->
                let xtymap = List.map (fun (x, ty) -> (unloc x, (loc x, ty))) xtys in
                let xtymap = Mstr.of_list xtymap in

                List.iter (fun xtys' ->
                    let xtymap' = List.map (fun (x, ty) -> (unloc x, (loc x, ty))) xtys' in
                    let xtymap' = Mstr.of_list xtymap' in
                    let vars    = Sstr.of_list (List.map fst (Mstr.bindings xtymap ) @
                                                List.map fst (Mstr.bindings xtymap') ) in

                    Sstr.iter (fun x ->
                        match Mstr.find_opt x xtymap, Mstr.find_opt x xtymap' with
                        | Some (_, ty), Some (lc', ty') ->
                          if not (Type.equal ty ty') then begin
                            Env.emit_error env (lc', IncompatibleTypes (ty, ty'));
                            raise E.Bailout
                          end
                        | None, Some (lc, _)
                        | Some (lc, _), None ->
                          Env.emit_error env (lc, NonHomogeneousPattern x)
                        | None, None -> assert false) vars
                  ) rem; xtys
            in (bsm, args)

          with E.Bailout -> (bsm0, []) in
        (wd, bsm), args

      ) (None, bsm) bs in

    if Option.is_none wd then begin
      let missing = Mstr.bindings bsm in
      let missing = List.filter (fun (_, v) -> Option.is_none v) missing in
      let missing = List.sort String.compare (List.map fst missing) in

      if not (List.is_empty missing) then
        Env.emit_error env (theloc, PartialMatch missing)
    end;

    Some (kd, ctors, me, (wd, bsm, args), (List.map snd bs))

(* -------------------------------------------------------------------- *)
and for_asset_expr mode (env : env) (tope : PT.expr) =
  let ast = for_xexpr mode env tope in
  let typ =
    match Option.map Type.as_asset ast.A.type_ with
    | None ->
      None

    | Some None ->
      Env.emit_error env (loc tope, InvalidAssetExpression);
      None

    | Some (Some asset) ->
      Some (Env.Asset.get env (unloc asset))

  in (ast, typ)

(* -------------------------------------------------------------------- *)
and for_asset_collection_expr mode (env : env) tope =
  let ast =
    match tope with
    | `Typed   ast -> ast
    | `Parsed tope -> for_xexpr mode env tope
  in

  let typ =
    match Option.map Type.as_asset_collection ast.A.type_ with
    | None ->
      None

    | Some None ->
      Env.emit_error env
        (ast.A.loc, InvalidAssetCollectionExpr (Option.get ast.A.type_));
      None

    | Some (Some (asset, c)) ->
      Some (Env.Asset.get env (unloc asset), c)

  in (ast, typ)

(* -------------------------------------------------------------------- *)
and select_mop em name aty (args : A.pterm list) (op : opinfo) =
  let module E = struct exception Reject end in

  try
    if name <> op.op_name then
      raise E.Reject;

    let ety = Option.get_as_list op.op_thety @ op.op_sig in

    if List.length aty <> List.length ety then
      raise E.Reject;

    let filter =
      match op.op_filter with
      | Some f -> f aty args
      | None -> false;
    in

    if filter
    then raise E.Reject;

    let map = ref Mint.empty in

    List.iter2
      (fun ety aty -> Type.unify ~restr:op.op_restr ~ptn:ety ~tg:aty map)
      ety aty;

    let ety = List.map (Type.subst !map) ety in
    let rty = Type.subst !map (
        match op.op_resty with
        | `Self  -> Option.get op.op_thety
        | `Ty ty -> ty) in

    let rty =
      match op.op_partial, em with
      | `Partial, `Formula _ -> A.Toption rty
      | _, _ -> rty in

    let d = Type.sig_distance ~from_:aty ~to_:ety in

    Some (Option.get_exn E.Reject d, (op.op_const, (op.op_resty = `Self), (ety, rty)))

  with E.Reject | Type.UnificationFailure -> None

(* -------------------------------------------------------------------- *)
and for_api_call ~mode ?autoview ?capture env (the, m, args)
  : (A.pterm * (A.const * bool * (A.ptyp list * A.ptyp)) * A.pterm list) option
  =
  let module E = struct exception Bailout end in

  try
    let the =
      match the with
      | `Typed  ast -> ast
      | `Parsed the -> for_xexpr mode env the in

    let args =
      match args with
      | [ { pldesc = PT.Etuple l; _ } ] -> l
      | _ -> args in

    let args = List.map (for_xexpr mode ?autoview ?capture env) args in

    if List.exists (fun arg -> Option.is_none arg.A.type_) args then
      raise E.Bailout;

    let aty = List.map (fun a -> Option.get a.A.type_) args in

    let method_ =
      match the.A.type_ with
      | None ->
        raise E.Bailout

      | Some ty ->
        match
          List.find_map
            (fun op ->
               if Option.is_some op.op_thety then
                 select_mop mode.em_kind (unloc m) (ty :: aty) args op
               else None) allops
        with
        | None ->
          Env.emit_error env (loc m, NoSuchMethod (unloc m));
          raise E.Bailout
        | Some (_, method_) -> method_ in

    Some (the, method_, args)

  with E.Bailout -> None

(* -------------------------------------------------------------------- *)
and for_gen_method_call mode env theloc (the, m, args)
  : (A.pterm * (assetdecl * A.container) * method_ * A.pterm_arg list * A.type_ Mint.t) option
  =
  let module E = struct exception Bailout end in

  if mode.em_pred then
    Env.emit_error env (theloc, MethodCallInPredicate);

  try
    let the, asset = for_asset_collection_expr mode env the in

    let asset, c = Option.get_fdfl (fun () -> raise E.Bailout) asset in
    let method_ =
      match Mid.find_opt (unloc m) methods with
      | None ->
        Env.emit_error env (loc m, NoSuchMethod (unloc m));
        raise E.Bailout
      | Some method_ -> method_
    in

    let ne =
      match fst method_.mth_sig with
      | `Fixed sig_ -> List.length sig_
      | `Multi _    -> List.length args in
    let ng = List.length args in

    if ne <> ng then begin
      Env.emit_error env (theloc, InvalidNumberOfArguments (ne, ng));
      raise E.Bailout
    end;

    let rec doarg arg (aty : mthtyp) =
      match aty with
      | `Pk ->
        A.AExpr (for_xexpr mode env ~ety:asset.as_pkty arg)

      | `The ->
        A.AExpr (for_xexpr mode env ~ety:(Tasset asset.as_name) arg)

      | `ThePkForAggregate -> begin
          match the.type_ with
          | Some (A.Tcontainer(_, Aggregate)) ->  doarg arg `Pk
          | _ -> doarg arg `The
        end

      | (`Pred capture | `RExpr capture) as sub -> begin
          let env     = Env.Context.push env (unloc asset.as_name) in
          let theid   = mkloc (loc arg) Env.Context.the in
          let thety   = A.Tasset asset.as_name in
          let mode    = match sub with `Pred _ -> { mode with em_pred = true; } | _ -> mode in
          let ety     = match sub with `Pred _ -> Some A.vtbool | _ -> None in
          let map     = ref Mid.empty in
          let capture =
            let cp_local =
              if capture then `Yes (Some map) else `Only Sid.empty
            in { cp_global = true; cp_local; } in
          let body    = for_xexpr ~capture mode env ?ety arg in
          let closure =
            List.map
              (fun (x, (loc, xty)) ->
                 let xterm = A.mk_sp ~loc ~type_:xty (A.Pvar (VTnone, Vnone, mkloc loc x)) in
                 (mkloc loc x, xty, xterm))
              (Mid.bindings !map) in

          begin match sub with
            | `Pred  _ -> ()
            | `RExpr _ ->
              body.A.type_ |> Option.iter (fun ty ->
                  if not (Type.is_numeric ty || Type.is_currency ty) then
                    Env.emit_error env (loc arg, NumericExpressionExpected))
          end;

          A.AFun (theid, thety, closure, body)
        end

      | `Ef update ->
        let env     = Env.Context.push env (unloc asset.as_name) in
        A.AEffect (Option.get_dfl [] (for_arg_effect mode env ~update asset arg))

      | `Coll ->
        let ty = A.Tcontainer (Tasset asset.as_name, A.Collection) in
        A.AExpr (for_xexpr ~autoview:true mode env ~ety:ty arg)

      | `SubColl ->
        let ty = A.Tcontainer (Tasset asset.as_name, A.AssetView) in
        A.AExpr (for_xexpr ~autoview:true mode env ~ety:ty arg)

      | `OptVal ->
        let ty = A.Toption (A.Tcontainer (Tasset asset.as_name, A.AssetValue)) in
        A.AExpr (for_xexpr mode env ~ety:ty arg)

      | `T ty ->
        A.AExpr (for_xexpr mode env ~ety:ty arg)

      | `Cmp -> begin
          let asc, field =
            match unloc arg with
            | Eterm ((None, None), f) ->
              (true, Some f)
            | Eapp (Fident { pldesc = ("asc" | "desc") as order },
                    [{pldesc = Eterm ((None, None), f) }]) ->
              (order = "asc", Some f)
            | _ ->
              Env.emit_error env (loc arg, InvalidSortingExpression);
              (true, None) in

          let field = Option.bind (fun f ->
              match get_field (unloc f) asset with
              | None ->
                Env.emit_error env (loc f, UnknownFieldName (unloc f));
                None
              | Some _ -> Some f) field in

          let field = Option.get_fdfl (fun () -> mkloc (loc arg) "<error>") field in
          A.ASorting (asc, field)
        end

      | _ ->
        assert false

    in

    let atyps =
      match fst method_.mth_sig with
      | `Fixed x -> x | `Multi x -> List.make (fun _ -> x) ne in
    let args = List.map2 doarg args atyps in
    let amap =
      let aout = ref Mint.empty in
      List.iteri (fun i arg ->
          match arg with
          | A.AExpr { A.type_ = Some ty } ->
            aout := Mint.add i ty !aout
          | A.AFun (_, _, _, { A.type_ = Some ty }) ->
            aout := Mint.add i ty !aout
          | _ -> ()) args; !aout in

    Some (the, (asset, c), method_, args, amap)

  with E.Bailout -> None

(* -------------------------------------------------------------------- *)
and for_arg_effect
    mode (env : env) ~(update : bool) (asset : assetdecl) (tope : PT.expr)
  =
  match unloc tope with
  | Erecord fields ->
    let do1 map ((x, e) : PT.record_item) =
      match x with
      | None ->
        Env.emit_error env (loc tope, AnonymousFieldInEffect);
        map

      | Some (op, x) -> begin
          match get_field (unloc x) asset with
          | Some { fd_type = fty; fd_ghost = fghost } ->
            let rfty =
              match fty with
              | A.Tcontainer (A.Tasset subasset, A.Aggregate) -> begin
                  let subasset = Env.Asset.get env (unloc subasset) in
                  A.Tlist subasset.as_pkty
                end
              | _ -> fty
            in

            let op  = for_assignment_operator op in
            let e   = for_assign_expr
                ~autoview:false ~asset:true mode env
                (loc x) (op, fty, rfty) e in

            if Mid.mem (unloc x) map then begin
              Env.emit_error env (loc x, DuplicatedFieldInAssetOrRecordLiteral (unloc x));
              map
            end else if List.exists (fun f -> unloc x = unloc f) asset.as_pk then begin
              Env.emit_error env (loc x, UpdateEffectOnPkey);
              map
            end else if mode.em_kind = `Expr `Concrete && fghost then begin
              Env.emit_error env (loc x, InvalidShadowFieldAccess);
              map
            end else
              Mid.add (unloc x) (x, `Assign op, e) map

          | None ->
            Env.emit_error env (loc x, UnknownField (unloc asset.as_name, unloc x));
            map
        end
    in

    let effects = List.fold_left do1 Mid.empty fields in

    if not update then begin
      List.iter (fun field ->
          if List.for_all (fun f -> unloc f <> unloc field.fd_name) asset.as_pk then begin
            match Mid.find_opt (unloc field.fd_name) effects with
            | None ->
              if Option.is_none field.fd_dfl then
                Env.emit_error env
                  (loc tope, MissingFieldInAssetOrRecordLiteral (unloc field.fd_name))

            | Some (x, `Assign op, _) ->
              if op <> A.ValueAssign && Option.is_none field.fd_dfl then
                Env.emit_error env (loc x, UpdateEffectWithoutDefault)
          end
        ) asset.as_fields
    end;

    Some (List.map snd (Mid.bindings effects))

  | _ ->
    Env.emit_error env (loc tope, InvalidExpressionForEffect);
    None

(* -------------------------------------------------------------------- *)
and for_assign_expr ?autoview ?(asset = false) mode env orloc (op, lfty, rfty) e =
  let op =
    match op with
    | ValueAssign -> None
    | PlusAssign  -> Some (PT.Arith   PT.Plus   )
    | MinusAssign -> Some (PT.Arith   PT.Minus  )
    | MultAssign  -> Some (PT.Arith   PT.Mult   )
    | DivAssign   -> Some (PT.Arith   PT.DivRat )
    | AndAssign   -> Some (PT.Logical PT.And    )
    | OrAssign    -> Some (PT.Logical PT.Or     )
  in

  let ety = if Option.is_none op then Some rfty else None in
  let e = for_xexpr ?autoview mode env ?ety e in

  Option.get_dfl e (
    op |> Option.bind (fun op  ->
        e.type_ |> Option.bind (fun ety ->
            select_operator env ~asset orloc (op, [lfty; ety]) ~formula:(is_form_kind mode.em_kind)
            |> Option.map (fun sig_ ->
                cast_expr ?autoview env (Some (List.last sig_.osl_sig)) e))))

(* -------------------------------------------------------------------- *)
and for_formula ?(invariant = false) (env : env) (topf : PT.expr) : A.pterm =
  let e = for_xexpr (form_mode invariant) ~ety:(A.Tbuiltin A.VTbool) env topf in
  Option.iter (fun ety ->
      if ety <> A.vtbool then
        Env.emit_error env (loc topf, FormulaExpected))
    e.type_; e

(* -------------------------------------------------------------------- *)
and for_entry_description (env : env) (sa : PT.security_arg) : A.entry_description =
  match unloc sa with
  | Sident { pldesc = "anyentry" } ->
    A.ADAny

  | Sapp (act, [{ pldesc = PT.Sident asset }]) -> begin
      let mode  = { em_kind = `Formula false; em_pred = false; } in
      let asset = mkloc (loc asset) (PT.Eterm ((None, None), asset)) in
      let asset = for_asset_collection_expr mode env (`Parsed asset) in

      match snd asset with
      | None ->
        A.ADAny

      | Some (decl, _) ->
        A.ADOp (unloc act, decl.as_name)
    end

  | _ ->
    Env.emit_error env (loc sa, InvalidEntryDescription);
    A.ADAny

(* -------------------------------------------------------------------- *)
and for_security_entry (env : env) (sa : PT.security_arg) : A.security_entry =
  match unloc sa with
  | Sident id ->
    begin
      match unloc id with
      | "anyentry" -> Sany
      | _           ->
        let ad = Env.Tentry.lookup env (unloc id) in

        if Option.is_none ad then
          Env.emit_error env (loc id, UnknownEntry (unloc id));

        Sentry [id]
    end

  | Slist sas ->
    A.Sentry (List.flatten (List.map (
        fun x ->
          let a = for_security_entry env x in
          match a with
          | Sentry ids -> ids
          | _ -> assert false) sas))

  | _ ->
    Env.emit_error env (loc sa, InvalidSecurityEntry);
    Sentry []

(* -------------------------------------------------------------------- *)
and for_security_role (env : env) (sa : PT.security_arg) : A.security_role list =
  match unloc sa with
  | Sident id ->
    Option.get_as_list (for_role env id)

  | _ ->
    Env.emit_error env (loc sa, InvalidSecurityRole);
    []

(* -------------------------------------------------------------------- *)
and for_role (env : env) (name : PT.lident) =
  match Env.Var.lookup env (unloc name) with
  | None ->
    Env.emit_error env (loc name, UnknownLocalOrVariable (unloc name));
    None

  | Some nty ->
    if not (Type.compatible ~autoview:false ~for_eq:false ~from_:nty.vr_type ~to_:A.vtaddress) then
      (Env.emit_error env (loc name, NotARole (unloc name)); None)
    else Some name

(* -------------------------------------------------------------------- *)
let for_expr
    (kind : imode_t) ?autoview (env : env) ?(ety : A.type_ option)
    (tope : PT.expr) : A.pterm
  =
  for_xexpr (expr_mode kind) ?autoview env ?ety tope

(* -------------------------------------------------------------------- *)
let for_lbl_expr
    ?ety (kind : imode_t) (env : env) (topf : PT.label_expr) : env * (A.lident option * A.pterm)
  =
  if check_and_emit_name_free env (fst (unloc topf)) then
    let env = Env.Label.push env (fst (unloc topf), `Plain) in
    env, (Some (fst (unloc topf)), for_expr kind env ?ety (snd (unloc topf)))
  else
    env, (None, for_expr kind env ?ety (snd (unloc topf)))

(* -------------------------------------------------------------------- *)
let for_lbls_expr
    kind ?ety (env : env) (topf : PT.label_exprs) : env * (A.lident option * A.pterm) list
  =
  List.fold_left_map (for_lbl_expr ?ety kind) env topf

(* -------------------------------------------------------------------- *)
let for_lbl_bexpr = for_lbl_expr ~ety:(A.Tbuiltin A.VTbool)

(* -------------------------------------------------------------------- *)
let for_rf
    kind ?ety (env : env) (topf : (PT.lident * PT.expr * PT.expr option) list) : env * (A.lident option * A.pterm * A.pterm option) list
  =
  let aux
      ?ety (kind : imode_t) (env : env) (id, e, err : PT.lident * PT.expr * PT.expr option) : env * (A.lident option * A.pterm * A.pterm option)
    =

    let error = Option.map (for_expr kind env) err in
    error
    |> Option.iter (fun (x : A.pterm) ->
        x.type_ |> Option.iter (fun ty ->
            if (not (Type.Michelson.is_type ty))
            then (Env.emit_error env (x.loc, InvalidTypeForFail))));

    if check_and_emit_name_free env id then
      let env = Env.Label.push env (id, `Plain) in
      env, (Some id, for_expr kind env ?ety e, error)
    else
      env, (None, for_expr kind env ?ety e, error)
  in
  List.fold_left_map (aux ?ety kind) env topf

let for_rfs = for_rf ~ety:(A.Tbuiltin A.VTbool)

(* -------------------------------------------------------------------- *)
let for_cf
    kind ?ety (env : env) (topf : (PT.lident * PT.expr * PT.expr option) list) : env * (A.lident option * A.pterm * A.pterm option) list
  =
  let aux
      ?ety (kind : imode_t) (env : env) (id, e, err : PT.lident * PT.expr * PT.expr option) : env * (A.lident option * A.pterm * A.pterm option)
    =
    let error = Option.map (for_expr kind env) err in
    error
    |> Option.iter (fun (x : A.pterm) ->
        x.type_ |> Option.iter (fun ty ->
            if (not (Type.Michelson.is_type ty))
            then (Env.emit_error env (x.loc, InvalidTypeForFail))));

    if check_and_emit_name_free env id then
      let v = for_expr kind env ?ety e in
      let vty = v.A.type_ in
      let ty = if Option.is_some error then begin
          match vty with
          | Some (A.Toption ty) -> Some ty
          | _ -> Env.emit_error env (v.loc, InvalidTypeDeclOpt); vty
        end else vty
      in
      let env = Env.Local.push env (id, Option.get ty) ~kind:`Const in
      env, (Some id, v, error)
    else
      env, (None, for_expr kind env ?ety e, error)
  in
  List.fold_left_map (aux ?ety kind) env topf

let for_cfs = for_cf

(* -------------------------------------------------------------------- *)
let for_lbl_formula (env : env) (topf : PT.label_expr) : env * (A.lident option * A.pterm) =
  if check_and_emit_name_free env (fst (unloc topf)) then
    let env = Env.Label.push env (fst (unloc topf), `Plain) in
    env, (Some (fst (unloc topf)), for_formula env (snd (unloc topf)))
  else
    env, (None, for_formula env (snd (unloc topf)))

(* -------------------------------------------------------------------- *)
let for_xlbls_formula (env : env) (topf : PT.label_exprs) : env * (A.lident option * A.pterm) list =
  List.fold_left_map for_lbl_formula env topf

(* -------------------------------------------------------------------- *)
let for_lbls_formula (env : env) (topf : PT.label_exprs) : env * (A.lident option * A.pterm) list =
  List.fold_left_map for_lbl_formula env topf

(* -------------------------------------------------------------------- *)
let for_arg_decl ?(can_asset = false) (env : env) ((x, ty, _) : PT.lident_typ) =
  let ty = for_type env ty in
  let b  = check_and_emit_name_free env x in

  if not can_asset then begin
    ty |> Option.iter (fun ty ->
        if not (valid_var_or_arg_type ty) then
          Env.emit_error env (loc x, InvalidVarOrArgType))
  end;

  match b, ty with
  | true, Some ty ->
    (Env.Local.push ~kind:`Argument env (x, ty), Some (x, ty))

  | _, _ ->
    (env, None)

(* -------------------------------------------------------------------- *)
let for_args_decl ?can_asset (env : env) (xs : PT.args) =
  List.fold_left_map (for_arg_decl ?can_asset) env xs

(* -------------------------------------------------------------------- *)
let for_lvalue kind (env : env) (e : PT.expr) : (A.lvalue * A.ptyp) option =
  match unloc e with
  | Eterm ((None, None), x) -> begin
      match Env.lookup_entry env (unloc x) with
      | Some (`Local (xty, kind)) -> begin
          match kind with
          | `LoopIndex ->
            Env.emit_error env (loc e, CannotAssignLoopIndex (unloc x));
            None
          | `Argument ->
            Env.emit_error env (loc e, CannotAssignArgument (unloc x));
            None
          | `Pattern ->
            Env.emit_error env (loc e, CannotAssignPatternVariable (unloc x));
            None
          | `Const ->
            Env.emit_error env (loc e, CannotAssignConstVar (unloc x));
            None
          | `Standard ->
            Some (`Var x, xty)
        end

      | Some (`Global vd) ->
        begin match vd.vr_kind, kind, vd.vr_core with
          | `Variable, `Concrete, _
          | `Ghost, `Ghost, _
          | _, _, Some (A.Coperations | A.Cmetadata) -> ()
          | _, _, _ ->
            Env.emit_error env (loc e, ReadOnlyGlobal (unloc x));
        end;
        Some (`Var x, vd.vr_type)

      | _ ->
        Env.emit_error env (loc e, UnknownLocalOrVariable (unloc x));
        None
    end

  | Edot ({pldesc = Esqapp ({pldesc = Eterm ((None, None), asset)}, key)}, x) -> begin
      let asset = Env.Asset.get env (unloc asset) in
      if List.exists (fun f -> unloc f = unloc x) asset.as_pk then begin
        Env.emit_error env (loc x, CannotUpdatePKey);
        None
      end else begin
        match get_field (unloc x) asset with
        | None ->
          let err = UnknownField (unloc asset.as_name, unloc x) in
          Env.emit_error env (loc x, err); None

        | Some { fd_type = fty } ->
          let ktype = asset.as_pkty in
          let key = for_expr ~ety:ktype kind env key in
          Some (`Field (asset.as_name, key, x), fty)
      end
    end

  | Edot (ptg, x) -> begin
      let tg = for_expr kind env ptg in

      match tg.A.type_ with
      | Some (Trecord record) -> begin
          let record = Env.Record.get env (unloc record) in
          let field  = get_rfield (unloc x) record in

          match field with
          | None ->
            Env.emit_error env (loc x, UnknownFieldName (unloc x)); None

          | Some field ->
            Some (`Field (record.rd_name, tg, x), field.rfd_type)
        end

      | Some _ ->
        Env.emit_error env (loc ptg, RecordExpected); None

      | None ->
        None
    end
  | Esqapp (e, pk) -> begin
      let ee = for_expr kind env e in
      match ee.type_ with
      | Some (A.Ttuple lt) -> begin
          let pk = for_expr ?ety:(Some A.vtnat) kind env pk in
          let idx : Core.big_int =
            match pk.node with
            | A.Plit ({node = A.BVnat idx}) -> idx
            | _ -> Env.emit_error env (pk.loc, InvalidExprressionForTupleAccess); Big_int.zero_big_int
          in
          let i =
            if      Big_int.lt_big_int idx Big_int.zero_big_int
                 || Big_int.ge_big_int idx (Big_int.big_int_of_int (List.length lt))
            then (Env.emit_error env (pk.loc, IndexOutOfBoundForTuple); 0)
            else (Big_int.int_of_big_int idx)
          in
          let l = List.length lt in
          let fty = List.nth lt i in
          Some (`Tuple (ee, i, l), fty)
        end
      | _ -> Env.emit_error env (loc e, InvalidTypeForTuple); None
    end
  | _ ->
    Env.emit_error env (loc e, InvalidLValue); None

(* -------------------------------------------------------------------- *)
let rec for_instruction_r
    ~(ret : A.type_ option) (kind : imode_t) (env : env) (i : PT.expr) : env * A.instruction
  =
  let module E = struct exception Failure end in

  let bailout () = raise E.Failure in

  let mki ?label node : A.instruction =
    A.{ node; label; loc = loc i; } in

  let mkseq i1 i2 =
    let asblock = function A.{ node = Iseq is } -> is | _ as i -> [i] in
    match asblock i1 @ asblock i2 with
    | [i] -> i
    | is  -> mki (Iseq is) in

  try
    match unloc i with
    | Emethod (pthe, m, args) -> begin
        let the = for_expr kind env pthe in

        match the.A.type_ with
        | Some ty -> begin
            match Type.as_asset_collection ty with
            | Some _ ->
              let infos = for_gen_method_call (expr_mode kind) env (loc i) (`Typed the, m, args) in
              let the, (_assetdecl , c), method_, args, _ = Option.get_fdfl bailout infos in

              begin match c, method_.mth_purity with
                | ctn, `Effect allowed when not (List.mem ctn allowed) ->
                  Env.emit_error env (loc i, InvalidEffectForCtn (ctn, allowed))
                | _, _ ->
                  () end;

              (* begin match assetdecl.as_bm, method_.mth_map_type with
                 | true, `Standard -> Env.emit_error env (loc i, InvalidMethodWithBigMap (unloc m))
                 | _ -> ()
                 end; *)

              env, mki (A.Icall (Some the, A.Cconst method_.mth_name, args))

            | _ ->
              let infos =
                for_api_call ~mode:(expr_mode kind) env (`Typed the, m, args) in
              let the, (name, se, _), args = Option.get_fdfl bailout infos in
              let args = List.map (fun x -> A.AExpr x) args in
              let aout = mki (A.Icall (None, A.Cconst name, A.AExpr the :: args)) in
              let assign kind =
                mki (A.Iassign (
                    ValueAssign, Option.get the.type_, kind,
                    A.mk_sp ~loc:(loc i) ?type_:the.type_
                      (A.Pcall (None, A.Cconst name, [], A.AExpr the :: args)), None))
              in
              let aout =
                if se then begin
                  match the.node with
                  | Pvar (VTnone, Vnone, x) -> begin
                      (match Env.lookup_entry env (unloc x) with
                       | Some (`Local (_, kind)) -> begin
                           match kind with
                           | `Const -> Env.emit_error env (the.loc, CannotEffectConstVar);
                           | _ -> ()
                         end
                       | _ -> ());
                      assign (`Var x)
                    end
                  | Pconst Cmetadata -> assign (`Var (mkloc the.loc "metadata"))
                  | Pconst Coperations -> assign (`Var (mkloc the.loc "operations"))
                  | Pdot ({node = Pvar (VTnone, Vnone, _); type_ = Some (Trecord rn)} as x, fn) -> assign (`Field (rn, x, fn))
                  | Pdot ({node = Pcall (Some {type_ = Some (Tcontainer ((Tasset _), Collection))}, Cconst Cget, [], [AExpr k]); type_ = Some (Tasset an | Tcontainer (Tasset an, AssetValue))}, fn) -> assign (`Asset (an, k, fn))
                  (* TODO: handle partition, issue: #245 *)
                  | _ -> Env.emit_error env (the.loc, InvalidVariableForMethod); aout
                end else
                  mki (A.Icall (None, A.Cconst name, A.AExpr the :: args)) in

              env, aout
          end

        | None -> bailout ()
      end

    | Eseq (i1, i2) ->
      let env, i1 = for_instruction_r ~ret kind env i1 in
      let env, i2 = for_instruction_r ~ret kind env i2 in
      env, mkseq i1 i2

    | Eassign (op, plv, pe) -> begin
        let lv = for_lvalue kind env plv in
        let x, t  = Option.get_dfl
            (`Var (mkloc (loc plv) "<error>"), A.vtunit)
            (Option.map id lv) in
        let op = for_assignment_operator op in

        let e  =
          match lv with
          | None ->
            for_expr kind env pe

          | Some (_, fty) ->
            for_assign_expr
              (expr_mode kind) env (loc plv) (op, fty, fty) pe
        in

        env, mki (A.Iassign (op, t, x, e, None))
      end

    | Eassignopt (plv, pe, fa) -> begin
        let lv = for_lvalue kind env plv in
        let x, t  = Option.get_dfl
            (`Var (mkloc (loc plv) "<error>"), A.vtunit)
            (Option.map id lv) in
        let op = A.ValueAssign in
        let fa  = for_expr kind env fa in

        fa.type_ |> Option.iter (fun ty ->
            if (not (Type.Michelson.is_packable ty))
            then (Env.emit_error env (fa.loc, InvalidTypeForFail)));

        (* let t = match t with | A.Toption ty -> ty | _ -> Env.emit_error env (fa.loc, InvalidTypeForOptionalAssign); t in *)

        let e  =
          match lv with
          | None ->
            for_expr kind env pe

          | Some (_, fty) ->
            (* let fty = match fty with | A.Toption ty -> ty | _ -> Env.emit_error env (fa.loc, InvalidTypeForOptionalAssign); fty in *)
            let fty = A.Toption fty in
            for_assign_expr
              (expr_mode kind) env (loc plv) (op, fty, fty) pe
        in

        env, mki (A.Iassign (op, t, x, e, Some fa))
      end

    | Etransfer tr ->
      let tr =
        match tr with
        | TTsimple (e, to_) ->
          let e  = for_expr kind env ~ety:A.vtcurrency e in
          A.TTsimple (e, for_expr kind env ~ety:A.vtaddress to_)

        | TTcontract (e, to_, name, ty, arg) -> begin
            let e  = for_expr kind env ~ety:A.vtcurrency e in
            let ty  = for_type_exn env ty in
            let to_ = for_expr ~ety:A.vtaddress kind env to_ in
            let arg = for_expr ~ety:ty kind env arg in

            A.TTcontract (e, to_, name, ty, arg)
          end

        | TTentry (e, name, arg) -> begin
            let x  = for_expr kind env ~ety:A.vtcurrency e in
            let nty =
              match Env.lookup_entry env (unloc name) with
              | Some (`Local (nty, (`Standard | `Const | `Argument | `Pattern))) ->
                nty

              | Some (`Global { vr_type = nty; vr_kind = `Variable }) ->
                nty

              | _ ->
                Env.emit_error env (loc name, UnknownLocalOrVariable (unloc name));
                bailout () in

            if not (Type.is_contract nty) then begin
              Env.emit_error env (loc name, AEntryExpected nty);
              bailout ();
            end;

            let aty = Option.get (Type.as_contract nty) in
            let arg = for_expr kind env ~ety:aty arg in

            let e = A.mk_sp ~type_:nty (A.Pvar (VTnone, Vnone, name)) in

            A.TTentry (x, e, arg)
          end

        | TTentry2 (a, cn, address_arg, en, arg) -> begin
            let a  = for_expr kind env ~ety:A.vtcurrency a in
            let address_arg  = for_expr kind env ~ety:A.vtaddress address_arg in

            let import =
              match Env.Import.lookup env (unloc cn) with
              | None ->
                Env.emit_error env (loc cn, UnknownImport (unloc cn));
                bailout ()
              | Some import -> import in

            let pen = "%" ^ (unloc en) in

            let etyp =
              match List.assoc_opt pen import.id_entrypoints with
              | None ->
                Env.emit_error env (loc en, UnknownEntry (unloc en));
                bailout ()
              | Some typ -> typ in

            (* Format.eprintf "etyp: %a@\n" A.pp_ptyp etyp; *)
            (* Format.eprintf "%a@\n" pp_ptyp etyp; *)
            let arg  = for_expr kind env arg ~ety:etyp in

            TTgen (a, unloc en, unloc cn, etyp, address_arg, arg)
          end

        | TTself (e, name, args) -> begin
            let e  = for_expr kind env ~ety:A.vtcurrency e in
            let entry =
              match Env.Tentry.lookup env (unloc name) with
              | None ->
                Env.emit_error env (loc name, UnknownEntry (unloc name));
                bailout ()
              | Some entry -> entry in

            if List.length entry.ad_args <> List.length args then begin
              let n = List.length entry.ad_args in
              let c = List.length args in
              Env.emit_error env (loc name, InvalidNumberOfArguments (n, c));
              bailout ()
            end;

            let args =
              List.map2
                (fun (id, ety) arg -> id, for_expr ~ety kind env arg)
                entry.ad_args args in

            A.TTself (e, name, args)
          end

        | TToperation e ->
          let e  = for_expr kind env ~ety:A.Toperation e in
          A.TToperation (e)

      in env, mki (Itransfer tr)

    | Eemit (ty, arg) ->

      let idt, ety =
        match for_type env ty with
        | Some ((A.Tevent v) as t) -> (v, t)
        | _ ->
          Env.emit_error env (loc i, InvalidEventType);
          bailout ()

      in
      let e   = for_expr ~ety kind env arg in
      env, mki (A.Iemit (idt, e))

    | Eif (c, bit, bif) ->
      let c        = for_expr kind env ~ety:A.vtbool c in
      let env, cit = for_instruction ~ret kind env bit in
      let cif      = Option.map (for_instruction ~ret kind env) bif in
      let env, cif = Option.get_dfl (env, mki (Iseq [])) cif in
      env, mki (A.Iif (c, cit, cif))

    | Eletin _ ->
      Env.emit_error env (loc i, NoLetInInstruction);
      bailout ()

    | Efor (lbl, x, pe, i) ->
      let e = for_expr kind env pe in

      let kty =
        let is_for_ident k =
          match k, unloc x with
          | `Simple, PT.FIsimple _
          | `Double, PT.FIdouble _ -> true
          | _ -> false
        in
        match e.A.type_ with
        | Some (A.Tcontainer (A.Tasset asset, c)) ->
          let asset = Env.Asset.get env (unloc asset) in
          if (match asset.as_bm with | A.MKBigMap -> true | _ -> false) && (match c with | Collection -> true | _ -> false) then
            Env.emit_error env (loc pe, NonIterableBigMapAsset (unloc asset.as_name));
          if   is_for_ident `Double
          then (Env.emit_error env (loc x, InvalidForIdentSimple); None)
          else Some [asset.as_pkty]

        | Some (A.Tmap (kt, vt))
        | Some (A.Titerable_big_map (kt, vt)) ->
          if   is_for_ident `Simple
          then (Env.emit_error env (loc x, InvalidForIdentMap); None)
          else Some [kt; vt]

        | Some (A.Tset ty | A.Tlist ty) ->
          if   is_for_ident `Double
          then (Env.emit_error env (loc x, InvalidForIdentSimple); None)
          else Some [ty]

        | Some _ ->
          Env.emit_error env (loc pe, NonIterable); None

        | None ->
          None in

      let env, i = Env.inscope env (fun env ->
          let idents = match unloc x with PT.FIsimple i -> [i] | PT.FIdouble (x, y) -> [x; y] in
          let _ : bool = List.for_all (check_and_emit_name_free env) idents in

          let env =
            Option.map_dfl
              (List.fold_left2
                 (fun accu x y ->  Env.Local.push accu ~kind:`LoopIndex (x, y))
                 env idents)
              env kty in

          let env =
            match e.A.type_ with
            | None ->
              env
            | Some lblty ->
              Option.fold (fun env lbl ->
                  if (check_and_emit_name_free env lbl) then
                    Env.Label.push env (lbl, `Loop lblty)
                  else env) env lbl
          in for_instruction ~ret kind env i) in

      let x : A.lident A.for_ident =
        match unloc x with
        | PT.FIsimple  i     -> A.FIsimple i
        | PT.FIdouble (x, y) -> A.FIdouble (x, y)

      in env, mki (A.Ifor (x, e, i)) ?label:(Option.map unloc lbl)

    | Eiter (lbl, x, a, b, i) ->
      let zero_b = A.mk_sp (A.BVint Big_int.zero_big_int) ~type_:A.vtint in
      let zero : A.pterm = A.mk_sp (A.Plit zero_b) ~type_:A.vtint in
      let a = Option.map_dfl (fun x -> for_expr kind env ~ety:A.vtint x) zero a in
      let b = for_expr kind env ~ety:A.vtint b in
      let env, i = Env.inscope env (fun env ->
          let _ : bool = check_and_emit_name_free env x in
          let env = Env.Local.push env ~kind:`LoopIndex (x, A.vtint) in
          for_instruction ~ret kind env i) in
      env, mki (A.Iiter (x, a, b, i)) ?label:(Option.map unloc lbl)

    | Ewhile (lbl, c, i) ->
      let c = for_expr kind env ~ety:A.vtbool c in
      let env, i = for_instruction ~ret kind env i in
      env, mki (A.Iwhile (c, i)) ?label:(Option.map unloc lbl)

    | Edorequire (e, f) ->
      let e = for_expr ~ety:A.vtbool kind env e in
      let f = for_expr kind env f in
      let ty = Option.get f.type_ in

      if not (Type.Michelson.is_type ty) then
        Env.emit_error env (f.loc, InvalidTypeForDoRequire);

      env, mki (A.Irequire (true, e, f))

    | Edofailif (e, f) ->
      let e = for_expr ~ety:A.vtbool kind env e in
      let f = for_expr kind env f in
      let ty = Option.get f.type_ in

      if not (Type.Michelson.is_type ty) then
        Env.emit_error env (f.loc, InvalidTypeForDoFailIf);

      env, mki (A.Irequire (false, e, f))

    | Efail e ->
      let e = for_expr kind env e in

      e.type_ |> Option.iter (fun ty ->
          if (not (Type.Michelson.is_packable ty))
          then (Env.emit_error env (e.loc, InvalidTypeForFail)));

      env, mki (A.Ifail e)

    | Efailsome e ->
      let e = for_expr kind env e in

      e.type_ |> Option.iter (fun ty ->
          match ty with
          | A.Toption (ty) when Type.Michelson.is_packable ty-> ()
          | _ -> Env.emit_error env (e.loc, InvalidTypeForFailSome));

      env, mki (A.Ifailsome e)

    | Eassert lbl ->
      let env =
        if (check_and_emit_name_free env lbl) then
          Env.Label.push env (lbl, `Plain)
        else env in
      env, mki (Ilabel lbl)

    | Ematchwith (e, bs) -> begin
        match for_gen_matchwith (expr_mode kind) capture0 env (loc i) e bs with
        | None -> bailout () | Some (kd, ctors, me, (wd, bsm, args), is) ->

          let env, is = List.fold_left_map (fun env (i, xtys) ->
              Env.inscope env (fun env ->
                  let env = Env.Local.pushn ~kind:`Pattern env xtys in
                  for_instruction ~ret kind env i)) env (List.combine is args) in

          let aout = List.pmap (fun (cname, _, _) ->
              let bse =
                match Mstr.find (unloc cname) bsm, wd with
                | Some k, _ ->
                  Some (List.nth is k, List.map fst (List.nth args k))
                | None, Some _ ->
                  None
                | None, None ->
                  Some (mki (Iseq []), []) in
              bse |> Option.map
                (fun (bse, args) -> (A.mk_sp (A.Mconst (cname, args)), bse))
            ) ctors in

          let aout =
            Option.fold
              (fun aout extra -> aout @ [A.mk_sp A.Mwild, extra])
              aout (Option.map (List.nth is) wd) in

          let aout =
            match decompile_match_with kd aout with
            | Some (`List ((x, xs, bcons), bnil)) ->
              A.Imatchlist (me, x, xs, bcons, bnil)

            | Some (`Or ((xl, bl), (xr, br))) ->
              A.Imatchor (me, xl, bl, xr, br)

            | Some (`Option ((x, bsome), bnone)) ->
              A.Imatchoption (me, x, bsome, bnone)

            | None -> A.Imatchwith (me, aout)

          in env, mki aout
      end

    | Elabel lbl ->
      let env =
        if   check_and_emit_name_free env lbl
        then Env.Label.push env (lbl, `Code)
        else env
      in env, mki (Ilabel lbl)

    | Enothing ->
      env, mki (Iseq [])

    | Ereturn re ->
      if Option.is_none ret then
        Env.emit_error env (loc re, ReturnInVoidContext);
      env, mki (Ireturn (for_expr ?ety:ret  kind env re))

    | Evar (x, ty, v, c) ->
      let ty = Option.bind (for_type env) ty in
      let v  = for_expr kind env ?ety:ty v in
      let env =
        let _ : bool = check_and_emit_name_free env x in
        if Option.is_some v.A.type_ then
          let kind = if c then `Const else `Standard in
          Env.Local.push env (x, Option.get v.A.type_) ~kind
        else env in

      Option.iter (fun ty ->
          if not (valid_var_or_arg_type ty) then
            Env.emit_error env (loc x, InvalidVarOrArgType)) v.A.type_;

      env, mki (A.Ideclvar (x, v, c))

    | Evaropt (x, ty, v, fa, c) ->
      let ty = Option.bind (for_type env) ty in
      let oty = Option.bind (fun ty -> Some (A.Toption ty)) ty in
      let v  = for_expr kind env ?ety:oty v in
      let fa  = Option.map (for_expr kind env) fa in

      Option.iter (fun (fa : A.pterm) -> Option.iter (fun ty ->
          if (not (Type.Michelson.is_packable ty))
          then (Env.emit_error env (fa.loc, InvalidTypeForFail))) fa.type_) fa;

      let env =
        let _ : bool = check_and_emit_name_free env x in
        match v.A.type_ with
        | Some A.Toption ty -> begin
            let kind = if c then `Const else `Standard in
            Env.Local.push env (x, ty) ~kind
          end
        | _ -> (Env.emit_error env (v.loc, InvalidTypeDeclOpt); env)
      in

      Option.iter (fun ty ->
          if not (valid_var_or_arg_type ty) then
            Env.emit_error env (loc x, InvalidVarOrArgType)) v.A.type_;

      env, mki (A.Ideclvaropt (x, v, fa, c))

    | _ ->
      Env.emit_error env (loc i, InvalidInstruction);
      bailout ()

  with E.Failure ->
    env, mki (Iseq [])

(* -------------------------------------------------------------------- *)
and for_instruction ~(ret : A.type_ option) (kind : imode_t) (env : env) (i : PT.expr) : env * A.instruction =
  Env.inscope env (fun env -> for_instruction_r ~ret kind env i)

(* -------------------------------------------------------------------- *)
let for_effect (kind : imode_t) (env : env) (effect : PT.expr) =
  Env.inscope env (fun env ->
      let env, i = for_instruction ~ret:None kind env effect in (env, (env, i)))

(* -------------------------------------------------------------------- *)
type spmode = [`Global | `Local]

let for_specification_item
    (mode : spmode) (env, poenv : env * env) (v : PT.specification_item)
  : (env * env) * (env ispecification) list
  =
  match unloc v with
  | PT.Vpredicate (x, args, f) ->
    let env, (args, f) =
      Env.inscope env (fun env ->
          let env, args = for_args_decl ~can_asset:true env args in
          let args = List.pmap id args in
          let f = for_formula env f in
          (env, (args, f))) in

    let decl = { pr_name = x; pr_args = args; pr_body = f; } in

    let poenv =
      if not (check_and_emit_name_free poenv x) then poenv else
        Env.Predicate.push poenv decl in

    (env, poenv), [`Predicate (x, args, f)]

  | PT.Vdefinition (x, ty, y, f) ->
    let poenv, def =
      Env.inscope poenv (fun poenv ->
          let poenv, arg = for_arg_decl ~can_asset:true poenv (y, ty, None) in

          match arg with
          | Some ((_, A.Tasset asset) as arg) ->
            let f = for_formula poenv f in (poenv, Some (asset, arg, f))

          | _ -> (poenv, None)) in

    let decl =
      Option.map (fun (asset, arg, f) ->
          { df_name = x; df_arg = arg; df_asset = asset; df_body = f; }) def in

    let poenv =
      if not (check_and_emit_name_free poenv x) then poenv else
        Option.fold (fun poenv decl ->
            Env.Definition.push poenv decl) poenv decl in

    let item = Option.map (fun decl ->
        `Definition (decl.df_name, decl.df_arg, decl.df_body)) decl in
    (env, poenv), Option.get_as_list item

  | PT.Vvariable (x, ty, e) ->
    let ty = for_type env ty in
    let e  = Option.map (for_expr `Ghost env ?ety:ty) e in

    ty |> Option.iter (fun ty ->
        if not (valid_var_or_arg_type ty) then
          Env.emit_error env (loc x, InvalidVarOrArgType));

    let env, poenv =
      if not (check_and_emit_name_free poenv x) then env, poenv else
        Option.fold (fun (env, poenv) ty ->
            let decl = {
              vr_name =  x; vr_type =   ty; vr_kind =       `Ghost;
              vr_invs = []; vr_def  = None;
              vr_core = None;
            } in

            let env   = if mode = `Global then Env.Var.push env decl else env in
            let poenv = Env.Var.push poenv decl in

            (env, poenv)) (env, poenv) ty

    in (env, poenv), [`Variable (x, e)]

  | PT.Vassert (x, f, invs, uses) -> begin
      if mode = `Global then
        Env.emit_error env (loc x, AssertInGlobalSpec);
      let env0 =
        match Env.Label.lookup env (unloc x) with
        | None ->
          Env.emit_error env (loc x, UnknownLabel (unloc x));
          env
        | Some (env, _) ->
          env
      in

      let for_inv (lbl, linvs) =
        (lbl, List.map (for_formula env0) linvs) in

      let f    = for_formula env0 f in
      let invs = List.map for_inv invs in

      (env, poenv), [`Asset (x, f, invs, uses)]
    end

  | PT.Veffect i ->
    if mode = `Global then
      Env.emit_error env (loc i, EffectInGlobalSpec);
    (* FIXME: we are not properly tracking labels here *)
    let _, ((poenv, _) as i) = for_effect `Ghost poenv i in
    (env, poenv), [`Effect i]

  | PT.Vfails l -> begin
      let failures = [
        "InvalidCaller"       , A.vtstring;
        "InvalidSource"       , A.vtstring;
        "InvalidCondition"    , A.Ttuple [A.vtstring; A.vtstring];
        "NotFound"            , A.vtstring;
        "KeyExists"           , A.vtstring;
        "KeyExistsOrNotFound" , A.vtstring;
        "DivByZero"           , A.vtstring;
        "NatNegAssign"        , A.vtstring;
        "NoTransfer"          , A.vtstring;
        "InvalidState"        , A.vtstring;
      ] in
      let l, env = List.fold_left (
          fun (accu, aenv) (lbl, fid, arg, atype, f) ->
            let ty = for_type_exn aenv atype in
            Option.iter (fun x ->
                let vx = unloc x in
                let tfid_opt : A.ptyp option = List.assoc_opt vx failures in
                match tfid_opt with
                | Some tfid -> begin
                    if (not (Type.equal ty tfid))
                    then (Env.emit_error env (loc (fst atype), InvalidFailIdType (vx, tfid, ty)))
                  end
                | None -> (Env.emit_error env (loc x, UnknownFailId (vx)))
              ) fid;
            let env_internal = Env.Local.push env (arg, ty) in
            let f = for_formula env_internal f in
            let env =
              if   check_and_emit_name_free env lbl
              then Env.Label.push env (lbl, `Plain)
              else env
            in
            (lbl, fid, arg, ty, f)::accu, env
        ) ([], env) l in

      (env, poenv), [`Fails (List.rev l)]
    end

  | PT.Vpostcondition (x, f, invs, uses, kind) -> begin
      begin match kind, mode with
        | Some PKInv, `Local ->
          Env.emit_error env (loc x, ContractInvariantInLocalSpec)
        | Some PKPost, `Global ->
          Env.emit_error env (loc x, PostConditionInGlobalSpec)
        | _, _ -> () end;

      let for_inv (lbl, linvs) =
        let env0 =
          match Env.Label.lookup env (unloc lbl) with
          | None ->
            Env.emit_error env (loc lbl, UnknownLabel (unloc lbl));
            env
          | Some (env, `Loop lblty) ->
            Option.fold (fun env (aname, _) ->
                let ty = A.Tasset (mkloc (loc lbl) (unloc aname)) in
                let ty = A.Tcontainer (ty, A.AssetView) in
                let env = Env.Local.push env (mkloc coreloc "toiterate", ty) in
                let env = Env.Local.push env (mkloc coreloc "iterated", ty) in
                env) env (Type.as_asset_collection lblty)
          | Some (_, _) ->
            Env.emit_error env (loc lbl, NonLoopLabel (unloc lbl));
            env
        in (lbl, List.map (for_formula ~invariant:true env0) linvs) in
      let f    = for_formula poenv f in
      let invs = List.map for_inv invs in
      (env, poenv), [`Postcondition (x, f, invs, uses)]
    end

(* -------------------------------------------------------------------- *)
let for_specification mode ((env, poenv) : env * env) (v : PT.specification) =
  let (env, _), items =
    List.fold_left_map (for_specification_item mode) (env, poenv) (fst (unloc v))
  in (env, List.flatten items)

(* -------------------------------------------------------------------- *)
module SecurityPred = struct
  type _ mode =
    | EntryDesc : A.entry_description mode
    | Role      : A.lident list       mode
    | Entry     : A.security_entry    mode

  let validate1 (type a) (env : env) (mode : a mode) (v : PT.security_arg) : a =
    match mode with
    | EntryDesc -> for_entry_description env v
    | Role      -> for_security_role     env v
    | Entry     -> for_security_entry    env v

  type _ validator =
    | V0 : unit validator
    | VC : 'a mode * 'b validator -> ('a * 'b) validator

  let (^:) m v = VC (m, v)

  exception ArgCountError

  let rec vdlen : type a . a validator -> int =
    function V0 -> 0 | VC (_, vd) -> 1 + vdlen vd

  let rec validate
    : type a . env -> a validator * PT.security_arg list -> a
    = fun env -> function
      | V0, [] ->
        ()

      | VC (m, vd), v :: args ->
        let v    = validate1 env m    v     in
        let args = validate  env (vd, args) in
        (v, args)

      | _, _ ->
        raise ArgCountError

  type predc =
    | PredC : ('a -> A.security_node) * 'a validator -> predc

  let pclen (PredC (_, vd)) = vdlen vd

  let vd1 f m =
    PredC ((fun (x, ()) -> f x), m ^: V0)

  let vd2 f m1 m2 =
    PredC
      ((fun (x, (y, ())) -> f x y),
       m1 ^: m2 ^: V0)

  let vd3 f m1 m2 m3 =
    PredC
      ((fun (x, (y, (z, ()))) -> f x y z),
       m1 ^: m2 ^: m3 ^: V0)

  let validate_and_build env (PredC (f, vd)) args =
    f (validate env (vd, args))

  let preds = [
    "only_by_role",           vd2 (fun x y   -> A.SonlyByRole        (x, y)   ) EntryDesc Role;
    "only_in_entry",          vd2 (fun x y   -> A.SonlyInEntry       (x, y)   ) EntryDesc Entry;
    "only_by_role_in_entry",  vd3 (fun x y z -> A.SonlyByRoleInEntry (x, y, z)) EntryDesc Role Entry;
    "not_by_role",            vd2 (fun x y   -> A.SnotByRole         (x, y)   ) EntryDesc Role;
    "not_in_entry",           vd2 (fun x y   -> A.SnotInEntry        (x, y)   ) EntryDesc Entry;
    "not_by_role_in_entry",   vd3 (fun x y z -> A.SnotByRoleInEntry  (x, y, z)) EntryDesc Role Entry;
    "transferred_by",         vd1 (fun x     -> A.StransferredBy     (x)      ) EntryDesc;
    "transferred_to",         vd1 (fun x     -> A.StransferredTo     (x)      ) EntryDesc;
    "no_storage_fail",        vd1 (fun x     -> A.SnoStorageFail     (x)      ) Entry;
  ]

  let preds = Mid.of_list preds
end

(* -------------------------------------------------------------------- *)
let for_security_item (env : env) (v : PT.security_item) : (env * A.security_item) option =
  let module E = struct exception Bailout end in

  try
    let loc, (label, name, args) = Location.deloc v in

    (* FIXME: check and add label in env *)

    let sp =
      match Mid.find_opt (unloc name) SecurityPred.preds with
      | None ->
        Env.emit_error env (L.loc name, NoSuchSecurityPredicate (unloc name));
        raise E.Bailout
      | Some method_ -> method_
    in

    let ne = SecurityPred.pclen sp in
    let ng = List.length args in

    if ne <> ng then begin
      Env.emit_error env (loc, InvalidNumberOfArguments (ne, ng));
      raise E.Bailout
    end;

    let security_node : A.security_node =
      SecurityPred.validate_and_build env sp args
    in

    let security_item : A.security_item =
      A.{ loc; label; predicate = A.{ loc; s_node = security_node; }; }
    in

    Some (env, security_item)

  with E.Bailout -> None

(* -------------------------------------------------------------------- *)
let for_security (env : env) (v : PT.security) : env * A.security =
  let env, items = List.fold_left (fun (env, items) x ->
      match for_security_item env x with
      | Some (e, v) -> (e, v::items)
      | None -> (env, items)
    ) (env, []) (fst (unloc v)) in
  env, A.{ items = List.rev items; loc = loc v; }

(* -------------------------------------------------------------------- *)
let for_named_state ?enum (env : env) (x : PT.lident) =
  match Env.State.byctor env (unloc x) with
  | None ->
    Env.emit_error env (loc x, UnknownState (unloc x));
    mkloc (loc x) "<error>"

  | Some state ->
    let sname = unloc state.sd_name in

    if Option.get_dfl ("$" ^ statename) enum <> sname then begin
      Env.emit_error env (loc x, ForeignState (enum, Some sname));
      mkloc (loc x) "<error>"
    end else
      x

(* -------------------------------------------------------------------- *)
let rec for_state_formula ?enum (env : env) (st : PT.expr) : A.sexpr =
  let mk_sp = A.mk_sp ~loc:(loc st) in

  match unloc st with
  | Eterm ((None, None), x) ->
    mk_sp (A.Sref (for_named_state ?enum env x))

  | Eapp (Foperator { pldesc = Logical Or }, [e1; e2]) ->
    let s1 = for_state_formula ?enum env e1 in
    let s2 = for_state_formula ?enum env e2 in
    mk_sp (A.Sor (s1, s2))

  | Eany ->
    mk_sp (A.Sany)

  | _ ->
    Env.emit_error env (loc st, InvalidStateExpression);
    mk_sp (A.Sref (mkloc (loc st) "<error>"))

(* -------------------------------------------------------------------- *)
let named_sig_compatible args xargs =
  let module E = struct exception Incompatible end in

  try
    if List.length args <> List.length xargs then
      raise E.Incompatible;

    List.iter2 (fun arg xarg ->
        match arg, xarg with
        | Some ({ pldesc = name }, ty), Some ({ pldesc = xname }, xty) ->
          if name <> xname || not (Type.equal ty xty) then
            raise E.Incompatible
        | _, _ -> ()) args xargs;
    true

  with E.Incompatible -> false

(* -------------------------------------------------------------------- *)
let for_function
    ?(xspecs : PT.specfun loced list option) (env as topenv : env) (fdecl : PT.s_function loced)
  =
  let { pldesc = fdecl; plloc = loc; } = fdecl in

  Env.inscope env (fun env ->
      let env, args = for_args_decl env fdecl.args in
      let rty       = Option.bind (for_type env) fdecl.ret_t in
      let env, body = for_instruction ~ret:rty `Concrete env fdecl.body in
      let env, spec =
        let poenv = rty |> Option.fold (fun poenv rty ->
            let decl = {
              vr_name = mkloc loc "result";
              vr_type = rty;
              vr_kind = `Ghost;
              vr_invs = [];
              vr_def  = None;
              vr_core = None;
            } in Env.Var.push poenv decl
          ) env in

        let spec =
          let myspec { plloc = xloc; pldesc = (kind, x, xargs, xspec) } =
            match kind, fdecl.getter with
            | PT.SKgetter  , true
            | PT.SKfunction, false ->
              if unloc x = unloc fdecl.name then begin
                let _, xargs = for_args_decl topenv xargs in
                if not (named_sig_compatible args xargs) then
                  Env.emit_error env (xloc, IncompatibleSpecSig);
                Some xspec
              end  else None

            | _ -> None in

          Option.get_as_list fdecl.spec
          @ List.pmap myspec (Option.get_dfl [] xspecs) in

        let env, items =
          List.fold_left_map
            (fun env spec -> for_specification `Local (env, poenv) spec)
            env spec in

        env, List.flatten items in

      if Option.is_some rty && not (List.exists Option.is_none args) then
        if check_and_emit_name_free env fdecl.name then
          (env, Some {
              fs_name  = fdecl.name;
              fs_kind  = if fdecl.getter then FKgetter else if fdecl.view then FKview else FKfunction;
              fs_args  = List.pmap id args;
              fs_retty = Option.get rty;
              fs_body  = body;
              fs_spec  = spec; })
        else (env, None)
      else (env, None))

(* -------------------------------------------------------------------- *)
let rec for_callby (env : env) kind (cb : PT.expr) =
  match unloc cb with
  | Eany -> [mkloc (loc cb) None]

  | Eapp (Foperator { pldesc = Logical Or }, [e1; e2]) ->
    (for_callby env kind e1) @ (for_callby env kind e2)

  | Eterm (_, an) when Env.Asset.exists env (unloc an)->
    let asset = Env.Asset.get env (unloc an) in
    if (not (Type.is_address asset.as_pkty))
    then Env.emit_error env (loc cb, match kind with | `Called -> InvalidCallByAsset | `Sourced -> InvalidSourcedByAsset );
    [mkloc (loc cb) (Some (A.mk_sp ~loc:(loc an) ~type_:(A.Tcontainer(Tasset an, Collection)) (A.Pvar (VTnone, Vnone, an))))]

  | _ ->
    [mkloc (loc cb) (Some (for_expr `Concrete env ~ety:A.vtaddress cb))]

(* -------------------------------------------------------------------- *)
let for_entry (env : env) (act : PT.entry_properties) i_exts =
  let fe = for_expr `Concrete env in
  let sourcedby = Option.map (fun (x, _, _) -> for_callby env `Sourced x) act.sourcedby , Option.bind (Option.map fe |@ proj3_2) act.sourcedby in
  let calledby  = Option.map (fun (x, _, _) -> for_callby env `Called x)  act.calledby  , Option.bind (Option.map fe |@ proj3_2) act.calledby  in
  let stateis   = Option.map (fun (x, o) -> for_named_state env x, Option.map fe o) act.state_is in
  let actfs     = fst act.accept_transfer, Option.map fe (snd act.accept_transfer) in
  let env, cst  = Option.foldmap (for_cfs `Concrete) env (Option.fst act.constants) in
  let env, req  = Option.foldmap (for_rfs `Concrete) env (Option.fst act.require) in
  let env, fai  = Option.foldmap (for_rfs `Concrete) env (Option.fst act.failif) in
  let env, poeffect =
    Option.foldmap (for_effect `Concrete) env (Option.fst i_exts) in
  let effect = Option.map snd poeffect in
  let env, funs = List.fold_left_map for_function env act.functions in
  let poenv  = Option.get_dfl env (Option.map fst poeffect) in
  let env, spec = Option.foldmap
      (fun env x -> for_specification `Local (env, poenv) x) env act.spec_fun in
  (env, (sourcedby, calledby, stateis, actfs, cst, req, fai, spec, funs, effect))

(* -------------------------------------------------------------------- *)
let for_transition ?enum (env : env) (state, when_, effect) =
  let tx_state  = for_named_state ?enum env state in
  let tx_when   =
    Option.map (for_formula env) (Option.fst when_) in
  let env, tx_effect = snd_map (Option.map snd)
      (Option.foldmap (for_effect `Concrete) env (Option.fst effect)) in

  env, { tx_state; tx_when; tx_effect; }

(* -------------------------------------------------------------------- *)
type enum_core = ((PT.lident * PT.type_t list * PT.enum_option list) list)

let for_core_enum_decl (env : env) (enum : enum_core loced) =
  (* FIXME: check that ctor names are available *)

  let ctors = unloc enum in

  match ctors with
  | [] ->
    Env.emit_error env (loc enum, EmptyEnumDecl);
    env, None

  | _ ->
    Option.iter
      (fun (_, x) ->
         Env.emit_error env (loc x, DuplicatedCtorName (unloc x)))
      (List.find_dup unloc (List.map proj3_1 ctors));

    let ctors =
      let map =
        List.fold_left (fun ctors (cname, cty, opts) ->
            let cty = List.pmap (for_type env) cty in
            let bd  =
              match Mid.find (unloc cname) ctors with
              | (x, cty', opts') -> (x, cty', opts' @ opts)
              | exception Not_found -> (cname, cty, opts) in
            Mid.add (unloc cname) bd ctors) Mid.empty ctors
      in
      List.map
        (fun k -> Mid.find k map)
        (List.undup (fun x -> x) (List.map (proj3_1 %> unloc) ctors)) in

    let for1 (cname, options) =
      let init, inv =
        List.fold_left (fun (init, inv) option ->
            match option with
            | PT.EOinitial ->
              (init+1, inv)
            | PT.EOspecification spec ->
              (init, List.rev_append spec inv)
          ) (0, []) options in

      if init > 1 then
        Env.emit_error env (loc cname, DuplicatedInitMarkForCtor);
      (init <> 0, List.rev inv) in

    let for1 env ((cname : PT.lident), cty, options) =
      let init, inv = for1 (cname, options) in

      (env, (cname, cty, init, inv)) in

    let env, ctors = List.fold_left_map for1 env ctors in

    let ictor =
      let ictors =
        List.pmap
          (fun (x, _, b, _) -> if b then Some x else None)
          ctors in

      match ictors with
      | [] ->
        proj4_1 (List.hd ctors)
      | init :: ictors ->
        if not (List.is_empty ictors) then
          Env.emit_error env (loc enum, MultipleInitialMarker);
        init in

    env, Some (unloc ictor, List.map (fun (x, cty, _, inv) -> (x, cty, inv)) ctors)

(* -------------------------------------------------------------------- *)
let for_enum_decl (env : env) (decl : (PT.lident * PT.enum_decl) loced) =
  let (name, (ctors, _)) = unloc decl in
  let env, ctors = for_core_enum_decl env (mkloc (loc decl) ctors) in
  let env, decl =
    Option.foldbind (fun env (sd_init, ctors) ->
        let sd_ctors = List.map (fun (x, cty, _) -> (x, cty, [])) ctors in
        let enum = { sd_name = name; sd_ctors; sd_init; sd_state = false; } in
        if   check_and_emit_name_free env name
        then Env.State.push env enum, Some enum
        else env, None) env ctors in
  let inv = Option.map (fun (_, ctors) -> List.map proj3_3 ctors) ctors in

  env, (decl, inv)

(* -------------------------------------------------------------------- *)
let for_import_decl (env : env) (decls : (PT.lident * PT.lident) loced list) =
  List.fold_left (fun (env, accu : env * importdecl list) (a : (PT.lident * PT.lident) loced) -> begin
        let lo, (id, path) = deloc a in
        match Micheline.parse (unloc path) with
        | Some content -> begin
            let views       = Micheline.get_views content in
            match Micheline.get_entrypoints content with
            | Some entrypoints -> begin
                let importdecl = { id_name = id; id_path = path; id_content = content; id_entrypoints = entrypoints; id_views = views } in
                (if   check_and_emit_name_free env id
                 then Env.Import.push env importdecl
                 else env), importdecl::accu
              end
            | None -> Env.emit_error env (lo, InvalidTzFile); (env, accu)
          end
        | None ->(Env.emit_error env (lo, FileNotFound (unloc path)); (env, accu))
      end) (env, []) decls

(* -------------------------------------------------------------------- *)
let for_enums_decl (env : env) (decls : (PT.lident * PT.enum_decl) loced list) =
  List.fold_left_map for_enum_decl env decls

(* -------------------------------------------------------------------- *)
let for_var_decl (env : env) (decl : PT.variable_decl loced) =
  let (x, ty, pe, ctt, invs, _) = unloc decl in

  let ty   = for_type env ty in
  let e    = Option.map (for_expr `Concrete env ?ety:ty) pe in
  let dty  =
    if   Option.is_some ty
    then ty
    else Option.bind (fun e -> e.A.type_) e in

  dty |> Option.iter (fun ty ->
      if not (valid_var_or_arg_type ty) then
        Env.emit_error env (loc x, InvalidVarOrArgType));

  let ctt  = match ctt with
    | VKconstant  -> `Constant
    | VKvariable  -> `Variable
  in

  if Option.is_none pe && (match dty with | Some A.Tsapling_state _ -> false | _ -> true) then
    Env.emit_error env (loc decl, UninitializedVar);

  match dty with
  | None ->
    (env, (None, None))

  | Some dty ->

    let decl = {
      vr_name = x;
      vr_type = dty;
      vr_kind = ctt;
      vr_core = None;
      vr_invs = [];
      vr_def  = Option.map (fun e -> (e, `Std)) e; } in

    if   (check_and_emit_name_free env x)
    then (Env.Var.push env decl, (Some decl, Some invs))
    else (env, (None, Some invs))

(* -------------------------------------------------------------------- *)
let for_vars_decl (env : env) (decls : PT.variable_decl loced list) =
  List.fold_left_map for_var_decl env decls

(* -------------------------------------------------------------------- *)
let for_var_specs
    (env : env) (specs : (PT.lident * PT.label_exprs) loced list)
  =
  List.iter (fun { pldesc = (x, _) } ->
      if not (Env.Var.exists env (unloc x)) then
        Env.emit_error env (loc x, UnknownVariable (unloc x)))
    specs

(* -------------------------------------------------------------------- *)
let for_fun_decl
    ?(xspecs : PT.specfun loced list option) (env : env) (fdecl : PT.s_function loced)
  =
  let env, decl = for_function ?xspecs env fdecl in
  (Option.fold (fun env decl -> Env.Function.push env decl) env decl, decl)

(* -------------------------------------------------------------------- *)
let for_funs_decl
    (env : env) (decls : PT.s_function loced list) (xspecs : PT.specfun loced list)
  =
  List.fold_left_map (for_fun_decl ~xspecs) env decls

(* -------------------------------------------------------------------- *)
let for_fun_specs (env : env) (specs : PT.specfun loced list) =
  let for1 { plloc = _; pldesc = (kind, x, _, _) } =
    match kind with
    | PT.SKfunction -> begin
        match Env.Function.lookup env (unloc x) with
        | Some fund when fund.fs_kind = A.FKfunction ->
          ()

        | _ ->
          Env.emit_error env (loc x, UnknownFunction (unloc x))
      end

    | PT.SKgetter -> begin
        match Env.Function.lookup env (unloc x) with
        | Some fund when fund.fs_kind = A.FKgetter ->
          ()

        | _ ->
          Env.emit_error env (loc x, UnknownGetter (unloc x));
      end

    | PT.SKview -> begin
        match Env.Function.lookup env (unloc x) with
        | Some fund when fund.fs_kind = A.FKview ->
          ()

        | _ ->
          Env.emit_error env (loc x, UnknownGetter (unloc x));
      end

    | PT.SKentry -> begin
        if not (Env.Tentry.exists env (unloc x)) then
          Env.emit_error env (loc x, UnknownEntry (unloc x));
      end

  in List.iter for1 specs

(* -------------------------------------------------------------------- *)
type pre_assetdecl = {
  pas_name   : A.lident;
  pas_fields : (string * A.ptyp * PT.expr option * bool) loced list;
  pas_pkty   : A.ptyp;
  pas_pk     : A.lident list;
  pas_sortk  : A.lident list;
  pas_bm     : A.map_kind;
  pas_invs   : PT.label_exprs list;
  pas_state  : statedecl option;
  pas_init   : PT.expr list;
}

let for_asset_decl
    ?(xspecs = []) pkey (env : env) ((adecl, decl) : assetdecl * PT.asset_decl loced)
  =
  let (x, cfields, sfields, opts, postopts, _ (* FIXME *), _) = unloc decl in

  let for_field field =
    let (f, fty, init, shadow) = field in
    let fty  = for_type ~pkey env fty in

    if   check_and_emit_name_free env f
    then Option.map (fun fty -> mkloc (loc f) (unloc f, fty, init, shadow)) fty
    else None in

  let fields =
    let cfields =
      List.map
        (fun { pldesc = PT.Ffield (x, ty, e, _) } -> (x, ty, e, false))
        cfields in

    let sfields =
      List.map
        (fun { pldesc = PT.Ffield (x, ty, e, _) } -> (x, ty, e,  true))
        sfields in

    List.pmap for_field (cfields @ sfields) in

  Option.iter
    (fun (_, { plloc = lc; pldesc = (name, _, _, _) }) ->
       Env.emit_error env (lc, DuplicatedFieldInAssetDecl name))
    (List.find_dup (fun x -> proj4_1 (unloc x)) fields);

  let get_field name =
    List.Exn.find
      (fun { pldesc = (x, _, _, _) } -> x = name)
      fields
  in

  let pks     = List.pmap (function PT.AOidentifiedby pk -> Some pk | _ -> None)     opts in
  let sortks  = List.pmap (function PT.AOsortedby     sk -> Some sk | _ -> None)     opts in
  let invs    = List.pmap (function PT.APOconstraints fi -> Some fi | _ -> None) postopts in
  let state   = List.pmap (function PT.APOstates      st -> Some st | _ -> None) postopts in
  let inits   = List.pmap (function PT.APOinit        it -> Some it | _ -> None) postopts in

  let invs    =
    let xinvs =
      List.pmap (fun { pldesc = ({ pldesc = xname }, xinv ) } ->
          if xname = unloc adecl.as_name then Some xinv else None) xspecs in
    invs @ xinvs in

  let to_a_map_kind = function
    | PT.MKMap -> A.MKMap
    | PT.MKBigMap -> A.MKBigMap
    | PT.MKIterableBigMap -> A.MKIterableBigMap in

  let bigmaps : A.map_kind = List.fold_left (fun accu x -> match x with | PT.AOtoMapKind x -> to_a_map_kind x | _ -> accu) A.MKMap opts in

  let pks =
    let dokey key =
      match get_field (unloc key) with
      | None ->
        Env.emit_error env (loc key, UnknownFieldName (unloc key));
        None
      | Some { pldesc = (_, _, _, true) } ->
        Env.emit_error env (loc key, ShadowPKey);
        None
      | Some _ -> Some key in

    List.pmap dokey (List.flatten pks) in

  let pks =
    if   List.is_empty pks
    then Option.get_as_list (Option.map (L.lmap proj4_1) (List.ohead fields))
    else pks in

  pks |> List.iter (fun pk ->
      match Option.get (get_field (unloc pk)) with
      | { pldesc = _, ty, _, _; plloc = loc; } ->
        if not (Type.pktype ty) then
          Env.emit_error env (loc, InvalidTypeForPk)
    );

  let _ : Sstr.t =
    List.fold_left (fun seen pk ->
        if Sstr.mem (unloc pk) seen then
          Env.emit_error env (loc pk, DuplicatedPkeyField (unloc pk));
        Sstr.add (unloc pk) seen) Sstr.empty pks in

  begin
    let opks =
      List.filter
        (fun { pldesc = (fd, _, _, _) } ->
           List.exists (fun f -> unloc f = fd) pks)
        fields in
    let opks = List.map (unloc %> proj4_1) opks in

    if opks <> List.map unloc pks then
      Env.emit_error env (loc decl, MisorderedPkeyFields)
  end;

  let pkty =
    Type.create_tuple
      (List.map (fun pk -> proj4_2 (unloc (Option.get (get_field (unloc pk))))) pks) in

  let sortks =
    let dokey key =
      match get_field (unloc key) with
      | None ->
        Env.emit_error env (loc key, UnknownFieldName (unloc key));
        None
      | Some { pldesc = (_, _, _, true) } ->
        Env.emit_error env (loc key, ShadowSKey);
        None
      | Some _ -> Some key in

    List.pmap dokey sortks in

  let state =
    let for1 x =
      let state = Env.State.lookup env (unloc x) in
      if Option.is_none state then
        Env.emit_error env (loc x, UnknownEnum (unloc x));
      state in

    if List.length state > 1 then
      Env.emit_error env (loc decl, MultipleAssetStateDeclaration);

    let state = List.map for1 state in
    Option.bind (fun x -> x) (List.ohead state) in

  let env, adecl =
    let for_ctor { pldesc = (fd, fdty, fdinit, shadow); plloc = fdloc; } =
      let fddfl =
        fdinit |> Option.map (fun fdinit ->
            A.mk_sp ~type_:fdty ~loc:(loc fdinit)
              (A.Pvar (VTnone, Vnone, mkloc (loc fdinit) "<init>"))) in
      { fd_name  = mkloc fdloc fd;
        fd_type  = fdty;
        fd_dfl   = fddfl;
        fd_ghost = shadow; } in

    let adecl = { adecl with as_fields = List.map for_ctor fields } in
    let env   = Env.Asset.push env adecl in

    (env, adecl) in

  let module E = struct exception Bailout end in

  try
    if List.is_empty adecl.as_fields then begin
      Env.emit_error env (loc decl, AssetWithoutFields);
      raise E.Bailout
    end;

    if List.is_empty pks then env, None else

      let aout =
        { pas_name   = x;
          pas_fields = fields;
          pas_pkty   = pkty;
          pas_pk     = pks;
          pas_sortk  = sortks;
          pas_bm     = bigmaps;
          pas_invs   = invs;
          pas_state  = state;
          pas_init   = List.flatten inits; }

      in env, Some aout

  with E.Bailout -> env, None

(* -------------------------------------------------------------------- *)
let for_assets_decl (env as env0 : env) (decls : PT.asset_decl loced list) xspecs =
  let (b, env), adecls = List.fold_left_map (fun (b, env) decl ->
      let (name, _, _, _, _, _, _) = unloc decl in
      let b = b && check_and_emit_name_free env name in
      let d = { as_name   = name;
                as_fields = [];
                as_pkty   = A.vtunit;
                as_pk     = [];
                as_sortk  = [];
                as_bm     = A.MKMap;
                as_invs   = [];
                as_state  = None;
                as_init   = []; } in
      ((b, Env.Asset.push env d), d)) (true, env) decls in

  let module E = struct exception Bailout end in

  try
    if not b then
      raise E.Bailout;

    let pkey = List.map (fun { pldesc = (x, _, _, _, _, _, _) } -> unloc x) decls in

    let _, decls =
      List.fold_left_map
        (for_asset_decl ~xspecs pkey) env (List.combine adecls decls) in

    if not (List.for_all Option.is_some decls) then
      raise E.Bailout;

    let decls = List.map Option.get decls in
    let pksty =
      let for1 decl =
        let fields =
          List.filter
            (fun fd -> List.exists (fun f -> unloc f = proj4_1 (L.unloc fd)) decl.pas_pk)
            decl.pas_fields in

        Type.create_tuple (List.map (fun fd -> proj4_2 (unloc fd)) fields) in

      List.map for1 decls in

    let pksty = Mint.of_list (List.mapi (fun i x -> (i, x)) pksty) in

    let adecls =
      let for1 decl =
        let for_ctor { pldesc = (fd, fdty, fdinit, shadow); plloc = fdloc; } =
          let fdty  = Type.subst pksty fdty in
          let fddfl =
            fdinit |> Option.map (fun fdinit ->
                A.mk_sp ~type_:fdty ~loc:(loc fdinit)
                  (A.Pvar (VTnone, Vnone, mkloc (loc fdinit) "<init>"))) in

          { fd_name  = mkloc fdloc fd;
            fd_type  = fdty;
            fd_dfl   = fddfl;
            fd_ghost = shadow; }
        in

        { as_name   = decl.pas_name;
          as_fields = List.map for_ctor decl.pas_fields;
          as_pkty   = decl.pas_pkty;
          as_pk     = decl.pas_pk;
          as_sortk  = decl.pas_sortk;
          as_bm     = decl.pas_bm;
          as_invs   = [];
          as_state  = Option.map (fun x -> x.sd_name) decl.pas_state;
          as_init   = []; }

      in List.map for1 decls in

    let env = List.fold_left Env.Asset.push env0 adecls in

    let adecls =
      let for1 adecl decl =
        let for_ctor ctor { pldesc = (_, _, dfl, shadow); plloc = xloc; } =
          if shadow && Option.is_none dfl then
            Env.emit_error env (xloc, MissingInitValueForShadowField);
          let fd_dfl =
            dfl |> Option.map
              (for_expr `Concrete env ~ety:ctor.fd_type) in
          { ctor with fd_dfl }
        in

        { adecl with
          as_fields = List.map2 for_ctor adecl.as_fields decl.pas_fields; }

      in List.map2 for1 adecls decls in

    let env, adecls =
      let for1 env (adecl, decl) =
        let env, as_invs =
          Env.inscope env (fun env ->
              let env =
                List.fold_left (fun env field ->
                    Env.Local.push env (field.fd_name, field.fd_type)
                  ) env adecl.as_fields
              in List.fold_left_map for_xlbls_formula env decl.pas_invs)

        in (env, { adecl with as_invs = List.flatten as_invs }) in

      List.fold_left_map for1 env (List.combine adecls decls)
    in

    let adecls =
      let for1 adecl decl =
        let forinit = function
          | { pldesc = PT.Erecord init1; plloc = thisloc }
            when List.for_all (fun (x, _) -> Option.is_none x) init1
            ->
            if List.length init1 <> List.length adecl.as_fields then begin
              Env.emit_error env (thisloc, InvalidAssetExpression); None
            end else
              let init1 =
                List.map2
                  (fun field (_, ie) ->
                     for_expr `Concrete env ~ety:(ty_of_init_ty env field.fd_type) ie)
                  adecl.as_fields init1 in
              Some init1

          | { pldesc = PT.Erecord init1; plloc = _; }
            when List.for_all
                (function (Some (PT.ValueAssign, _), _) -> true | _ -> false)
                init1
            ->
            let init1 =
              List.pmap (function (Some (_, x), e) -> Some (x, e) | _ -> None) init1 in

            let init1 =
              List.filter (fun (x, _) ->
                  if Option.is_none (get_field (unloc x) adecl) then
                    (Env.emit_error env (loc x, UnknownFieldName (unloc x)); false)
                  else true) init1 in

            let init1 =
              List.fold_left (fun init1 ({pldesc = x; plloc = tloc}, e) ->
                  let { fd_type = fty } = Option.get (get_field x adecl) in
                  let e = for_expr `Concrete env ~ety:(ty_of_init_ty env fty) e in
                  Mid.update x (fun es -> Some ((e, tloc) :: (Option.get_dfl [] es))) init1
                ) Mid.empty init1 in

            Mid.iter (fun x es ->
                List.iter
                  (fun (_, lloc) ->
                     Env.emit_error env (lloc, DuplicatedFieldInAssetOrRecordLiteral x))
                  (List.chop (List.rev es))
              ) init1;

            let init1 = List.map (fun fd ->
                match Mid.find_opt (unloc fd.fd_name) init1 with
                | None when Option.is_none fd.fd_dfl ->
                  Env.emit_error env
                    (loc fd.fd_name, MissingFieldInAssetOrRecordLiteral (unloc fd.fd_name));
                  None
                | None ->
                  fd.fd_dfl
                | Some es ->
                  Some (fst (Option.get (List.ohead (List.rev es))))
              ) adecl.as_fields in

            if   List.for_all Option.is_some init1
            then Some (List.pmap (fun x -> x) init1)
            else None


          | { plloc = thisloc } ->
            Env.emit_error env (thisloc, InvalidAssetExpression); None in

        { adecl with as_init = List.pmap forinit decl.pas_init } in

      List.map2 for1 adecls decls in

    (* Bind in working environment so that we keep labels *)
    let env = List.fold_left Env.Asset.push env adecls in

    (env, List.map Option.some adecls)

  with E.Bailout ->
    (env0, List.map (fun _ -> None) decls)

(* -------------------------------------------------------------------- *)
let for_asset_specs
    (env : env) (specs : (PT.lident * PT.label_exprs) loced list)
  =
  List.iter (fun { pldesc = (x, _) } ->
      if not (Env.Asset.exists env (unloc x)) then
        Env.emit_error env (loc x, UnknownAsset (unloc x)))
    specs

(* -------------------------------------------------------------------- *)
let for_record_decl k (env : env) (decl : PT.record_decl loced) =
  let name, fields, packing, _ = unloc decl in
  let fields =
    let get_field { pldesc = PT.Ffield (x, ty, e, _) } = (x, ty, e) in
    List.map get_field fields in

  let fields =
    let for1 (x, pty, e) =
      let ty = for_type env pty in

      ty |> Option.iter (fun ty ->
          if not (Type.Michelson.is_type ty) then
            Env.emit_error env (loc (fst pty), InvalidRecordFieldType));
      let e  = e |> Option.map (for_expr `Concrete env ?ety:ty) in
      (x, ty, e) in
    List.map for1 fields in

  let _, fields = List.fold_left_map (fun seen (x, ty, e) ->
      if Sid.mem (unloc x) seen then begin
        Env.emit_error env (loc x, DuplicatedFieldInRecordDecl (unloc x));
        (seen, None)
      end else (Sid.add (unloc x) seen, Some (x, ty, e))) Sid.empty fields in

  let fields = List.pmap (fun x -> x) fields in

  let fields =
    let for1 (x, ty, e) =
      match k with
      | `Record -> begin
          match check_and_emit_name_free env x, ty with
          | true, Some ty -> Some { rfd_name = x; rfd_type = ty; rfd_dfl = e }
          | _, _          -> None
        end
      | `Event -> begin
          match ty with
          | Some ty -> Some { rfd_name = x; rfd_type = ty; rfd_dfl = e }
          | _       -> None
        end
    in
    List.pmap for1 fields
  in

  let packing =
    let module E = struct
      exception InvalidPacking of (Location.t * [`Expr | `Format | `Dup of ident])
    end in

    let rec doit (e : PT.expr) =
      match unloc e with
      | Etuple es ->
        RNode (List.map doit es)
      | Eterm  ((None, None), x) ->
        RLeaf x
      | _ ->
        raise (E.InvalidPacking (loc e, `Expr)) in

    packing |> Option.bind (fun e ->
        try
          let packing = doit e in
          let vars    =
            let rec doit = function
              | RLeaf x  -> Mid.singleton (unloc x) [loc x]
              | RNode ch ->
                List.fold_left
                  (Mid.merge (fun _ s1 s2 ->
                       Some ((Option.get_dfl [] s1) @ (Option.get_dfl [] s2))))
                  Mid.empty (List.map doit ch)
            in doit packing in

          let nvars = Mid.fold (fun _ lcs i -> List.length lcs + i) vars 0 in

          if nvars <> List.length fields then
            raise (E.InvalidPacking (loc e, `Format));

          Mid.iter (fun x lcs ->
              if List.length lcs > 1 then
                raise (E.InvalidPacking (List.hd (List.rev lcs), `Dup x)))
            vars;

          Some packing

        with E.InvalidPacking (lc, e) ->
          begin match e with
            | `Expr   -> Env.emit_error env (lc, InvalidPackingExpr)
            | `Format -> Env.emit_error env (lc, InvalidPackingFormat)
            | `Dup x  -> Env.emit_error env (lc, DuplicatedPackingVar x) end;
          None) in

  if check_and_emit_name_free env name then
    let rdecl = { rd_name    = name   ;
                  rd_fields  = fields ;
                  rd_packing = packing; } in
    match k with
    | `Record -> Env.Record.push env rdecl, Some rdecl
    | `Event  -> Env.Event.push env rdecl, Some rdecl
  else (env, None)

(* -------------------------------------------------------------------- *)
let for_records_decl (env : env) (decls : PT.record_decl loced list) =
  List.fold_left_map (for_record_decl `Record) env decls

let for_events_decl (env : env) (decls : PT.record_decl loced list) =
  List.fold_left_map (for_record_decl `Event) env decls

(* -------------------------------------------------------------------- *)
let for_acttx_decl
    ?(xspecs : PT.specfun loced list option) (env as topenv : env) (decl : acttx loced)
  =
  match unloc decl with
  | `Entry (x, args, pt, i_exts, _exts) -> begin
      let env, decl =
        Env.inscope env (fun env ->
            let env, args = for_args_decl env args in
            let env, (srcby, callby, stateis, actfs, csts, reqs, fais, spec, funs, effect) =
              for_entry env pt i_exts in

            let env, xspec =
              let myspec { plloc = xloc; pldesc = (kind, xname, xargs, xspec) } =
                match kind with
                | PT.SKentry ->
                  if unloc xname = unloc x then begin
                    let _, xargs = for_args_decl topenv xargs in

                    if not (named_sig_compatible args xargs) then
                      Env.emit_error env (xloc, IncompatibleSpecSig);
                    Some xspec
                  end else None

                | _ -> None in

              let env, items =
                List.fold_left_map
                  (fun env spec -> for_specification `Local (env, env) spec)
                  env (List.pmap myspec (Option.get_dfl [] xspecs))

              in env, List.flatten items in

            let decl =
              { ad_name   = x;
                ad_args   = List.pmap (fun x -> x) args;
                ad_srcby  = Option.get_dfl [] (fst srcby), snd srcby;
                ad_callby = Option.get_dfl [] (fst callby), snd callby;
                ad_stateis= stateis;
                ad_effect = Option.map (fun x -> `Raw x) effect;
                ad_funs   = funs;
                ad_csts   = Option.get_dfl [] csts;
                ad_reqs   = Option.get_dfl [] reqs;
                ad_fais   = Option.get_dfl [] fais;
                ad_spec   = Option.get_dfl [] spec @ xspec;
                ad_actfs  = actfs; } in

            (env, decl))

      in

      if check_and_emit_name_free env x then
        (Env.Tentry.push env decl, Some decl)
      else (env, None)
    end

  | `Transition (x, args, tgt, from_, entrys, tx, _exts) ->
    let env, decl =
      Env.inscope env (fun env ->
          let env, args = for_args_decl env args in
          let env, enum, tgt =
            let env, aout =
              Option.foldbind (fun env (vtg, ttg) ->
                  Option.foldbind (fun env aname ->
                      let asset = Env.Asset.get env (unloc aname) in
                      let env =
                        if check_and_emit_name_free env vtg then
                          Env.Local.push env (vtg, asset.as_pkty)
                        else env in
                      let tgt = (vtg, asset) in
                      (env, Option.map (fun x -> (unloc x, tgt)) asset.as_state))
                    env (for_asset_keyof_type env ttg))
                env tgt in
            env, Option.map fst aout, Option.map snd aout in

          let from_ = for_state_formula ?enum env from_ in
          let env, (srcby, callby, stateis, actfs, csts, reqs, fais, spec, funs, _effect) =
            for_entry env entrys None in

          let env, xspec =
            let myspec { plloc = xloc; pldesc = (kind, xname, xargs, xspec) } =
              match kind with
              | PT.SKentry when unloc xname = unloc x ->
                let _, xargs = for_args_decl topenv xargs in

                if not (named_sig_compatible args xargs) then
                  Env.emit_error env (xloc, IncompatibleSpecSig);
                Some xspec

              | _ -> None in

            let env, items =
              List.fold_left_map
                (fun env spec -> for_specification `Local (env, env) spec)
                env (List.pmap myspec (Option.get_dfl [] xspecs))

            in env, List.flatten items in


          let env, tx =
            List.fold_left_map (for_transition ?enum) env tx in

          let decl =
            { ad_name   = x;
              ad_args   = List.pmap (fun x -> x) args;
              ad_srcby  = Option.get_dfl [] (fst srcby),  snd srcby;
              ad_callby = Option.get_dfl [] (fst callby), snd callby;
              ad_stateis= stateis;
              ad_effect = Some (`Tx (from_, tgt, tx));
              ad_funs   = funs;
              ad_csts   = Option.get_dfl [] csts;
              ad_reqs   = Option.get_dfl [] reqs;
              ad_fais   = Option.get_dfl [] fais;
              ad_spec   = Option.get_dfl [] spec @ xspec;
              ad_actfs  = actfs; }

          in (env, decl))

    in

    if check_and_emit_name_free env x then
      (Env.Tentry.push env decl, Some decl)
    else (env, None)

(* -------------------------------------------------------------------- *)
let for_acttxs_decl
    (env : env) (decls : acttx loced list) (xspecs : PT.specfun loced list)
  =
  List.fold_left_map (for_acttx_decl ~xspecs) env decls

(* -------------------------------------------------------------------- *)
let for_specs_decl (env as poenv : env) (decls : PT.specification loced list) =
  List.fold_left_map
    (fun env { pldesc = x } -> for_specification `Global (env, poenv) x)
    env decls

(* -------------------------------------------------------------------- *)
let for_secs_decl (env : env) (decls : PT.security loced list) =
  List.fold_left_map
    (fun env { pldesc = x } -> for_security env x)
    env decls

(* -------------------------------------------------------------------- *)
let group_declarations (decls : (PT.declaration list)) =
  let empty = {
    gr_archetypes = [];
    gr_imports    = [];
    gr_states     = [];
    gr_enums      = [];
    gr_assets     = [];
    gr_records    = [];
    gr_events     = [];
    gr_vars       = [];
    gr_funs       = [];
    gr_acttxs     = [];
    gr_specs      = [];
    gr_specfuns   = [];
    gr_specvars   = [];
    gr_specassets = [];
    gr_secs       = [];
  } in

  let for1 { plloc = loc; pldesc = decl } (g : groups) =
    let mk x = Location.mkloc loc x in

    match decl with
    | PT.Darchetype (x, _, _, exts) ->
      { g with gr_archetypes = mk (x, exts) :: g.gr_archetypes }

    | PT.Dimport (id, path) ->
      { g with gr_imports = mk (id, path) :: g.gr_imports }

    | PT.Dvariable infos ->
      { g with gr_vars = mk infos :: g.gr_vars }

    | PT.Denum (PT.EKstate, infos) ->
      { g with gr_states = mk infos :: g.gr_states }

    | PT.Denum (PT.EKenum x, infos) ->
      { g with gr_enums = mk (x, infos) :: g.gr_enums }

    | PT.Dasset infos ->
      { g with gr_assets = mk infos :: g.gr_assets }

    | PT.Drecord infos ->
      { g with gr_records = mk infos :: g.gr_records }

    | PT.Devent infos ->
      { g with gr_events = mk infos :: g.gr_events }

    | PT.Dentry infos ->
      { g with gr_acttxs = mk (`Entry infos) :: g.gr_acttxs }

    | PT.Dtransition infos ->
      { g with gr_acttxs = mk (`Transition infos) :: g.gr_acttxs }

    | PT.Dfunction infos ->
      { g with gr_funs = mk infos :: g.gr_funs }

    | PT.Dspecification infos ->
      { g with gr_specs = mk infos :: g.gr_specs }

    | PT.Dsecurity infos ->
      { g with gr_secs = mk infos :: g.gr_secs }

    | PT.Dspecfun infos ->
      { g with gr_specfuns = mk infos :: g.gr_specfuns }

    | PT.Dspecvariable infos ->
      { g with gr_specvars = mk infos :: g.gr_specvars }

    | PT.Dspecasset infos ->
      { g with gr_specassets = mk infos :: g.gr_specassets }


    | Dtype      _  -> assert false
    | Dnamespace _  -> assert false
    | Dextension _  -> assert false
    | Dinvalid      -> assert false

  in List.fold_right for1 decls empty

(* -------------------------------------------------------------------- *)
type decls = {
  imports   : importdecl list;
  state     : statedecl option;
  variables : vardecl option list;
  enums     : statedecl option list;
  records   : recorddecl option list;
  events    : recorddecl option list;
  assets    : assetdecl option list;
  functions : env fundecl option list;
  acttxs    : env tentrydecl option list;
  specs     : env ispecification list list;
  secspecs  : A.security list;
}

let for_grouped_declarations (env : env) (toploc, g) =
  if not (List.is_empty g.gr_archetypes) then
    Env.emit_error env (toploc, InvalidArcheTypeDecl);

  if List.length g.gr_states > 1 then
    Env.emit_error env (toploc, MultipleStateDeclaration);

  let state, stinv, env =
    let for1 { plloc = loc; pldesc = state } =
      match for_core_enum_decl env (mkloc loc (fst state)) with
      | env, Some state -> Some (env, loc, state)
      | _  , None       -> None in

    match List.pmap for1 g.gr_states with
    | (env, loc, (init, ctors)) :: _ ->
      let stinv = List.map proj3_3 ctors in
      let ctors = List.map (fun (x, cty, _) -> (x, cty, [])) ctors in
      let decl = { sd_name  = mkloc loc ("$" ^ statename);
                   sd_state = true;
                   sd_ctors = ctors;
                   sd_init  = init; } in
      let vdecl = { vr_name = (mkloc loc statename);
                    vr_type = A.Tenum (mkloc loc ("$" ^ statename));
                    vr_kind = `Constant;
                    vr_invs = [];
                    vr_def  = None;
                    vr_core = Some Cstate; } in
      let env = Env.State.push env decl in
      let env = Env.Var.push env vdecl in
      (Some decl, Some stinv, env)
    | _ ->
      (None, None, env) in

  let env, imports      = for_import_decl    env g.gr_imports in
  let env, enums        = for_enums_decl     env g.gr_enums   in
  let env, records      = for_records_decl   env g.gr_records in
  let env, events       = for_events_decl    env g.gr_events  in
  let enums, especs     = List.split enums                    in
  let env, variables    = for_vars_decl      env g.gr_vars    in
  let variables, vspecs = List.split variables                in
  let env, assets       = for_assets_decl    env g.gr_assets g.gr_specassets in

  let () = for_asset_specs env g.gr_specassets in

  let env, enums =
    let check_enum_spec env (enum, spec) =
      match spec with None -> env, enum | Some spec ->

        let env, spec = List.fold_left_map for_lbls_formula env spec in

        Option.foldmap (fun env enum ->
            let sd_ctors =
              List.map2 (fun (x, args, oinv) inv -> (x, args, oinv @ inv))
                enum.sd_ctors spec in
            let enum = { enum with sd_ctors } in
            (Env.State.push env enum, enum)) env enum

    in List.fold_left_map
      check_enum_spec env
      (List.combine (state :: enums) (stinv :: especs))
  in

  let env, variables =
    let check_var_spec env (var, specs) =
      let xspecs =
        match var with
        | Some var ->
          List.pmap (fun { pldesc = ({ pldesc = xname }, xspec) } ->
              if   xname = unloc var.vr_name
              then Some xspec
              else None) g.gr_specvars
        | None -> [] in

      let specs = List.flatten (Option.get_as_list specs @ xspecs) in

      match specs with [] -> env, var | _ ->

        let env, spec = for_lbls_formula env specs in
        let spec = List.map (fun (label, term) ->
            A.{ label; term; error = None; loc = term.A.loc }
          ) spec in

        Option.foldmap (fun env var ->
            let var = { var with vr_invs = var.vr_invs @ spec } in
            (Env.Var.push env var, var)) env var

    in List.fold_left_map
      check_var_spec env
      (List.combine variables vspecs) in

  let () = for_var_specs env g.gr_specvars in

  let state = List.hd enums in
  let enums = List.tl enums in

  let env, specs     = for_specs_decl     env g.gr_specs     in
  let env, functions = for_funs_decl      env g.gr_funs      g.gr_specfuns in
  let env, acttxs    = for_acttxs_decl    env g.gr_acttxs    g.gr_specfuns in
  let ()             = for_fun_specs      env g.gr_specfuns  in
  let env, secspecs  = for_secs_decl      env g.gr_secs      in

  let output =
    { state    ; variables; enums   ; assets ; functions;
      acttxs   ; specs    ; secspecs; records; events; imports }

  in (env, output)

(* -------------------------------------------------------------------- *)
let enums_of_statedecl (enums : statedecl list) : A.enum list =
  let for1 tg =
    let for_ctor1 ((id, cty, invs) : ctordecl) =
      let invs = List.map (fun (label, inv) -> A.mk_label_term ?label inv) invs in

      A.{ name       = id;
          initial    = String.equal (unloc id) tg.sd_init;
          args       = cty;
          invariants = invs;
          loc        = Location.dummy; } in

    let items = List.map for_ctor1 tg.sd_ctors in
    let kind  =
      if tg.sd_state then A.EKstate else A.EKenum tg.sd_name in

    A.{ kind; items; loc = Location.dummy; }

  in List.map for1 enums

(* -------------------------------------------------------------------- *)
let assets_of_adecls adecls =
  let for1 (decl : assetdecl) =
    let for_field fd =
      A.{ name    = fd.fd_name;
          typ     = Some fd.fd_type;
          default = fd.fd_dfl;
          shadow  = fd.fd_ghost;
          loc     = loc fd.fd_name; } in

    let spec (l, f) =
      A.{ label = l; term = f; error = None; loc = f.loc } in

    A.{ name     = decl.as_name;
        fields   = List.map for_field decl.as_fields;
        keys     = decl.as_pk;
        sort     = decl.as_sortk;
        map_kind = decl.as_bm;
        state    = decl.as_state;
        init     = decl.as_init;
        specs    = List.map spec decl.as_invs;
        loc      = loc decl.as_name; }

  in List.map for1 (List.pmap (fun x -> x) adecls)

(* -------------------------------------------------------------------- *)
let records_of_rdecls rdecls =
  let for1 (decl : recorddecl) =
    let for_field fd =
      A.{ name    = fd.rfd_name;
          typ     = Some fd.rfd_type;
          default = fd.rfd_dfl;
          shadow  = false;
          loc     = loc fd.rfd_name; } in

    let packing =
      let rec doit = function
        | RLeaf id -> A.Pleaf id
        | RNode ch -> A.Pnode (List.map doit ch)
      in Option.map doit decl.rd_packing in

    A.{ name   = decl.rd_name;
        fields = List.map for_field decl.rd_fields;
        pos    = Option.get_dfl (Pnode []) packing;
        loc    = loc decl.rd_name; }

  in List.map for1 rdecls

(* -------------------------------------------------------------------- *)
let variables_of_vdecls fdecls =
  let for1 (decl : vardecl) =
    A.{ decl =
          A.{ name    = decl.vr_name;
              typ     = Some decl.vr_type;
              default = Option.fst decl.vr_def;
              shadow  = false;
              loc     = loc decl.vr_name; };
        kind     = (match decl.vr_kind with | `Constant -> VKconstant | `Variable -> VKvariable | _ -> VKvariable);
        invs     = decl.vr_invs;
        loc      = loc decl.vr_name; }

  in List.map for1 (List.pmap (fun x -> x) fdecls)

(* -------------------------------------------------------------------- *)
let imports_of_vdecls idecls : A.import_struct list =
  let for1 (decl : importdecl) : A.import_struct =
    A.{
      name = decl.id_name;
      path = decl.id_path;
      kind_node = INMichelson {ms_content = decl.id_content};
      views = decl.id_views;
      entrypoints = decl.id_entrypoints;
    }

  in List.map for1 idecls


(* -------------------------------------------------------------------- *)
let specifications_of_ispecifications =
  let env0 : A.lident A.specification = A.{
      predicates  = [];
      definitions = [];
      fails       = [];
      lemmas      = [];
      theorems    = [];
      variables   = [];
      invariants  = [];
      effect      = None;
      specs       = [];
      asserts     = [];
      loc         = L.dummy;      (* FIXME *) } in

  let do1 (env : A.lident A.specification) (ispec : env ispecification) =
    match ispec with
    | `Postcondition (x, e, invs, uses) ->
      let spec =
        let for_inv (lbl, inv) =
          A.{ label = lbl; formulas = inv }
        in
        A.{ name       = x;
            formula    = e;
            invariants = List.map for_inv invs;
            uses       = uses; }
      in { env with A.specs = env.specs @ [spec] }

    | `Asset (x, form, invs, uses) ->
      let asst =
        let for_inv (lbl, inv) =
          A.{ label = lbl; formulas = inv }
        in
        A.{ name       = x;
            label      = x;
            formula    = form;
            invariants = List.map for_inv invs;
            uses       = uses; }
      in { env with A.asserts = env.asserts @ [asst] }

    | `Variable (x, e) ->
      let var =
        A.mk_variable ~loc:(loc x)
          (A.mk_decl
             ~loc:(loc x) ?default:e
             ?typ:(Option.bind (fun e -> e.A.type_) e)
             x) VKvariable
      in { env with A.variables = env.variables @ [var] }

    | `Effect (_, i) ->
      assert (Option.is_none env.A.effect);
      { env with A.effect = Some i; }

    | `Predicate (defname, args, body) ->
      let def = A.mk_predicate ~loc:(loc defname) defname ~args body in
      { env with A.predicates = env.predicates @ [def] }

    | `Definition (defname, (x, xty), body) ->
      let def = A.mk_definition ~loc:(loc defname) defname xty x body in
      { env with A.definitions = env.definitions @ [def] }

    | `Fails fails ->
      let fails = List.map (fun (id, fid, arg, atype, f) -> A.mk_fail id fid arg atype f) fails in
      { env with A.fails = env.fails @ fails }

  in fun ispecs -> List.fold_left do1 env0 ispecs

(* -------------------------------------------------------------------- *)
let functions_of_fdecls fdecls =
  let for1 (decl : env fundecl) =
    let args = List.map (fun (x, ty) -> A.{
        name = x; typ = Some ty; default = None; shadow  = false; loc = loc x;
      }) decl.fs_args in

    let specs =
      if   List.is_empty decl.fs_spec
      then None
      else Some (specifications_of_ispecifications decl.fs_spec) in

    A.{ name          = decl.fs_name;
        kind          = decl.fs_kind;
        args          = args;
        body          = decl.fs_body;
        specification = specs;
        return        = decl.fs_retty;
        loc           = loc decl.fs_name; }

  in List.map for1 (List.pmap (fun x -> x) fdecls)

(* -------------------------------------------------------------------- *)
let transentrys_of_tdecls tdecls =
  let for_calledby ocb : (A.rexpr * A.pterm option) option =
    match fst ocb with [] -> None | c :: cb ->

      let for1 = fun (x : A.pterm option loced) ->
        let node =
          Option.get_dfl A.Rany (Option.map (
              fun (e : A.pterm) ->
                match e with
                | { node = Pvar (VTnone, Vnone, _); type_= Some (A.Tcontainer(Tasset an, Collection)) } -> A.Rasset an
                | _ -> A.Rexpr e) (unloc x))
        in A.mk_sp ~loc:(loc x) node in

      let aout = List.fold_left
          (fun acc c' ->  A.mk_sp (A.Ror (acc, for1 c')))
          (for1 c) cb
      in Some (aout, snd ocb)
  in

  let for1 tdecl =
    let mkl (x, c, e) =  A.{ label = x; term = c; error = e; loc = L.dummy; } in

    let transition =
      match tdecl.ad_effect with
      | Some (`Tx (from_, tgt, x)) ->
        let on =
          Option.map (fun (on, asset) ->
              let pkty = asset.as_pkty in
              let stty = A.Tenum (Option.get asset.as_state) in
              (on, pkty, asset.as_name, stty)
            ) tgt in
        let trs = List.map (fun tx -> (tx.tx_state, tx.tx_when, tx.tx_effect)) x in
        Some (A.{ from = from_; on; trs })

      | _ -> None in

    let effect =
      match tdecl.ad_effect with
      | Some (`Raw x) -> Some x | _ -> None in

    A.{ name = tdecl.ad_name;
        args =
          List.map (fun (x, xty) ->
              A.{ name = x; typ = Some xty; default = None; shadow  = false; loc = loc x; })
            tdecl.ad_args;
        sourcedby       = for_calledby tdecl.ad_srcby;
        calledby        = for_calledby tdecl.ad_callby;
        state_is        = tdecl.ad_stateis;
        accept_transfer = tdecl.ad_actfs;
        constants       = Some (List.map mkl tdecl.ad_csts);
        require         = Some (List.map mkl tdecl.ad_reqs);
        failif          = Some (List.map mkl tdecl.ad_fais);
        transition      = transition;
        specification   = Some (specifications_of_ispecifications tdecl.ad_spec);
        functions       = functions_of_fdecls tdecl.ad_funs;
        effect          = effect;
        loc             = loc tdecl.ad_name; }

  in List.map for1 (List.pmap id tdecls)

(* -------------------------------------------------------------------- *)
let for_parameters ?init env params =

  match params with
  | None -> env, []
  | Some params -> begin
      let env, ps =
        List.fold_left (fun (env, accu) p ->
            let pname, ptyp, pdv, const = unloc p in
            let ety = for_type env ptyp in
            let dv = Option.map (for_xexpr (expr_mode `Concrete) ?ety env) pdv in
            let typ =
              match ety with
              | Some x -> x
              | None -> (Env.emit_error env (loc p, InvalidTypeForParameter); assert false)
            in
            let decl = {
              vr_name = pname;
              vr_type = typ;
              vr_kind = if const then `Constant else `Variable;
              vr_core = None;
              vr_invs = [];
              vr_def  = None;
            } in
            let env = Env.Var.push env decl in
            let param : A.lident A.parameter = A.{
                name    = pname;
                typ     = typ;
                default = dv;
                value   = None;
                const   = const;
                loc     = loc p;
              } in
            env, (accu @ [param])
          ) (env, []) (unloc params)
      in
      match init with
      | None -> env, ps
      | _ -> begin
          let inits =
            match init with
            | None -> None
            | Some { pldesc = PT.Etuple l } when List.length ps > 1 -> Some l
            | Some x -> Some [x]
          in
          match inits with
          | None -> env, ps
          | Some inits -> begin
              let np = List.length ps in
              let ni = List.length inits in
              if np <> ni
              then Env.emit_error env (loc params, InvalidNumberOfParameters (np, ni));
              env, List.map2 (
                fun (param : A.lident A.parameter) (init : PT.expr) ->
                  let ety = Some param.typ in
                  let v = for_xexpr ?ety (expr_mode `Concrete) env init in
                  { param with value = Some v }
              ) ps inits
            end
        end
    end

let sort_decl refs l =
  let get_name = function
    | A.Dvariable x -> unloc x.decl.name
    | A.Denum {kind = EKenum id}  -> unloc id
    | A.Denum {kind = EKstate}  -> "_state"
    | A.Drecord x   -> unloc x.name
    | A.Dasset x    -> unloc x.name
    | A.Devent x    -> unloc x.name
  in
  if l |> List.map get_name |> List.exists (fun x -> not (List.mem x refs))
  then l
  else
    let cmp d1 d2 : int =
      let fidx d = get_name d |> fun x -> List.index_of (String.equal x) refs in
      let idx1 = fidx d1 in
      let idx2 = fidx d2 in
      idx1 - idx2
    in
    List.sort cmp l

(* -------------------------------------------------------------------- *)
let for_declarations ?init (env : env) (decls : (PT.declaration list) loced) : A.ast =
  let sorted_decl_ids = List.map (PT.get_name |@ unloc) (unloc decls) in
  (* Format.printf "[%a]@\n" (Printer_tools.pp_list "; " (fun fmt x -> Format.fprintf fmt "%s" x)) _sorted_decl_ids; *)
  let toploc = loc decls in

  match unloc decls with
  | { pldesc = Darchetype (x, params, metadata, _exts) } :: decls ->
    let groups = group_declarations decls in
    let env, parameters = for_parameters env params ?init in
    let metadata = Option.map (function | PT.Muri x -> A.MKuri x | PT.Mjson x -> A.MKjson x) metadata in
    let _env, decls = for_grouped_declarations env (toploc, groups) in

    A.mk_model
      ~parameters
      ~imports:(imports_of_vdecls decls.imports)
      ?metadata
      ~decls:((
          List.map (fun x -> A.Dvariable x) (variables_of_vdecls decls.variables)                            @
          List.map (fun x -> A.Denum x)     (enums_of_statedecl (List.pmap id (decls.state :: decls.enums))) @
          List.map (fun x -> A.Drecord x)   (records_of_rdecls (List.pmap id decls.records))                 @
          List.map (fun x -> A.Dasset x)    (assets_of_adecls decls.assets)                                  @
          List.map (fun x -> A.Devent x)    (records_of_rdecls (List.pmap id decls.events))
        ) |> sort_decl sorted_decl_ids)
      ~funs:(
        List.map (fun x -> A.Ffunction x)    (functions_of_fdecls decls.functions) @
        List.map (fun x -> A.Ftransaction x) (transentrys_of_tdecls decls.acttxs)
      )
      ~specifications:(List.map specifications_of_ispecifications decls.specs)
      ~securities:(decls.secspecs)
      ~loc:toploc
      x

  | _ ->
    Env.emit_error env (loc decls, InvalidArcheTypeDecl);
    { (A.mk_model (mkloc (loc decls) "<unknown>")) with loc = loc decls }

(* -------------------------------------------------------------------- *)
let typing ?init (env : env) (cmd : PT.archetype) =

  match unloc cmd with
  | Marchetype decls ->
    for_declarations env (mkloc (loc cmd) decls) ?init

  | Mextension _ ->
    assert false
