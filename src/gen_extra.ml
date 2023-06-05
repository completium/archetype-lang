open Location
open Ident
open Tools
open Core

module PT = ParseTree
module T  = Michelson

exception TypeEntrypointNotFound of string
type error_desc =
  | TypeNotCompatible of T.type_ * PT.expr

let pp_error_desc fmt = function
  | TypeNotCompatible (t, e) ->
    Format.fprintf fmt
      "Type '%a' is not compatible with expression: '%a'."
      Printer_michelson.pp_type t Printer_pt.pp_simple_expr e

type error = Location.t * error_desc

let emit_error (lc, error : Location.t * error_desc) code =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ());
  raise (Error.Stop code)

let string_to_ttype ?entrypoint (input : string) : T.type_ =

  let typ =
    input
    |> Lexing.from_string
    |> Michelson_parser.main Michelson_lexer.token
    |> T.to_type
  in
  match entrypoint with
  | Some e -> begin
      let rec aux accu (t : T.type_) =
        match t.node with
        | _ when Option.map_dfl (String.equal e) false t.annotation -> Some t
        | T.Tor (l, r) -> aux (aux accu l) r
        | _ -> accu
      in
      let t = aux None typ in
      match t with
      | Some v -> v
      | None -> raise (TypeEntrypointNotFound e)
    end
  | _ -> typ


let to_model_expr (e : PT.expr) : T.data =

  let rec f ?typ (e : PT.expr) : T.data =

    let check_compatibility e et tref =
      Option.iter (fun x -> if not (List.exists (T.cmp_type x) tref) then emit_error (loc e, TypeNotCompatible (x, e)) 3) et
    in

    let cc tref = check_compatibility e typ tref in

    let error_cc t = emit_error (loc e, TypeNotCompatible (t, e)) 3 in

    let to_one ?typ (l : PT.expr list) =
      match List.rev l with
      | [] -> T.Dunit
      | i::q ->
        let t, q =
          match typ with
          | Some x -> begin
              let rec aux accu (t : T.type_) =
                match t.node with
                | T.Tpair [tl; tr] -> aux (accu @ [tl]) tr
                | _ -> accu @ [t]
              in
              match List.rev (aux [] x) with
              | hd::tl -> begin
                  Some hd,
                  if List.length q <> List.length tl
                  then error_cc x
                  else List.map2 (fun x y -> x, Some y) q tl
                end
              | _ -> error_cc x
            end
          | None -> None, List.map (fun x -> x, None) q
        in

        List.fold_left (fun accu (x, typ) ->
            T.Dpair [f ?typ x; accu]
          ) (f ?typ:t i) q
    in

    match unloc e with
    | Eliteral (Lint      n)
    | Eliteral (Lnat      n) -> begin
        cc [T.tnat; T.tint];
        Dint n
      end
    | Eliteral (Ldecimal  s) -> begin
        cc [T.tpair [T.tint; T.tnat]];
        let n, d = Core.decimal_string_to_rational s in Dpair [Dint n; Dint d]
      end
    | Eliteral (Ltz       n) -> cc [T.tmutez]; Dint (string_to_big_int_tz Ktz  n)
    | Eliteral (Lmtz      n) -> cc [T.tmutez]; Dint (string_to_big_int_tz Kmtz n)
    | Eliteral (Lutz      n) -> cc [T.tmutez]; Dint (string_to_big_int_tz Kutz n)
    | Eliteral (Laddress  s) -> cc [T.taddress]; Dstring s
    | Eliteral (Lstring   s) -> cc [T.tstring]; Dstring s
    | Eliteral (Lbool  true) -> cc [T.tbool]; Dtrue
    | Eliteral (Lbool false) -> cc [T.tbool]; Dfalse
    | Eliteral (Lduration s) -> cc [T.tint; T.ttimestamp]; Dint (s |> Core.string_to_duration |> Core.duration_to_timestamp)
    | Eliteral (Ldate     s) -> cc [T.ttimestamp]; Dint (s |> Core.string_to_date |> Core.date_to_timestamp)
    | Eliteral (Lbytes    s) -> cc [T.tbytes]; Dbytes s
    | Eliteral (Lpercent  n) -> cc [T.tpair [T.tint; T.tnat]]; let n, d = string_to_big_int_percent n in Dpair [Dint n; Dint d]
    | Eunit
    | Enothing               -> cc [T.tunit]; Dunit
    | Earray         (_scope, l)  -> begin
        let ll =
          match typ with
          | Some ({node = (T.Tset t | T.Tlist t); annotation = _}) -> List.map (f ~typ:t) l
          | Some ({node = (T.Tmap (tk, tv) | T.Tbig_map (tk, tv)); annotation = _} as tm) -> begin
              List.map ( fun (x : PT.expr) ->
                  match unloc x with
                  | Etuple [a; b] -> T.Delt (f ~typ:tk a, f ~typ:tv b)
                  | _ -> error_cc tm) l
            end
          | Some t -> error_cc t
          | None -> List.map f l
        in
        Dlist ll
      end
    | Etuple l -> to_one ?typ l
    | Eoption o -> begin
        let g =
          match typ with
          | Some ({node = T.Toption t; annotation = _}) -> f ~typ:t
          | Some t -> error_cc t
          | None -> f ?typ:None
        in
        match o with
        | OSome x -> Dsome (g x)
        | ONone _ -> Dnone
      end
    | Eor o -> begin
        let g =
          match typ with
          | Some ({node = T.Tor (tl, tr); annotation = _}) -> (match o with | Oleft _ -> f ~typ:tl | Oright _ -> f ~typ:tr)
          | Some t -> error_cc t
          | None -> f ?typ:None
        in
        match o with
        | Oleft  (_, _, x) -> Dleft  (g x)
        | Oright (_, _, x) -> Dright (g x)
      end
    (* | Elambda        _       -> assert false *)
    | _ -> assert false
  in
  let entrypoint = !Options.opt_entrypoint in
  let typ = Option.map (string_to_ttype ?entrypoint) !Options.opt_type in

  f ?typ e

let extract_from_micheline tag input =
  let seek i l : T.obj_micheline = List.find T.(function | Oprim ({prim = p; _}) -> String.equal i p | _ -> false) l in
  let get_arg = function | T.Oprim ({args=x::_; _}) -> x | _ -> assert false in

  input
  |> (function | T.Oarray l -> l | _ -> assert false)
  |> seek tag
  |> get_arg

type eargs = {
  id: string;
  typ: string;
}
[@@deriving yojson, show {with_path = false}]

type entry = {
  name: string;
  args : eargs list;
  typ: string;
}
[@@deriving yojson, show {with_path = false}]

type entries = entry list
[@@deriving yojson, show {with_path = false}]

let show_entries (input : T.obj_micheline) =
  let with_annot (t : T.type_) : bool = Option.is_some t.annotation in

  let for_type (t : T.type_) : Model.type_ = Gen_decompile.ttype_to_mtype t in

  let do_or (typ : T.type_) =
    let rec aux accu (t : T.type_) =
      match t.node with
      | _ when with_annot t -> accu @ [(Option.get t.annotation, t)]
      | T.Tor (l, r) -> aux (aux accu l) r
      | _ -> accu @ ["default", t]
    in
    aux [] typ
  in

  let do_pair (typ : T.type_) : (ident * Model.type_) list =
    let ft = for_type in
    let rec aux accu (t : T.type_) =
      match t.node with
      | _ when with_annot t -> accu @ [(Option.get t.annotation, ft t)]
      | T.Tpair l -> List.fold_left aux accu l
      | _ -> accu @ ["_", ft t]
    in
    aux [] {typ with annotation = None}
  in

  let seek i l : T.obj_micheline = List.find T.(function | Oprim ({prim = p; _}) -> String.equal i p | _ -> false) l in
  let get_arg = function | T.Oprim ({args=x::_; _}) -> x | _ -> assert false in

  input
  |> (function | Oarray l -> l | _ -> assert false)
  |> seek "parameter"
  |> get_arg
  |> T.to_type
  |> do_or
  |> List.map (fun (x, y) -> (x, y, do_pair y))
  |> fun (l : (ident * T.type_ * ((ident * Model.type_) list)) list) ->
  if !Options.opt_rjson
  then
    let res : entries = List.fold_right (fun (id, t, args) accu ->
        {name = id;
         typ = Format.asprintf "%a" Printer_michelson.pp_type t;
         args = (List.fold_right (fun (id, typ) accu -> {id = id; typ = Format.asprintf "%a" Printer_model.pp_type typ}::accu) args [])}::accu) l [] in
    (Yojson.Safe.to_string (entries_to_yojson res))
  else begin
    Format.asprintf "%a@."
      (Printer_tools.pp_list "@\n"
         (fun fmt (id, _, l : ident * T.type_ * (ident * Model.type_) list) ->
            Format.fprintf fmt "%s (%a)" id
              (Printer_tools.pp_list ", " (fun fmt (id, t) ->
                   Format.fprintf fmt "%s : %a" id Printer_model.pp_type t
                 )
              ) l
         )
      ) l
  end

let to_micheline (input : string) =
  let tokens = Lexing.from_string input in
  let m = Michelson_parser.main Michelson_lexer.token tokens in
  Format.printf "%a@." (Printer_michelson.pp_obj_micheline ~var_dynamic:false) m


type storage_value = {
  id: string;
  value: string;
}
[@@deriving yojson, show {with_path = false}]

type storage_values = storage_value list
[@@deriving yojson, show {with_path = false}]

let replace_code (_model : Model.model) (data : T.data) : T.data =
  Gen_michelson.process_data data

let get_storage_values (model : Model.model) =
  let ir = Gen_michelson.to_ir model in
  let storage_values : storage_values = List.map (fun (id, _, v) -> {id = id; value = Format.asprintf "%a" Printer_michelson.pp_data (replace_code model v)}) ir.storage_list in
  Yojson.Safe.to_string (storage_values_to_yojson storage_values)

let generate_contract_metadata ?(only_views=false) (model : Model.model) offchain_views =
  (match model.metadata with | Some MKjson v when not only_views -> unloc v | _ -> "{}")
  |> String.trim
  |> fun input ->
  (match offchain_views with
   | [] -> input
   | _ -> begin
       let offchain_views_value = Format.asprintf "\"views\":[%a]@." (Printer_tools.pp_list "," Printer_michelson.pp_offchain_view) offchain_views in
       let sep = match input with | "{}" -> "" | _ -> "," in
       (String.sub input 0 (String.length input - 1)) ^ sep ^ offchain_views_value ^ "}"
     end)
  |> Tools.minify_json
