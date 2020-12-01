open Archetype
open Tools
open Location
open ParseTree

type status =
  | Passed
  | Error
[@@deriving yojson, show {with_path = false}]

type service =
  | GetProperties
[@@deriving yojson, show {with_path = false}]

let service = ref GetProperties

type position = {
  line : int;
  col : int;
  char : int;
}
[@@deriving yojson, show {with_path = false}]

type location = {
  start: position;
  end_: position [@key "end"];
}
[@@deriving yojson, show {with_path = false}]

type kind =
  | StorageInvariant
  | Fails
  | Assert
  | PostCondition
  | SecurityPredicate
[@@deriving yojson, show {with_path = false}]

type invariant = {
  label : string;
  formulas: string list;
}
[@@deriving yojson, show {with_path = false}]

type property = {
  kind : kind;
  id : string;
  formula: string;
  invariants: invariant list;
  location : location;
}
[@@deriving yojson, show {with_path = false}]

type 'a result = {
  status : status;
  obj: 'a;
}
[@@deriving yojson, show {with_path = false}]

type result_get_property = property list result
[@@deriving yojson, show {with_path = false}]

let mk_position (line, col) char : position = {
  line = line - 1;
  col = col;
  char = char;
}

let mk_location (loc : Location.t) : location = {
  start = mk_position loc.loc_start loc.loc_bchar;
  end_  = mk_position loc.loc_end loc.loc_echar;
}

let mk_invariant label formulas =
  { label; formulas }

let mk_property ?(invariants=[]) kind id formula location =
  { kind; id; formula; invariants; location }

let mk_property_get_property status obj : result_get_property =
  { status; obj }

let extract_properties (pt : archetype) : property list =
  let ep_specification_item (spi : specification_item) : property list =
    let f (label, formula, invs) m =
      let formula = Format.asprintf "%a" Printer_pt.pp_simple_expr formula in
      let location = mk_location (loc spi) in
      let invariants = List.map (fun (x, y : lident * expr list) ->
          let formulas = List.map (Format.asprintf "%a" Printer_pt.pp_simple_expr) y in
          mk_invariant (unloc x) formulas) invs in
      [mk_property ~invariants:invariants m (unloc label) formula location]
    in
    match unloc spi with
    | Vfails l ->
      List.map (fun (label, _, _, formula : lident * lident * type_t * expr) ->
          let formula = Format.asprintf "%a" Printer_pt.pp_simple_expr formula in
          mk_property ~invariants:[] Fails (unloc label) formula (mk_location (loc label))) l
    | Vassert (label, formula, invs, _) ->
      f (label, formula, invs) Assert
    | Vpostcondition (label, formula, invs, _, _) ->
      f (label, formula, invs) PostCondition
    | _ -> []
  in

  let ep_security_item (si : security_item) : property =
    let label, id, args = unloc si in
    let formula = Format.asprintf "%a (%a)" Printer_tools.pp_id id (Printer_tools.pp_list ", " Printer_pt.pp_security_arg) args in
    let location = mk_location (loc si) in
    mk_property SecurityPredicate (unloc label) formula location
  in

  let ep_specification sp =
    sp
    |> unloc
    |> fst
    |> List.map ep_specification_item
    |> List.flatten
  in

  let ep_decl = function
    | Dasset (_, _, _, _, a, _, _) ->
      begin
        a
        |> List.fold_left (fun accu x ->
            match x with
            | APOconstraints l -> l @ accu
            | _ -> accu
          ) []
        |> List.map (fun (x : label_expr) ->
            let loca, (label, formula) = Location.deloc x in
            let formula = Format.asprintf "%a" Printer_pt.pp_simple_expr formula in
            let location = mk_location (loca) in

            mk_property StorageInvariant (unloc label) formula location
          )
      end

    | Dfunction f ->
      begin
        match f.spec with
        | Some sp -> ep_specification sp
        | _ -> []
      end

    | Dentry (_, _, ap, _, _) ->
      begin
        match ap.spec_fun with
        | Some sp -> ep_specification sp
        | _ -> []
      end

    | Dspecification sp ->
      ep_specification sp

    | Dspecfun (_, _, _, sp) ->
      ep_specification sp

    | Dsecurity sec ->
      sec
      |> unloc
      |> fst
      |> List.map ep_security_item
    | _ -> []
  in

  match unloc pt with
  | Marchetype decls -> List.map (fun x -> x |> unloc |> ep_decl) decls |> List.flatten
  | _ -> []

let print_json f p =
  Format.printf "%s@\n"
    (Yojson.Safe.to_string (f p))


let process (filename, channel) =
  match !service with
  | GetProperties ->
    let print_error () =
      let e = mk_property_get_property Error [] in
      print_json result_get_property_to_yojson e
    in

    try
      let pt = Io.parse_archetype_strict ~name:filename channel in
      match !Error.errors with
      | [] ->
        let properties = extract_properties pt in
        let res = mk_property_get_property Passed properties in
        print_json result_get_property_to_yojson res
      | _ -> print_error ()
    with
      _ -> print_error ()
