open Location
open Tools
open Model
open Printer_tools
open Ident

exception Anomaly of string

type error_desc =
  | UnsupportedBreak
  | UnsupportedTerm of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

type operator =
  | Equal
  | Nequal
  | Lt
  | Le
  | Gt
  | Ge
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo

type position =
  | Lhs
  | Rhs

let pp_cast (pos : position) (ltype : type_) (rtype : type_) (pp : 'a -> mterm -> unit) (fmt : Format.formatter) =
  match pos, ltype, rtype with
  | Lhs, Tbuiltin Brole, Tbuiltin Baddress ->
    Format.fprintf fmt "(%a : address)" pp
  | Rhs, Tbuiltin Baddress, Tbuiltin Brole ->
    Format.fprintf fmt "(%a : address)" pp
  | _ -> pp fmt

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let to_lident = dumloc

let pp_nothing (fmt : Format.formatter) = ()

type action = {
  name : ident;
  fun_name: ident;
  args: (ident * type_) list
}

module LigoUtils : sig
  val get_actions : model -> action list
end = struct
  let get_actions (model : model) : action list =
    List.fold_right (fun (f : function__) accu ->
        match f.node with
        | Entry fs ->
          let fun_name = fs.name |> unloc in
          let id = fs.name |> unloc |> String.up_firstcase in
          let args = ["a", Tunit] in
          let action = {
            name = id;
            fun_name = fun_name;
            args = args;
          } in
          action::accu
        | _ -> accu)
      model.functions []
end

let pp_model fmt (model : model) =

  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "// contract: %a@\n"
      pp_id model.name
  in

  let pp_currency fmt = function
    | Tez   -> Format.fprintf fmt "tz"
    | Mutez -> Format.fprintf fmt "mtz"
  in

  let pp_btyp fmt = function
    | Bbool       -> Format.fprintf fmt "bool"
    | Bint        -> Format.fprintf fmt "int"
    | Brational   -> Format.fprintf fmt "rational"
    | Bdate       -> Format.fprintf fmt "timestamp"
    | Bduration   -> Format.fprintf fmt "duration"
    | Bstring     -> Format.fprintf fmt "string"
    | Baddress    -> Format.fprintf fmt "address"
    | Brole       -> Format.fprintf fmt "address"
    | Bcurrency c -> pp_currency fmt c
    | Bkey        -> Format.fprintf fmt "key"
  in

  let pp_container fmt = function
    | Collection -> Format.fprintf fmt "list"
    | Partition  -> Format.fprintf fmt "list"
    | List       -> Format.fprintf fmt "list"
  in

  let rec pp_type fmt t =
    match t with
    | Tasset an ->
      Format.fprintf fmt "%a" pp_id an
    | Tenum en ->
      Format.fprintf fmt "%a" pp_id en
    | Tcontract cn ->
      Format.fprintf fmt "%a" pp_id cn
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (t, c) ->
      Format.fprintf fmt "%a(%a)"
        pp_container c
        pp_type t
    | Toption t ->
      Format.fprintf fmt "%a option"
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
    | Tassoc (k, v) ->
      Format.fprintf fmt "map(%a, %a)"
        pp_btyp k
        pp_type v
    | Tunit ->
      Format.fprintf fmt "unit"
    | Tstorage ->
      Format.fprintf fmt "storage"
    | Toperation ->
      Format.fprintf fmt "operation"
    | Tentry ->
      Format.fprintf fmt "entry"
    | Tprog _
    | Tvset _
    | Ttrace _ -> Format.fprintf fmt "todo"
  in

  let pp_action_type (fmt : Format.formatter) _ =
    let actions = LigoUtils.get_actions model in
    Format.fprintf fmt
      "type action is@\n  \
       @[%a@]@\n"
      (pp_list "@\n" (fun fmt action ->
           Format.fprintf fmt "| %s of %a"
             action.name
             pp_type (List.nth action.args 0 |> snd))) actions
  in

  let pp_record_item (fmt : Format.formatter) (record_item : record_item) =
    Format.fprintf fmt
      "%a : %a;"
      pp_id record_item.name
      pp_type record_item.type_
  in

  let pp_record (fmt : Format.formatter) (record : record) =
    Format.fprintf fmt
      "type %a is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      pp_id record.name
      (pp_list "@\n" pp_record_item) record.values
  in

  let pp_decl (fmt : Format.formatter) (decl : decl_node) =
    match decl with
    | Denum e -> ()
    | Drecord r -> pp_record fmt r
    | Dcontract c -> ()
  in

  let pp_decls (fmt : Format.formatter) _ =
    (pp_list "@\n" pp_decl) fmt model.decls
  in

  let pp_storage_item (fmt : Format.formatter) (si : storage_item) =
    Format.fprintf fmt
      "%a : %a;"
      pp_id si.name
      pp_type si.typ
  in

  let pp_storage (fmt : Format.formatter) _ =
    Format.fprintf fmt
      "type storage_type is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      (pp_list "@\n" pp_storage_item) model.storage
  in

  let pp_function (fmt : Format.formatter) (f : function__) =
    match f.node with
    | Entry fs ->
      let name = fs.name in
      Format.fprintf fmt
        "function %a(const action : unit; const s : storage_type) : (list(operation) * storage_type) is@\n  \
         block {skip} with@\n  \
         ((nil : list(operation)), s)@\n"
        pp_id name
    | Function _ -> ()
  in

  let pp_functions (fmt : Format.formatter) _ =
    (pp_list "@\n" pp_function) fmt model.functions
  in

  let pp_main_function (fmt : Format.formatter) _ =
    let actions = LigoUtils.get_actions model in
    Format.fprintf fmt
      "function main(const action : action ; const s : storage_type) : (list(operation) * storage_type) is@\n  \
       block {skip} with@\n  \
       case action of@\n  \
       @[%a@]@\n  \
       end@\n"
      (pp_list "@\n"
         (fun fmt action -> Format.fprintf fmt "| %s (%a) -> %s(a, s)"
             action.name
             (pp_list "@ " pp_str) (List.map fst action.args)
             action.fun_name
         )) actions
  in

  Format.fprintf fmt "// LIGO output generated by archetype@\n@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      @."
    pp_model_name ()
    pp_action_type ()
    pp_decls ()
    pp_storage ()
    pp_functions ()
    pp_main_function ()

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
