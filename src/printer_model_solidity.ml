open Location
open Tools
open Model
open Printer_tools

let pragma = ">=0.5.0 <0.7.0"

(* -------------------------------------------------------------------------- *)
let pp_model fmt (model : model) =

  let pp_pragma fmt _ =
    Format.fprintf fmt "pragma solidity %a;@\n" pp_str pragma
  in

  let pp_btyp fmt x =
    let pp = Format.fprintf fmt in
    match x with
    | Bbool       -> pp "bool"
    | Bint        -> pp "int"
    | Brational   -> pp "rational"
    | Bdate       -> pp "timestamp"
    | Bduration   -> pp "duration"
    | Btimestamp  -> pp "timestamp"
    | Bstring     -> pp "string"
    | Baddress    -> pp "address"
    | Brole       -> pp "address"
    | Bcurrency   -> pp "tez"
    | Bkey        -> pp "key"
  in

  let pp_container fmt = function
    | Collection -> Format.fprintf fmt "list"
    | Partition  -> Format.fprintf fmt "list"
    | List       -> Format.fprintf fmt "list"
  in

  let rec pp_type fmt t =
    match t with
    | Tasset an    -> Format.fprintf fmt "asset_%a" pp_id an
    | Tenum en     -> pp_id fmt en
    | Tcontract cn -> pp_id fmt cn
    | Tbuiltin b   -> pp_btyp fmt b
    | Tstate       -> pp_str fmt "states"
    | Tcontainer (t, c) ->
      Format.fprintf fmt "%a %a"
        pp_type t
        pp_container c
    | Toption t ->
      Format.fprintf fmt "%a option"
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
    | Tassoc (k, v) ->
      Format.fprintf fmt "(%a, %a) map"
        pp_btyp k
        pp_type v
    | Tunit -> pp_str fmt "unit"
    | Tstorage -> pp_str fmt "storage"
    | Toperation -> pp_str fmt "operation"
    | Tentry -> pp_str fmt "entry"
    | Tprog _
    | Tvset _
    | Ttrace _ -> pp_str fmt "todo"
  in

  let pp_storage_location fmt (t, v: type_ * mterm)  =
    let storage = "storage " in
    let memory = "memory " in
    let storage_location =
      match t, v.node with
      | Tasset _, Mget _   -> storage
      | Tasset _, Masset _ -> memory
      | _ -> ""
    in
    Format.fprintf fmt " %s" storage_location
  in

  let pp_mterm fmt (mt : mterm) =
    let rec f fmt (mtt : mterm) =
      match mtt.node with
      | Mif (c, t, None) ->
        Format.fprintf fmt "@[if (%a) {@\n  @[%a@]@\n}@]"
          f c
          f t
      | Mif (c, t, Some e) ->
        Format.fprintf fmt "@[if (%a) {@\n  @[%a@]@\n} else {@\n  @[%a@]@\n}@]"
          f c
          f t
          f e
      | Mmatchwith _  -> pp_str fmt "todo_Mmatchwith"
      | Mapp _        -> pp_str fmt "todo_Mapp"
      | Maddshallow _ -> pp_str fmt "todo_Maddshallow"
      | Mexternal _   -> pp_str fmt "todo_Mexternal"
      | Mget (c, k) ->
        let pp fmt (c, k) =
          Format.fprintf fmt "get_%a (%a)"
            pp_str c
            f k
        in
        pp fmt (c, k)

      | Mgetbefore _  -> pp_str fmt "todo_Mgetbefore"
      | Mgetat _      -> pp_str fmt "todo_Mgetat"
      | Mgetfrommap _ -> pp_str fmt "todo_Mgetfrommap"
      | Mset (c, _l, k, v) ->
        Format.fprintf fmt "set_%a (%a, %a);"
          pp_str c
          f k
          f v
      | Maddasset (an, i) ->
        Format.fprintf fmt "add_%a (%a);"
          pp_str an
          f i
      | Maddfield _    -> pp_str fmt "todo_Maddfield"
      | Mremoveasset (an, i) ->
        Format.fprintf fmt "remove_%a (%a);"
          pp_str an
          f i
      | Mremovefield _ -> pp_str fmt "todo_Mremovefield"
      | Mremoveif _    -> pp_str fmt "todo_Mremoveif"
      | Mupdate _      -> pp_str fmt "todo_Mupdate"
      | Mselect _      -> pp_str fmt "todo_Mselect"
      | Msort _        -> pp_str fmt "todo_Msort"
      | Mcontains (an, c, i) ->
        Format.fprintf fmt "contains_%a (%a, %a)"
          pp_str an
          f c
          f i
      | Mmem _          -> pp_str fmt "todo_Mmem"
      | Msubsetof _     -> pp_str fmt "todo_Msubsetof"
      | Mnth _          -> pp_str fmt "todo_Mnth"
      | Mcount _        -> pp_str fmt "todo_Mcount"
      | Msum _          -> pp_str fmt "todo_Msum"
      | Mmin _          -> pp_str fmt "todo_Mmin"
      | Mmax _          -> pp_str fmt "todo_Mmax"
      | Mfunmax _       -> pp_str fmt "todo_Mfunmax"
      | Mfunmin _       -> pp_str fmt "todo_Mfunmin"
      | Mfunabs _       -> pp_str fmt "todo_Mfunabs"
      | Mhead _         -> pp_str fmt "todo_Mhead"
      | Mtail _         -> pp_str fmt "todo_Mtail"
      | Mlistprepend _  -> pp_str fmt "todo_Mlistprepend"
      | Mlistcontains _ -> pp_str fmt "todo_Mlistcontains"
      | Mlistcount _    -> pp_str fmt "todo_Mlistcount"
      | Mlistnth _      -> pp_str fmt "todo_Mlistnth"
      | Mfail ft ->

        let pp_fail_type fmt = function
          | Invalid e -> f fmt e
          | InvalidCaller -> Format.fprintf fmt "invalid caller"
          | InvalidCondition c ->
            Format.fprintf fmt "require %afailed"
              (pp_option (pp_postfix " " pp_str)) c
          | NoTransfer -> Format.fprintf fmt "no transfer"
          | InvalidState -> Format.fprintf fmt "invalid state"
        in

        Format.fprintf fmt "revert (\"%a\");"
          pp_fail_type ft

      | Mand        _ -> pp_str fmt "todo_Mand"
      | Mor         _ -> pp_str fmt "todo_Mor"
      | Mimply      _ -> pp_str fmt "todo_Mimply"
      | Mequiv      _ -> pp_str fmt "todo_Mequiv"
      | Misempty    _ -> pp_str fmt "todo_Misempty"
      | Mnot c -> Format.fprintf fmt "!(%a)" f c
      | Mmulticomp  _ -> pp_str fmt "todo_Mmulticomp"
      | Mequal  (lhs, rhs) -> Format.fprintf fmt "%a == %a" f lhs f rhs
      | Mnequal (lhs, rhs) -> Format.fprintf fmt "%a != %a" f lhs f rhs
      | Mgt     (lhs, rhs) -> Format.fprintf fmt "%a > %a" f lhs f rhs
      | Mge     (lhs, rhs) -> Format.fprintf fmt "%a >= %a" f lhs f rhs
      | Mlt     (lhs, rhs) -> Format.fprintf fmt "%a < %a" f lhs f rhs
      | Mle     (lhs, rhs) -> Format.fprintf fmt "%a <= %a" f lhs f rhs
      | Mplus   (lhs, rhs) -> Format.fprintf fmt "%a + %a" f lhs f rhs
      | Mminus  (lhs, rhs) -> Format.fprintf fmt "%a - %a" f lhs f rhs
      | Mmult   (lhs, rhs) -> Format.fprintf fmt "%a * %a" f lhs f rhs
      | Mdiv    (lhs, rhs) -> Format.fprintf fmt "%a / %a" f lhs f rhs
      | Mdivrat     _ -> pp_str fmt "todo_Mdivrat"
      | Mmodulo (lhs, rhs) -> Format.fprintf fmt "%a %% %a" f lhs f rhs
      | Muplus v -> Format.fprintf fmt "+(%a)" f v
      | Muminus v -> Format.fprintf fmt "-(%a)" f v
      | Mrateq      _ -> pp_str fmt "todo_Mrateq"
      | Mratcmp     _ -> pp_str fmt "todo_Mratcmp"
      | Mratarith   _ -> pp_str fmt "todo_Mratarith"
      | Mrattez     _ -> pp_str fmt "todo_Mrattez"
      | Minttorat   _ -> pp_str fmt "todo_Minttorat"
      | Masset l ->
        let asset_name =
          match mtt.type_ with
          | Tasset asset_name -> asset_name
          | _ -> assert false
        in
        let a = Utils.get_asset model (unloc asset_name) in
        let ll = List.map (fun (x : asset_item) -> x.name) a.values in

        let lll = List.map2 (fun x y -> (x, y)) ll l in

        Format.fprintf fmt "%a({%a})"
          pp_id asset_name
          (pp_list ", " (fun fmt (a, b)->
               Format.fprintf fmt "%a: %a"
                 pp_id a
                 f b)) lll
      | Mletin (ids, a, t, b, _) ->
        Format.fprintf fmt "%a%a%a = %a;@\n@[%a@]"
          (pp_option pp_type) t
          pp_storage_location (Option.get t, a)
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          f a
          f b
      | Mdeclvar        _ -> pp_str fmt "todo_Mdeclvar"
      | Mvarstorevar v
      | Mvarstorecol v
      | Mvarenumval v
      | Mvarlocal v
      | Mvarparam v
      | Mvarfield v -> pp_id fmt v
      | Mvarthe           -> pp_str fmt "todo_Mvarthe"
      | Mvarstate         -> pp_str fmt "state"
      | Mnow              -> pp_str fmt "todo_Mnow"
      | Mtransferred      -> pp_str fmt "todo_Mtransferred"
      | Mcaller           -> pp_str fmt "msg.sender"
      | Mbalance          -> pp_str fmt "todo_Mbalance"
      | Msource           -> pp_str fmt "todo_Msource"
      | Mnone             -> pp_str fmt "todo_Mnone"
      | Msome           _ -> pp_str fmt "todo_Msome"
      | Marray          _ -> pp_str fmt "todo_Marray"
      | Mint            i -> pp_big_int fmt i
      | Muint           i -> pp_big_int fmt i
      | Mbool           v -> pp_str fmt (if v then "true" else "false")
      | Menum           _ -> pp_str fmt "todo_Menum"
      | Mrational       _ -> pp_str fmt "todo_Mrational"
      | Mstring       str -> Format.fprintf fmt "\"%s\"" str
      | Mcurrency       _ -> pp_str fmt "todo_Mcurrency"
      | Maddress        _ -> pp_str fmt "todo_Maddress"
      | Mdate           _ -> pp_str fmt "todo_Mdate"
      | Mduration       _ -> pp_str fmt "todo_Mduration"
      | Mtimestamp      _ -> pp_str fmt "todo_Mtimestamp"
      | Mdotasset (e, i)
      | Mdotcontract (e, i) ->
        Format.fprintf fmt "%a.%a"
          f e
          pp_id i
      | Mtuple          _ -> pp_str fmt "todo_Mtuple"
      | Massoc          _ -> pp_str fmt "todo_Massoc"
      | Mfor            _ -> pp_str fmt "todo_Mfor"
      | Miter           _ -> pp_str fmt "todo_Miter"
      | Mfold           _ -> pp_str fmt "todo_Mfold"
      | Mseq l -> (pp_list "@\n" f) fmt l
      | Massign         _ -> pp_str fmt "todo_Massign"
      | Massignvarstore _ -> pp_str fmt "todo_Massignvarstore"
      | Massignfield (op, _, lhs, fn, v) ->
        Format.fprintf fmt "%a.%a %s %a;"
          f lhs
          pp_id fn
          ( match op with
            | ValueAssign -> "="
            | PlusAssign  -> "+="
            | MinusAssign -> "-="
            | MultAssign  -> "*="
            | DivAssign   -> "/="
            | AndAssign   -> "&="
            | OrAssign    -> "|="
          )
          f v
      | Massignstate    _ -> pp_str fmt "todo_Massignstate"
      | Mtransfer       _ -> pp_str fmt "todo_Mtransfer"
      | Mbreak            -> pp_str fmt "break"
      | Massert         _ -> pp_str fmt "todo_Massert"
      | Mreturn         _ -> pp_str fmt "todo_Mreturn"
      | Mlabel          _ -> pp_str fmt "todo_Mlabel"
      | Mshallow        _ -> pp_str fmt "todo_Mshallow"
      | Munshallow      _ -> pp_str fmt "todo_Munshallow"
      | Mlisttocoll     _ -> pp_str fmt "todo_Mlisttocoll"
      | Mtokeys         _ -> pp_str fmt "todo_Mtokeys"
      | Mcoltokeys      _ -> pp_str fmt "todo_Mcoltokeys"
      | Mforall         _ -> pp_str fmt "todo_Mforall"
      | Mexists         _ -> pp_str fmt "todo_Mexists"
      | Msetbefore      _ -> pp_str fmt "todo_Msetbefore"
      | Msetat          _ -> pp_str fmt "todo_Msetat"
      | Msetunmoved     _ -> pp_str fmt "todo_Msetunmoved"
      | Msetadded       _ -> pp_str fmt "todo_Msetadded"
      | Msetremoved     _ -> pp_str fmt "todo_Msetremoved"
      | Msetiterated    _ -> pp_str fmt "todo_Msetiterated"
      | Msettoiterate   _ -> pp_str fmt "todo_Msettoiterate"

    in
    f fmt mt
  in

  let pp_var fmt (var : var) =
    Format.fprintf fmt "%a %a%a;"
      pp_type var.type_
      pp_id var.name
      (pp_option (fun fmt x -> Format.fprintf fmt " = %a" pp_mterm x)) var.default
  in

  let pp_struct_asset fmt (asset : asset) =
    let pp_asset_item (fmt : Format.formatter) (asset_item : asset_item) =
      Format.fprintf fmt
        "%a %a;"
        pp_type asset_item.type_
        pp_id asset_item.name
    in
    Format.fprintf fmt
      "struct %a {@\n  @[%a@]@\n}@\n"
      pp_type (Tasset asset.name)
      (pp_list "@\n" pp_asset_item) asset.values;
  in

  let pp_decl_asset fmt (asset : asset) =
    let an = unloc asset.name in
    let _k, t = Utils.get_asset_key model an in
    Format.fprintf fmt "mapping(%a => %a) public %s;"
      pp_btyp t
      pp_type (Tasset asset.name)
      an
  in

  let pp_asset fmt (asset : asset) =
    pp_struct_asset fmt asset;
    pp_decl_asset fmt asset
  in

  let pp_decl_node fmt = function
    | Dvar v      -> pp_var fmt v
    | Denum _     -> pp_str fmt "todo_enum"
    | Dasset a    -> pp_asset fmt a
    | Dcontract _ -> pp_str fmt "todo_contract"
  in

  let pp_decls fmt x = (pp_list "@\n" pp_decl_node) fmt x in

  let pp_api_asset fmt = function
    | Get an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function get_%a(%a k) view returns(%a %a) {@\n  \
         return %a[k];@\n\
         }@\n"
        pp_str an pp_btyp t pp_btyp t pp_str an
        pp_str an
    | Set an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function set_%a(%a k, %a asset) {@\n  \
         %a[k] = asset;@\n\
         }@\n"
        pp_str an pp_btyp t pp_str an
        pp_str an
    | Add an ->
      let k, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function add_%a(%a asset) {@\n  \
         %a key = asset.%a;@\n  \
         if (%a[key].isValue) {@\n    \
         revert(\"key already exists\");@\n  \
         }@\n  \
         %a[key] = asset;@\n\
         }@\n"
        pp_str an pp_str an
        pp_btyp t pp_str k
        pp_str an
        pp_str an
    | Remove        _an         -> pp_str fmt "todo_Remove"
    | UpdateAdd    (_an, _fn)   -> pp_str fmt "todo_UpdateAdd"
    | UpdateRemove (_an, _fn)   -> pp_str fmt "todo_UpdateRemove"
    | ToKeys        _an         -> pp_str fmt "todo_ToKeys"
    | ColToKeys     _an         -> pp_str fmt "todo_ColToKeys"
    | Select       (_an, _pred) -> pp_str fmt "todo_Select"
    | Sort         (_an, _v)    -> pp_str fmt "todo_Sort"
    | Contains an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function contains_%a(%a key) returns(bool res) {@\n  \
         return (%a[key].isValue);@\n\
         }@\n"
        pp_str an pp_btyp t
        pp_str an
    | Nth           _an         -> pp_str fmt "todo_Nth"
    | Count         _an         -> pp_str fmt "todo_Count"
    | Sum          (_an, _fn)   -> pp_str fmt "todo_Sum"
    | Min          (_an, _fn)   -> pp_str fmt "todo_Min"
    | Max          (_an, _fn)   -> pp_str fmt "todo_Max"
    | Shallow       _an         -> pp_str fmt "todo_Shallow"
    | Unshallow     _an         -> pp_str fmt "todo_Unshallow"
    | Listtocoll    _an         -> pp_str fmt "todo_Listtocoll"
    | Head          _an         -> pp_str fmt "todo_Head"
    | Tail          _an         -> pp_str fmt "todo_Tail"
  in

  let pp_api_list fmt = function
    | Lprepend  _t -> pp_str fmt "todo_Lprepend"
    | Lcontains _t -> pp_str fmt "todo_Lcontains"
    | Lcount    _t -> pp_str fmt "todo_Lcount"
    | Lnth      _t -> pp_str fmt "todo_Lnth"
  in

  let pp_api_builtin fmt = function
    | MinBuiltin _t -> pp_str fmt "todo_MinBuiltin"
    | MaxBuiltin _t -> pp_str fmt "todo_MaxBuiltin"
  in

  let pp_api_internal fmt = function
    | RatEq      -> pp_str fmt "todo_RatEq"
    | RatCmp     -> pp_str fmt "todo_RatCmp"
    | RatArith   -> pp_str fmt "todo_RatArith"
    | RatTez     -> pp_str fmt "todo_RatTez"
  in

  let pp_api_item_node fmt = function
    | APIAsset      v -> pp_api_asset    fmt v
    | APIList       v -> pp_api_list     fmt v
    | APIBuiltin    v -> pp_api_builtin  fmt v
    | APIInternal   v -> pp_api_internal fmt v
  in

  let pp_api_item fmt (api_storage : api_storage) =
    pp_api_item_node fmt api_storage.node_item
  in

  let pp_api_items fmt _ =
    let filter_api_items l : api_storage list =
      List.fold_right (fun (x : api_storage) accu ->
          if x.only_formula
          then accu
          else x::accu
        ) l []
    in
    let l : api_storage list = filter_api_items model.api_items in
    match l with
    | [] -> ()
    | _ ->
      begin
        Format.fprintf fmt "/* API functions */@\n";
        (pp_list "@\n" pp_api_item) fmt l
      end
  in

  let pp_arg fmt (a, t, _) =
    Format.fprintf fmt "%a %a"
      pp_type t
      pp_id a
  in

  let pp_args fmt l = (pp_list ", " pp_arg) fmt l in

  let pp_entry fmt (fs : function_struct) =
    Format.fprintf fmt
      "function %a(%a) public {@\n  @[%a@]@\n}@\n"
      pp_id fs.name
      pp_args fs.args
      pp_mterm fs.body
  in

  let pp_function_node fmt = function
    | Entry fs -> pp_entry fmt fs
    | Function (_fs, _ret) -> pp_str fmt "todo_function_node_function"
  in

  let pp_function fmt f =
    pp_function_node fmt f.node
  in

  let pp_functions fmt x = (pp_list "@\n" pp_function) fmt x in

  let pp_contract_content _fmt _ =
    pp_decls fmt model.decls;
    pp_newline fmt ();
    pp_newline fmt ();
    pp_api_items fmt model.api_items;
    pp_newline fmt ();
    pp_newline fmt ();
    pp_functions fmt model.functions
  in

  let pp_contract fmt _ =
    Format.fprintf fmt "contract %a {@\n  @[%a@]@\n}@."
      pp_id model.name
      pp_contract_content ()
  in

  Format.fprintf fmt "// Solidity output generated by %a@."
    pp_bin ();
  pp_newline fmt ();
  pp_pragma fmt ();
  pp_newline fmt ();
  pp_contract fmt ()


(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
