(* -------------------------------------------------------------------------- *)
open Ident
open Tools
open Location
open ParseTree
open Printer_tools

let cmp_lident i1 i2 : bool = String.equal (unloc i1) (unloc i2)

let pp_literal fmt (l : literal) =
  match l with
  | Lnumber   n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Ldecimal  n -> Format.fprintf fmt "%s" n
  | Ltz       n -> Format.fprintf fmt "%stz"   (Big_int.string_of_big_int n)
  | Lmtz      n -> Format.fprintf fmt "%smtz"  (Big_int.string_of_big_int n)
  | Lutz      n -> Format.fprintf fmt "%sutz"  (Big_int.string_of_big_int n)
  | Laddress  a -> Format.fprintf fmt "%s" a
  | Lstring   s -> Format.fprintf fmt "\"%s\"" s
  | Lbool     b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lduration d -> Format.fprintf fmt "%s" d
  | Ldate     d -> Format.fprintf fmt "%s" d
  | Lbytes    s -> Format.fprintf fmt "0x%s" s

let pp_expr fmt e =
  match unloc e with
  | Eliteral l -> pp_literal fmt l
  | _ -> Printer_pt.pp_simple_expr fmt e
let pp_archetype fmt pt =

  let pp_marchetype fmt es =
    let decls =  List.map unloc es in

    let pp_title fmt _ =
      let name = List.fold_left (fun accu -> function | Darchetype (x, _) -> unloc x | _ -> accu) "" decls in
      Format.fprintf fmt "# %a@\n\
                          > Genrated with [Archetype](%a) v%a@\n@\n" (* date ? *)
        pp_str name
        pp_str Options.url
        pp_str Options.version
    in

    let pp_extension fmt (e : extension_unloc) =
      match e with
      | Eextension (id, _) -> pp_id fmt id
    in

    let pp_roles fmt _ =
      let pp_variable_decl fmt (id, _type_, dv, _vos, variable_kind, _invs, exts : variable_decl) =
        Format.fprintf fmt
          "### %a@\n@\n\
           | Attribute              | Value  |@\n\
           |----------------|---|---|@\n\
           | Kind           | %a  |@\n\
           | Address        | %a  |@\n\
           %a@\n"
          pp_id id
          (fun fmt ->
             function
             | VKvariable -> pp_str fmt "variable"
             | VKconstant -> pp_str fmt "constant") variable_kind
          (pp_option pp_expr) dv
          (fun fmt xs ->
             if Option.is_none xs
             then ()
             else
               let xs = xs |> Option.get |> List.map unloc in
               Format.fprintf fmt "| Extension      |  %a |@\n"
                 (pp_list ", " pp_extension) xs
          ) exts
      in
      let roles : variable_decl list =
        List.fold_right (fun x accu -> x |> unloc |> function | Dvariable ((_, {pldesc = Tref {pldesc = "role"}; _}, _, _, _, _, _) as a) -> a::accu | _ -> accu) es []
      in
      match roles with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "## Roles@\n@\n%a@\n"
          (pp_list "@\n" pp_variable_decl) roles
    in

    let pp_assets fmt _ =
      let pp_asset_decl fmt ((name, fields, shadow_fields, aopts, _ , _, _) : asset_decl) =
        let key : lident option = List.fold_left (fun accu x -> match x with AOidentifiedby k -> Some (k) | _ -> accu) None aopts in
        let orders : lident list = List.fold_left (fun accu x -> match x with AOsortedby s -> s::accu | _ -> accu) [] aopts in
        let fields = fields @ shadow_fields |> List.map unloc in
        let pp_asset_field fmt (f : field_unloc) =
          match f with
          | Ffield (id, type_, _dv, _exts) ->
            let attributes : ident list =
              []
              |>
              (fun l ->
                 if List.exists (cmp_lident id) orders
                 then "order"::l
                 else l)
              |> (fun l ->
                  if Option.cmp cmp_lident key (Some id)
                  then "__key__"::l
                  else l)
            in
            Format.fprintf fmt "| %a |  | %a | %a"
              pp_id id
              Printer_pt.pp_type type_
              (pp_list ", " pp_str) attributes
        in
        Format.fprintf fmt "### %a@\n@\n\
                            | Field | Desc | Type | Attribute |@\n\
                            |--|--|--|--|@\n\
                            %a@\n@\n"
          pp_id name
          (pp_list "@\n" pp_asset_field) fields
      in
      let assets : asset_decl list =
        List.fold_right (fun x accu -> x |> unloc |> function | Dasset a -> a::accu | _ -> accu) es []
      in
      match assets with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "## Assets@\n@\n%a"
          (pp_list "@\n" pp_asset_decl) assets
    in

    let pp_called_by = (pp_option (fun fmt (x, _) -> Format.fprintf fmt "`called by ` %a@\n" pp_expr x)) in
    let pp_actions fmt _ =
      let pp_action_decl fmt ((name, args, action_properties, _ , _exts) : action_decl) =
        let pp_formula fmt (label, f : ident * expr) =
          Format.fprintf fmt "##### %a@\n`%a`"
            pp_str label
            pp_expr f
        in
        let pp_args fmt l =
          let pp_arg fmt (id, type_, _exts : lident_typ) =
            Format.fprintf fmt "|%a||%a|"
              pp_id id
              Printer_pt.pp_type type_
          in
          if List.is_empty l
          then ()
          else
            Format.fprintf fmt "| Name | Desc | Type |@\n\
                                |--|--|--|@\n\
                                %a@\n"
              (pp_list "@\n" pp_arg) l
        in
        let pp_formulas fmt (l : (ident * expr) list) =
          if List.is_empty l
          then ()
          else
            Format.fprintf fmt "#### Postconditions @\n%a@\n"
              (pp_list "@\n" pp_formula) l
        in
        let formulas : (ident * expr) list =
          action_properties.spec_fun
          |> Option.get_as_list
          |> List.map unloc
          |> List.map fst
          |> List.flatten
          |> List.map unloc
          |>  (fun l -> List.fold_right (fun x accu ->
              match x with
              | Vpostcondition (l, e, _, _) -> (unloc l, e)::accu
              | _ -> accu
            ) l [])
        in
        let pp_rf name fmt (l : (label_exprs * exts) option) =
          let l =
            l
            |> Option.get_as_list
            |> List.map fst
            |> List.flatten
            |> List.map unloc
            |> List.map (fun (x, y) -> unloc x, y)
          in
          if List.is_empty l
          then ()
          else
            Format.fprintf fmt "#### %a @\n%a@\n"
              pp_str name
              (pp_list "@\n" pp_formula) l
        in
        Format.fprintf fmt "### %a@\n" pp_id name;
        pp_called_by fmt action_properties.calledby;
        pp_args fmt args;
        (pp_rf "require") fmt action_properties.require;
        (pp_rf "failif") fmt action_properties.failif;
        pp_formulas fmt formulas;
      in
      let actions : action_decl list =
        List.fold_right (fun x accu -> x |> unloc |> function | Daction a -> a::accu | _ -> accu) es []
      in
      match actions with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "## Actions@\n@\n%a"
          (pp_list "@\n" pp_action_decl) actions
    in

    let pp_transitions fmt _ =
      let pp_transition_decl fmt (name, _args, _, _, action_properties, _transitions, _exts : transition_decl) =
        Format.fprintf fmt "### %a@\n" pp_id name;
        pp_called_by fmt action_properties.calledby;
      in
      let transitions : transition_decl list =
        List.fold_right (fun x accu -> x |> unloc |> function | Dtransition a -> a::accu | _ -> accu) es []
      in
      match transitions with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "## Transitions@\n@\n%a"
          (pp_list "@\n" pp_transition_decl) transitions
    in

    let pp_sec_preds fmt _ =
      let rec pp_security_arg fmt arg =
        let arg = unloc arg in
        match arg with
        | Sident id -> pp_id fmt id
        | Sdot (a, b) ->
          Format.fprintf fmt "%a.%a"
            pp_id a
            pp_id b
        | Slist l ->
          Format.fprintf fmt "[%a]"
            (pp_list " or " pp_security_arg) l
        | Sapp (id, args) ->
          Format.fprintf fmt "(%a %a)"
            pp_id id
            (pp_list "@ " pp_security_arg) args
        | Sbut (id, arg) ->
          Format.fprintf fmt "(%a but %a)"
            pp_id id
            pp_security_arg arg
        | Sto (id, arg) ->
          Format.fprintf fmt "(%a to %a)"
            pp_id id
            pp_security_arg arg
      in
      let pp_security_item fmt (s : security_item) =
        let (label, id, args) = unloc s in
        Format.fprintf fmt "##### %a@\n\
                            `%a %a`"
          pp_id label
          pp_id id
          (pp_list " " pp_security_arg) args
      in
      let pp_security_items =
        pp_list "@\n" pp_security_item
      in
      let security_items : security_item list =
        List.fold_right (
          fun x accu ->
            x
            |> unloc
            |> function
            | Dsecurity s -> (
                s
                |> unloc
                |> fst
                |> fun x -> x @ accu)
            | _ -> accu) es []
      in
      match security_items with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "## Security predicates@\n@\n%a@\n@\n"
          pp_security_items security_items
    in

    pp_title fmt ();
    pp_roles fmt ();
    pp_assets fmt ();
    pp_transitions fmt ();
    pp_actions fmt ();
    pp_sec_preds fmt ()
  in

  match unloc pt with
  | Marchetype es ->
    pp_marchetype fmt es
  | Mextension (_id, _ds, _es) -> ()

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : archetype) = string_of__of_pp pp_archetype x
