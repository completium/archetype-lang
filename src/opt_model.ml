open Model
open Gen_transform
open Tools
open Location
open Ident
open Printer_tools

module T = Michelson

type model_entries =
  | Munion of model_entries * model_entries
  | Mentry of ident * T.type_
[@@deriving show {with_path = false}]

let rec pp_model_entries fmt =
  function
  | Munion (lt, rt) -> Format.fprintf fmt "+@\n  @[%a@]@\n  @[%a@]@\n" pp_model_entries lt pp_model_entries rt
  | Mentry (id, args) -> Format.fprintf fmt "%s(%a)" id Printer_michelson.pp_type args

let get_model_entries (parameter : T.type_) : model_entries =
  let rec aux (ty : T.type_) : model_entries =
    match ty with
    | {annotation = Some id} -> Mentry (Gen_decompile.remove_prefix_annot id, ty)
    | { node = Tor(lt, rt) } -> Munion (aux lt, aux rt)
    | _ -> Mentry ("default", ty)
  in
  let res = aux parameter in
  (* Format.eprintf "%a@\n" pp_model_entries res; *)
  res

let build_entries (env : Gen_decompile.env) (model : model) : model =
  let storage_ids = List.map (fun (x : parameter) -> unloc_mident x.name) model.parameters in
  let rec gen_arg_id (x : ident) = if List.mem x storage_ids then gen_arg_id (x ^ "_") else x in
  let entries = get_model_entries (Option.get env.type_parameter) in
  let mk_seq a b c =
    let aux (a : mterm) (b : mterm) : mterm =
      match a.node, b.node with
      | Mseq aa, Mseq bb -> mk_mterm (Mseq (aa @ bb)) tunit
      | Mseq aa, _ -> {a with node = Mseq (aa @ [b])}
      | _ , Mseq bb -> {b with node = Mseq (a::bb)}
      | _, _ -> mk_mterm (Mseq ([a; b])) tunit
    in
    aux (seq a) b |> aux (seq c)
  in
  let seek_ifleft param code =
    let f code =
      match code.node with
      | Mdmatchor ({node = Mvar (id, _) }, pl, bl, pr, br) when String.equal (unloc_mident id) param -> Some (pl, bl, pr, br)
      | _ -> None
    in
    match code.node with
    | Mseq s -> begin
        let rec aux accu xs =
          match xs with
          | a::tl -> begin
              match f a with
              | Some (lids, le, rids, re) -> Some (lids, mk_seq accu le tl, rids, mk_seq accu re tl)
              | None -> aux (accu @ [a]) tl
            end
          | _ -> None
        in
        aux [] s
      end
    | _ -> f code
  in
  let rec aux (entries : model_entries) (arg_pattern : dpattern) (code : mterm) : function_node list =
    match entries with
    | Munion (l, r) -> begin
        let opt_param = match arg_pattern with | DPid id -> Some id | _ -> None in
        let param = Option.get opt_param in
        match seek_ifleft param code with
        | Some (pl, bl, pr, br) -> (aux l pl bl) @ (aux r pr br)
        | None -> []
      end
    | Mentry (name, pty) -> begin
        let replace_var src dst code =
          let rec aux mt =
            match mt.node with
            | Mvar (id, vk) when String.equal (unloc_mident id) src-> {mt with node = Mvar (mk_mident (dumloc dst), vk)}
            | _ -> map_mterm aux mt
          in
          aux code
        in
        let rec flat_pair (ty : T.type_) : T.type_ list =
          match ty with
          | {node = Tpair tys} -> begin
              match List.rev tys with
              | ({node = Tpair _} as t )::tl -> List.rev tl @ (flat_pair t)
              | _ -> tys
            end
          | _ -> [ty]
        in
        let rec flat_all_pair (ty : T.type_) =
          match ty.node with
          | Tpair ls -> List.map flat_all_pair ls |> List.flatten
          | _ -> [ty]
        in
        let rec flat_dpattern = function
          | DPid id -> [id]
          | DPlist dps -> List.map flat_dpattern dps |> List.flatten
        in
        let get_annot = Gen_decompile.get_annot_from_type in
        let do_it ids tys =
          let a = List.map2 (fun id ty -> (mk_mident (dumloc (match get_annot ty with Some id -> gen_arg_id id | None -> id)), Gen_decompile.ttype_to_mtype ty, None)) ids tys in
          let c = List.map2 (fun src ty -> (src, (match get_annot ty with | Some annot -> gen_arg_id annot | _ -> src))) ids tys in
          let b = List.fold_left (fun code (src, dst) -> replace_var src dst code) code c in
          (a, b)
        in
        let args, code =
          match arg_pattern, flat_all_pair pty, flat_pair pty with
          | _, [{node = T.Tunit}], _ -> [], code
          | DPid id, [pty], _ -> [(mk_mident (dumloc "arg"), Gen_decompile.ttype_to_mtype pty, None)], replace_var id "arg" code
          | DPid "arg_1", tys, _ when List.length tys = List.length (env.parameter_list) -> begin
              let ids = List.map fst env.parameter_list in
              do_it ids tys
            end
          | DPid _, tys, _ -> begin
              let ids = List.mapi (fun i (ty : T.type_) -> match ty.annotation with | Some v -> Gen_decompile.remove_prefix_annot v | None -> ("arg_" ^ string_of_int i)) tys in
              do_it ids tys
            end
          | DPlist ids, tys, _ when List.length ids = List.length tys && List.for_all (function | DPid _ -> true | _ -> false) ids -> begin
              let ids = List.map (function | DPid id -> id | _ -> assert false) ids in
              do_it ids tys
            end
          | DPlist ids, _, tys when List.length ids = List.length tys && List.for_all (function | DPid _ -> true | _ -> false) ids -> begin
              let ids = List.map (function | DPid id -> id | _ -> assert false) ids in
              do_it ids tys
            end
          | dps, _, _ when List.length (flat_dpattern dps) = List.length (flat_all_pair pty) -> begin
              let tys = flat_all_pair pty in
              let ids = flat_dpattern dps in
              do_it ids tys
            end
          | _, _, _ ->
            Format.eprintf "node:%a@\narg_pattern:%a@\n"
              Printer_michelson.pp_type pty
              pp_dpattern arg_pattern;
            assert false
        in
        [Entry (mk_function_struct ~args (mk_mident (dumloc name)) code)]
      end
  in
  let def_entry : mterm option =
    match List.find_opt (fun (x : function_node) -> (match x with | Entry fs -> String.equal (unloc_mident fs.name) "default" | _ -> false)) model.functions with
    | Some (Entry fs) -> Some fs.body
    | _ -> None
  in
  match def_entry with
  | Some code -> {model with functions = aux entries (DPid "arg_1") code }
  | None -> model

let add_decls_var (_env : Gen_decompile.env) (model : model) : model =
  let rec get_init_value (ty : type_) : mterm =
    let f = get_init_value in
    match get_ntype ty with
    | Tasset _                   -> assert false
    | Tenum _                    -> assert false
    | Tstate                     -> mk_int 0
    | Tbuiltin Bunit             -> unit
    | Tbuiltin Bbool             -> mfalse
    | Tbuiltin Bint              -> mk_int 0
    | Tbuiltin Brational         -> mk_rat 0 1
    | Tbuiltin Bdate             -> mk_date (Core.mk_date ())
    | Tbuiltin Bduration         -> mk_duration (Core.mk_duration ())
    | Tbuiltin Btimestamp        -> mk_int 0
    | Tbuiltin Bstring           -> mk_string ""
    | Tbuiltin Baddress          -> mk_address "tz1XvkuUNDk8j2tG3RJaRUo4Xppcjc6FvK39"
    | Tbuiltin Btez              -> mk_tez 0
    | Tbuiltin Bsignature        -> mk_signature "edsigu41hG2o5Db6ss1o6pi7wWYoNQuee5o44VjAKhGP2qQ8zvY2y4Q1G7Bp6XC4dg44BzKjngGwK4V6BP8Zbz6itXo4dBQQntW"
    | Tbuiltin Bkey              -> mk_key "edpku6Pc31JWM3RXfym4pG5RzoKkyNCxQzakzsfQiG1aKXP1J651n8"
    | Tbuiltin Bkeyhash          -> mk_key_hash "tz1XvkuUNDk8j2tG3RJaRUo4Xppcjc6FvK39"
    | Tbuiltin Bbytes            -> mk_bytes ""
    | Tbuiltin Bnat              -> mk_nat 0
    | Tbuiltin Bchainid          -> mk_chain_id "NetXnHfVqm9iesp"
    | Tbuiltin Bbls12_381_fr     -> mk_bls12_381_fr "10"
    | Tbuiltin Bbls12_381_g1     -> mk_bls12_381_g1 "063bd6e11e2fcaac1dd8cf68c6b1925a73c3c583e298ed37c41c3715115cf96358a42dbe85a0228cbfd8a6c8a8c54cd015b5ae2860d1cc47f84698d951f14d9448d03f04df2ca0ffe609a2067d6f1a892163a5e05e541279134cae52b1f23c6b"
    | Tbuiltin Bbls12_381_g2     -> mk_bls12_381_g2 "10c6d5cdca84fc3c7f33061add256f48e0ab03a697832b338901898b650419eb6f334b28153fb73ad2ecd1cd2ac67053161e9f46cfbdaf7b1132a4654a55162850249650f9b873ac3113fa8c02ef1cd1df481480a4457f351d28f4da89d19fa405c3d77f686dc9a24d2681c9184bf2b091f62e6b24df651a3da8bd7067e14e7908fb02f8955b84af5081614cb5bc49b416d9edf914fc608c441b3f2eb8b6043736ddb9d4e4d62334a23b5625c14ef3e1a7e99258386310221b22d83a5eac035c"
    | Tbuiltin Bnever            -> mk_string "never"
    | Tbuiltin Bchest            -> mk_chest "dae4b7998aefb7e58aa6d8f59893e4e3aeb0cdaaf2ccada8e9eef7ac8efed8b0e0a7b8f5d0bbffcbdc88e4a1f9a3fba6de8087dbb085f0efb38e86e3f09cf9b0a6fea0dbaad0e68fe1bd98e3a6a7f8bfcee5d0b595abaf9aa5a4c3dcd3e2c38acbbcc9cffdd193cc8390ccc7f3f8a6a3d9b19ac390e6c6d6e2a3cdf9e6d7fec7dffcaeeddaee8eceeac288df939ed0bfadb696c7aac3b4fce1939ed5f0afb2bff2b9e5c1f4bc8cd196fe8490ccc1d2eb9798bdd1e5cbefceeac5c4aa8cec849aeaaa8bd9bac19d90b3c9cd94d5d0c68ff3b9a49ec1f9a9cfbba6cacdafa6e692a4949ba399d083aff4e3cae1ece8c5a2c6bb9dae848083c2c9e5dc98bf9df5ac8ec5c8eae1b7c9a9ee92fee8f0edb7ffecddc5a2c0bb91ae9fc2e0c18f92f0dda3e1abfb027b88ac13b80d13c2540181a679f1021aa4aa94ec1faf384e0000001f0c62bca2656e74df9b28296aa17e03bd476d28d813fd2f44d75e3259d0dc4b"
    | Tbuiltin Bchest_key        -> mk_chest_key "f18ca686eae9c4a0af9afaddaeb8f189f2bcdd85f2f8aec380e0a4b0d3fb98b1aae1a28bc8b1dce091def4ca96978ddabdf782a2acf5d69bbdbb9cedcfb980989298bcb4dfd1dce6df8fbad98180cbafaccaa2cfbea080ee83c0fbfdbe85c6aefbfeb095bdbbedc9acc0d8c495cf9db4b4c4c8f4ad9e96e8b9fdbf9792fbc090fea1f9c4cd95b283cafdec8eb5e5a9ddbac1adeff3b6cbfeb3bcf1d7e8c4ab97a59697d7e987ad92eb87b99cde93a09a949e92a6d4d092d99c9495e5d8d6a2b5acd5e1e298feaac8f3d8cecde4ea8d8badc0b7f3e283bfabfeb8ff8caec1ea92faacc68f83c38a988bc5a8cedc96bfb7a7d6aeeaa38b8bdb88a99ab994e089edb6c8ee84aead85cdd1abc3d481d3d29cc8b9a0b79e92dfb9ba90cfe8d5ba9098cfa7d4b70898e8a1d498b2a3e7a7d4aab294a2e484e5ea8af6e1b8a9d7dcdbfbffffc4f892a587adcdf1ddb6b28ce586e2c1c7d6bab8b8f48697fd8ccc8595f9e7e39cf6c2b48ffeeae9f2df9ed5dbc0f290edc8e5d08baccb8ad6df80a6f2bf8bab99fee09ac8f3859fcd87bdaed3a2c5bde49eb7b8978fb4b7eb8ecdbccfb3c1aeb089fedeeba3f191dadece97d3cc92c285f0fbc087c8d1ca8fa1cebecfc7c2d7d29690f8f7d4b6eec3c59087c2ea8feec3c5e8f1bb8da6d7aff2a2fbbec6c8d8e7a180f2fdeed3c0c6e1bafdf5e493c5a6e29bbec7be8aa5e3e7a7edef92c793d1a7a9c9ae86c5facba2b7b7c2b290aec3f288c385e6e8b9868ae7ad98c09db3ceb2eac9d6fdabb1a3f180b785c5d1b6a8999c99e5aa9686f38995b19de3b0b78c8af88398bdf707adbfe5f8f0ec90c1ecfda19bffe6f7c2e993f397a997ffc192aeb8b5bdf0dd9cedcbadbad1d5e5f5c6bc96c5fecca5b299dfc2abb499e798a3acabcf8e82ea83bdf586cffccdf8c7928fa892dad2c3ca9ce88992c286f7aed189aad9def7fcd6aca18accf398bdba8b94bc94eddf9599899d8abbabbbbbc3e7aba7c1cdec93eeb9e184fcdbe2cd9499b7a6b38afb95fdbde4b8a1ddc6a1b1bfd8b9c7a5ccd38eafc0dee8cbc796d49eabb983e88882c0998ecd95cab2adb8f3dfbf9ff38191afde8fa69f97edb3c197a2a7d2aba58dd88eafe59d9abae39986f3a4c3c0daebbcc3dbf4b4cfd8eee395defac8ed8fedb9f5fbf9a5cad7d7d5c584abf8dfb78197a5c4d587dff596aae6f5cfd5df8bf1ae85f5e0ced4e086f6cd958bdb899afde790d7c7b30182b8aecef88ede978981a0c4ffe8db94fdba03"
    | Tcontainer (_, _)          -> assert false
    | Tlist t                    -> mk_litlist t []
    | Toption t                  -> mk_none t
    | Ttuple ts                  -> let vs = List.map f ts in mk_tuple vs
    | Tset t                     -> mk_litset t []
    | Tmap (kt, vt)              -> mk_litmap kt vt []
    | Tbig_map (kt, vt)          -> mk_litbig_map kt vt []
    | Titerable_big_map (kt, vt) -> mk_lititerable_big_map kt vt []
    | Tor (lty, rty)             -> mk_left rty (f lty)
    | Trecord _                  -> assert false
    | Tevent _                   -> assert false
    | Tlambda (ity, oty)         -> mk_lambda ity (mk_mident (dumloc "x_l_0")) oty (f oty)
    | Tunit                      -> unit
    | Toperation                 -> mk_mterm (Mmakeoperation (mk_tez 0, mk_tez 0, unit)) toperation
    | Tcontract ty               -> mk_mterm (Mgetentrypoint (ty, mk_mident (dumloc "fake"), unit)) (tcontract ty)
    | Tticket ty                 -> mk_mterm (Mcreateticket (f ty, mk_nat 1)) (tticket ty)
    | Tsapling_state n           -> mk_sapling_state_empty n
    | Tsapling_transaction n     -> mk_sapling_transaction n "00000000000001f3849b5eba6e22354dbbccf076d39d63ab59d091f44bed6deb71a319cc10afed24a34ffaa403d7e58766dc6c5e0364a3d1a47e7286d87855544b8a9a4f04d6e1a6f80dba30932a82bb68fce3299aeed3ee9422d1330cffefed109dd0b753263470bea78799ee3f3cbb26a08c5dd8310ae8af66feb33950c45c67b7439e8c41e7941457b941e9ea3157105b860f9424eb210b4de663cd1239f692315049f789d367552c929f6b2aa4f0d01f2384ad1cc2daa5c4cd0731245506b614f67e7bd102ee0b639501c39b7028766fb469a99d3cd3754207098a1daec24645419514e76cbc29173e49d5d16e7aa43cd96acb77054aa333078b407987c4afdd42160bc5f585ba60296a8c1a1e48b7070c1d7106afdf6bf32c688d153b3871a784c354a779560000004f544b45fe787256593b593dcf8e54e9d57c15f86ad6ebc17c3ff65d5e7e6f216283ab4af840848b9a6928f3d65156fd10bef74b06366de141f906f94b48c9f0d0af5da81ee00177b8760cb6b99f74db3951eede8ad2be0b2f7aee18486431a9a1a439c639cacb0f6ebf7834e7c772d8cfa98ec7c844298f59107b5933c8876eeca7368bb9b0efb82b35e3acf6c0f6a1a7db98f3cd1c4e93f865dd654b393425d04a78e0a72529511e961025ba5e41d83a56825ab4db8809c7e9589959453608b4db6e1ce0ffa0077237bd3477007cc972642977b926d3d0d4f690550fbb543193ab31bf2c2ddf7c2a946fae1c62253dafaf25b87cbc18107469630b9f2cd0657cfdf4a6fff5d9f04bc1a50e43613900ffffffffff676980fbc2f4300c01f0b7820d00e3347c8da4ee614674376cbc45359daa54f9b5493e00000000"
  in
  let mk_decl (id, ty : ident * type_) : mterm =
    let mk_dvar id ty v = mk_declvar (mk_mident (dumloc id)) ty v in
    let v = get_init_value ty in
    mk_dvar id ty v
  in
  let for_mterm (mt : mterm) : mterm =
    let add_var (accu : (ident * type_) list) (id, ty : ident * type_) =
      match List.find_opt (fun x -> String.equal x id) (List.map fst accu) with
      | Some _ -> accu
      | None -> (id, ty)::accu
    in
    let is_fresh_id input =
      let r = Str.regexp "^x[0-9]+" in
      let res = Str.string_match r input 0 in
      res
    in
    let fetched_vars : (ident * type_) list =
      let rec aux (accu : (ident * type_) list) (mt : mterm) : (ident * type_) list =
        match mt.node with
        | Mvar (id, _) when is_fresh_id (unloc_mident id) -> add_var accu (unloc_mident id, mt.type_)
        | _ -> fold_term aux accu mt
      in
      aux [] mt
    in
    let removed_ids =
      let rec for_dpatterm = function
        | DPid id -> [id]
        | DPlist dps -> List.map for_dpatterm dps |> List.flatten
      in

      let for_for_ident = function
        | FIsimple id -> [unloc_mident id]
        | FIdouble (i, j) -> List.map unloc_mident [i; j]
      in

      let rec aux (accu : ident list) (mt : mterm) : ident list =
        match mt.node with
        | Mfor (fi, _, b) -> aux accu b @ for_for_ident fi
        | Mdmatchoption (_, ps, bs, bn) -> aux (aux accu bs) bn @ for_dpatterm ps
        | Mdmatchor (_, psl, bl, psr, br) -> aux (aux accu bl) br @ for_dpatterm psl @ for_dpatterm psr
        | _ -> fold_term aux accu mt
      in
      aux [] mt
    in
    let lid_types : (ident * type_) list = List.filter  (fun (id, _) -> not (List.exists (fun x -> String.equal id x) removed_ids)) fetched_vars in
    let res = seq ((List.map mk_decl lid_types) @ [mt]) in
    let remove_unknown_assign (mt : mterm) : mterm =
      let rec aux ctx (mt : mterm) : mterm =
        match mt.node with
        | Massign(ValueAssign, _, Avar (_, {pldesc = id}), _) when is_fresh_id id && not (List.exists (fun (x, _) -> String.equal x id) fetched_vars)-> seq []
        | _ -> map_mterm (aux ctx) mt
      in
      aux () mt
    in
    let res = remove_unknown_assign res in
    res
  in
  let for_fs (fs : function_struct) : function_struct =
    {fs with body = for_mterm fs.body}
  in
  let for_fn = function
    | Function (fs, rft) -> Function (for_fs fs, rft)
    | Getter (fs, ty)    -> Getter (for_fs fs, ty)
    | View (fs, ty, vv)  -> View (for_fs fs, ty, vv)
    | Entry fs           -> Entry (for_fs fs)
  in
  {model with functions = List.map for_fn model.functions}

let remove_operations_nil (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Massign(ValueAssign, _, Avar (_, {pldesc = "operations"}), { node = (Mlitlist []) }) -> seq []
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_tupleaccess (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mtupleaccess(c, i) -> begin
        let c = aux ctx c in
        match c.node with
        | Mtuple vs when Big_int.lt_big_int i (Big_int.big_int_of_int (List.length vs)) ->
          let n = Big_int.int_of_big_int i in
          List.nth vs n
        | _ -> {mt with node = Mtupleaccess(c, i)}
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_useless_else (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mif(cond, th, Some {node = Mseq []}) -> {mt with node = Mif(f cond, f th, None)}
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let optimize (model, env : model * Gen_decompile.env) =
  let model =
    model
    |> build_entries env
    |> add_decls_var env
    |> remove_operations_nil
    |> clean_mterm
    |> apply_syntactic_sugar
    |> remove_tupleaccess
    |> remove_useless_else
    |> transform_match
    |> flat_sequence
  in
  model, env
