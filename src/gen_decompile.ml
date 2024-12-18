open Location
open Tools
open Core
open UF

module T = Michelson
module B = Ast
module M = Model
module A = ParseTree

type env = {
  name: string;
  type_storage: T.type_ option;
  type_parameter: T.type_ option;
  storage_list: (Ident.ident * T.type_) list;
  parameter_list: (Ident.ident * T.type_) list;
}

let mk_env ?(name="") _ : env = { name = name; type_storage = None; type_parameter = None; storage_list = []; parameter_list = [] }

let remove_prefix_annot str =
  if String.starts ~pattern:"%" str || String.starts ~pattern:":" str
  then (String.sub str 1 ((String.length str) - 1))
  else str

let get_annot_from_type (ty : T.type_) : string option =
  match ty with
  | {annotation = Some annot} -> Some (remove_prefix_annot annot)
  | _ -> None

let is_valid_name input =
  let is_keyword = Lexer.keywords_ |> List.map fst |> fun x y -> List.mem y x in
  let is_address (input : string) : bool =
    let prefix = ["tz1"; "tz2"; "tz3"; "tz4"; "KT1"] in
    List.exists (fun (p : string) -> let lp = String.length p in lp <= String.length input && String.equal (String.sub input 0 lp) p) prefix
  in
  List.for_all (fun f -> not (f input)) [is_keyword; is_address]

let parse_micheline ?ijson (input : from_input)  : T.obj_micheline * env =
  let name =
    match input with
    | FIChannel (filename, _) -> begin
        match filename with
        | "<stdin>" -> "noname"
        | _ -> filename |> Filename.basename |> Filename.chop_extension
      end
    | FIString (path, _) -> path
  in
  let name = if is_valid_name name then name else "my_contract" in
  let env = mk_env ~name:name () in

  let input =
    if match ijson with | Some v -> v | _ -> (!Options.opt_json || !Options.opt_input_json)  then
      let open Yojson.Safe in

      let is_tag s l = List.exists (fun x -> String.equal s (fst x)) l in

      let rec aux (i : t) : T.obj_micheline =
        match i with
        | `Assoc l when is_tag "prim" l -> begin
            let extract l =
              List.fold_left (
                fun (prim, args, annots) (id, json) ->
                  match id, json with
                  | "prim", `String s -> (s, args, annots)
                  | "args", `List l ->  (prim, List.map aux l, annots)
                  | "annots", `List l ->  (prim, args, List.map (function | `String s -> s | _ -> assert false) l)
                  | _ -> (prim, args, annots)
              ) ("", [], []) l
            in
            let prim, args, annots = extract l in
            let prim : T.prim = { prim = prim; args = args; annots = annots } in
            T.Oprim prim
          end
        | `Assoc l when is_tag "int" l -> begin
            let json = List.find (fun x -> String.equal "int" (fst x)) l in
            let s = match snd json with | `String s -> s | _ -> assert false in
            T.Oint s
          end
        | `Assoc l when is_tag "string" l -> begin
            let json = List.find (fun x -> String.equal "string" (fst x)) l in
            let s = match snd json with | `String s -> s | _ -> assert false in
            T.Ostring s
          end
        | `Assoc l when is_tag "bytes" l -> begin
            let json = List.find (fun x -> String.equal "bytes" (fst x)) l in
            let s = match snd json with | `String s -> s | _ -> assert false in
            T.Obytes s
          end
        | `List l -> T.Oarray (List.map aux l)
        | _ -> Format.printf "%s@." (to_string i); assert false
      in
      let open Util in
      let json =
        match input with
        | FIChannel (_, ic) -> from_channel ic
        | FIString (_, content) -> from_string content
      in
      let code = json |> member "code" |> to_list in
      T.Oarray (List.map aux code)
    else
      let tokens =
        match input with
        | FIChannel (_, ic) -> Lexing.from_channel ic
        | FIString (_, content) -> Lexing.from_string content
      in
      Michelson_parser.main Michelson_lexer.token tokens
  in
  input, env

let to_michelson (input, env : T.obj_micheline * env) : T.michelson * env =
  let ff (input : T.obj_micheline) : T.michelson =
    let fa l = match l with | a::_ -> Some a | [] -> None in

    let to_type = T.to_type in

    let to_int = function | T.Oint x -> int_of_string x | o -> Format.eprintf "to_int unknown %a@." T.pp_obj_micheline o; assert false in
    let to_string = function | T.Ostring x -> x | o -> Format.eprintf "to_string unknown %a@." T.pp_obj_micheline o; assert false in

    let is_dup input =
      let r = Str.regexp "D[U]+P" in
      Str.string_match r input 0
    in

    let extract_dup input =
      if not (is_dup input)
      then assert false;
      let s = String.length input - 2 in
      let l = Tools.foldi (fun accu -> (T.mk_code T.DUP)::accu) [] s in
      T.mk_code (T.SEQ l)
    in

    let is_cadr input =
      let r = Str.regexp "C[AD]+R" in
      Str.string_match r input 0
    in

    let e_cadr input =
      let ll = ref [] in
      for i = 0 to String.length input - 1 do
        match String.get input i with
        | 'A' -> ll := !ll @ [ `A ]
        | 'D' -> ll := !ll @ [ `D ]
        | _ -> assert false
      done;
      !ll
    in

    let extract_cadr input : T.code =
      if not (is_cadr input)
      then assert false;

      let l = e_cadr (String.sub input 1 (String.length input - 2)) in

      T.mk_code (T.SEQ (List.map (function | `A -> T.mk_code T.CAR | `D -> T.mk_code T.CDR) l))
    in

    let rec to_code (o : T.obj_micheline) : T.code =
      let f = to_code in
      let to_data (o : T.obj_micheline) : T.data =
        match T.to_data_opt o with
        | Some v -> v
        | None -> begin
            try
              T.Dcode (to_code o)
            with
            | _ -> Format.eprintf "data unknown %a@." T.pp_obj_micheline o; assert false
          end
      in
      let seq = function | T.Oarray l -> List.map f l | _ -> assert false in
      match o with
      (* Control structures *)
      | Oarray l                                             -> T.mk_code (T.SEQ (List.map f l))
      | Oprim ({prim = "APPLY"; _})                          -> T.mk_code (T.APPLY)
      | Oprim ({prim = "EXEC"; _})                           -> T.mk_code (T.EXEC)
      | Oprim ({prim = "FAILWITH"; _})                       -> T.mk_code (T.FAILWITH)
      | Oprim ({prim = "IF"; args = t::e::_; _})             -> T.mk_code (T.IF        (seq t, seq e))
      | Oprim ({prim = "IF_CONS"; args = t::e::_; _})        -> T.mk_code (T.IF_CONS   (seq t, seq e))
      | Oprim ({prim = "IF_LEFT"; args = t::e::_; _})        -> T.mk_code (T.IF_LEFT   (seq t, seq e))
      | Oprim ({prim = "IF_NONE"; args = t::e::_; _})        -> T.mk_code (T.IF_NONE   (seq t, seq e))
      | Oprim ({prim = "ITER"; args = l::_; _})              -> T.mk_code (T.ITER      (seq l))
      | Oprim ({prim = "LAMBDA"; args = a::r::b; _})         -> T.mk_code (T.LAMBDA    (to_type a, to_type r, List.map f b))
      | Oprim ({prim = "LOOP"; args = l::_; _})              -> T.mk_code (T.LOOP      (seq l))
      | Oprim ({prim = "LOOP_LEFT"; args = l::_; _})         -> T.mk_code (T.LOOP_LEFT (seq l))
      (* Stack manipulation *)
      | Oprim ({prim = "DIG"; args = n::_})                  -> T.mk_code (T.DIG (to_int n))
      | Oprim ({prim = "DIG"; _})                            -> T.mk_code (T.DIG 1)
      | Oprim ({prim = "DIP"; args = n::l::_})               -> T.mk_code (T.DIP (to_int n, seq l))
      | Oprim ({prim = "DIP"; args = l::_})                  -> T.mk_code (T.DIP (1, seq l))
      | Oprim ({prim = "DROP"; args = n::_})                 -> T.mk_code (T.DROP (to_int n))
      | Oprim ({prim = "DROP"; _})                           -> T.mk_code (T.DROP 1)
      | Oprim ({prim = "DUG"; args = n::_})                  -> T.mk_code (T.DUG (to_int n))
      | Oprim ({prim = "DUG"; _})                            -> T.mk_code (T.DUG 1)
      | Oprim ({prim = "DUP"; args = n::_})                  -> T.mk_code (T.DUP_N (to_int n))
      | Oprim ({prim = "DUP"; _})                            -> T.mk_code (T.DUP)
      | Oprim ({prim = "PUSH"; args = t::v::_})              -> T.mk_code (T.PUSH (to_type t, to_data v))
      | Oprim ({prim = "SWAP"; _})                           -> T.mk_code (T.SWAP)
      (* Arthmetic operations *)
      | Oprim ({prim = "ABS"; _})                            -> T.mk_code T.ABS
      | Oprim ({prim = "ADD"; _})                            -> T.mk_code T.ADD
      | Oprim ({prim = "COMPARE"; _})                        -> T.mk_code T.COMPARE
      | Oprim ({prim = "EDIV"; _})                           -> T.mk_code T.EDIV
      | Oprim ({prim = "EQ"; _})                             -> T.mk_code T.EQ
      | Oprim ({prim = "GE"; _})                             -> T.mk_code T.GE
      | Oprim ({prim = "GT"; _})                             -> T.mk_code T.GT
      | Oprim ({prim = "NAT"; _})                            -> T.mk_code T.NAT
      | Oprim ({prim = "INT"; _})                            -> T.mk_code T.INT
      | Oprim ({prim = "BYTES"; _})                          -> T.mk_code T.BYTES
      | Oprim ({prim = "ISNAT"; _})                          -> T.mk_code T.ISNAT
      | Oprim ({prim = "LE"; _})                             -> T.mk_code T.LE
      | Oprim ({prim = "LSL"; _})                            -> T.mk_code T.LSL
      | Oprim ({prim = "LSR"; _})                            -> T.mk_code T.LSR
      | Oprim ({prim = "LT"; _})                             -> T.mk_code T.LT
      | Oprim ({prim = "MUL"; _})                            -> T.mk_code T.MUL
      | Oprim ({prim = "NEG"; _})                            -> T.mk_code T.NEG
      | Oprim ({prim = "NEQ"; _})                            -> T.mk_code T.NEQ
      | Oprim ({prim = "SUB"; _})                            -> T.mk_code T.SUB
      | Oprim ({prim = "SUB_MUTEZ"; _})                      -> T.mk_code T.SUB_MUTEZ
      (* Boolean operations *)
      | Oprim ({prim = "AND"; _})                            -> T.mk_code T.AND
      | Oprim ({prim = "NOT"; _})                            -> T.mk_code T.NOT
      | Oprim ({prim = "OR"; _})                             -> T.mk_code T.OR
      | Oprim ({prim = "XOR"; _})                            -> T.mk_code T.XOR
      (* Cryptographic operations *)
      | Oprim ({prim = "BLAKE2B"; _})                        -> T.mk_code T.BLAKE2B
      | Oprim ({prim = "CHECK_SIGNATURE"; _})                -> T.mk_code T.CHECK_SIGNATURE
      | Oprim ({prim = "HASH_KEY"; _})                       -> T.mk_code T.HASH_KEY
      | Oprim ({prim = "KECCAK"; _})                         -> T.mk_code T.KECCAK
      | Oprim ({prim = "PAIRING_CHECK"; _})                  -> T.mk_code T.PAIRING_CHECK
      | Oprim ({prim = "SAPLING_EMPTY_STATE"; args = [n]})   -> T.mk_code (T.SAPLING_EMPTY_STATE (to_int n))
      | Oprim ({prim = "SAPLING_VERIFY_UPDATE"; _})          -> T.mk_code T.SAPLING_VERIFY_UPDATE
      | Oprim ({prim = "SHA256"; _})                         -> T.mk_code T.SHA256
      | Oprim ({prim = "SHA512"; _})                         -> T.mk_code T.SHA512
      | Oprim ({prim = "SHA3"; _})                           -> T.mk_code T.SHA3
      (* Blockchain operations *)
      | Oprim ({prim = "ADDRESS"; _})                        -> T.mk_code T.ADDRESS
      | Oprim ({prim = "AMOUNT"; _})                         -> T.mk_code T.AMOUNT
      | Oprim ({prim = "BALANCE"; _})                        -> T.mk_code T.BALANCE
      | Oprim ({prim = "CHAIN_ID"; _})                       -> T.mk_code T.CHAIN_ID
      | Oprim ({prim = "CONTRACT"; args = t::_; annots = a}) -> T.mk_code (T.CONTRACT (to_type t, fa a))
      | Oprim ({prim = "CREATE_CONTRACT"; args = a::_; _})   -> T.mk_code (T.CREATE_CONTRACT a)
      | Oprim ({prim = "EMIT"; args = t::_; annots = a})     -> T.mk_code (T.EMIT (to_type t, fa a))
      | Oprim ({prim = "IMPLICIT_ACCOUNT"; _})               -> T.mk_code T.IMPLICIT_ACCOUNT
      | Oprim ({prim = "LEVEL"; _})                          -> T.mk_code T.LEVEL
      | Oprim ({prim = "MIN_BLOCK_TIME"; _})                 -> T.mk_code T.MIN_BLOCK_TIME
      | Oprim ({prim = "NOW"; _})                            -> T.mk_code T.NOW
      | Oprim ({prim = "SELF"; annots = a; _})               -> T.mk_code (T.SELF (fa a))
      | Oprim ({prim = "SELF_ADDRESS"; _})                   -> T.mk_code T.SELF_ADDRESS
      | Oprim ({prim = "SENDER"; _})                         -> T.mk_code T.SENDER
      | Oprim ({prim = "SET_DELEGATE"; _})                   -> T.mk_code T.SET_DELEGATE
      | Oprim ({prim = "SOURCE"; _})                         -> T.mk_code T.SOURCE
      | Oprim ({prim = "TOTAL_VOTING_POWER"; _})             -> T.mk_code T.TOTAL_VOTING_POWER
      | Oprim ({prim = "TRANSFER_TOKENS"; _})                -> T.mk_code T.TRANSFER_TOKENS
      | Oprim ({prim = "VOTING_POWER"; _})                   -> T.mk_code T.VOTING_POWER
      (* Operations on data structures *)
      | Oprim ({prim = "CAR"; args=[]; _})                   -> T.mk_code (T.CAR)
      | Oprim ({prim = "CDR"; args=[]; _})                   -> T.mk_code (T.CDR)
      | Oprim ({prim = "CONCAT"; _})                         -> T.mk_code (T.CONCAT)
      | Oprim ({prim = "CONS"; _})                           -> T.mk_code (T.CONS)
      | Oprim ({prim = "EMPTY_BIG_MAP" ; args = k::v::_})    -> T.mk_code (T.EMPTY_BIG_MAP (to_type k, to_type v))
      | Oprim ({prim = "EMPTY_MAP" ; args = k::v::_})        -> T.mk_code (T.EMPTY_MAP (to_type k, to_type v))
      | Oprim ({prim = "EMPTY_SET" ; args = t::_})           -> T.mk_code (T.EMPTY_SET (to_type t))
      | Oprim ({prim = "GET"; args = n::_})                  -> T.mk_code (T.GET_N (to_int n))
      | Oprim ({prim = "GET"; _})                            -> T.mk_code (T.GET)
      | Oprim ({prim = "GET_AND_UPDATE"; _})                 -> T.mk_code (T.GET_AND_UPDATE)
      | Oprim ({prim = "LEFT" ; args = t::_})                -> T.mk_code (T.LEFT (to_type t))
      | Oprim ({prim = "MAP"; args = s::_})                  -> T.mk_code (T.MAP (seq s))
      | Oprim ({prim = "MEM"; _})                            -> T.mk_code (T.MEM)
      | Oprim ({prim = "NEVER"; _})                          -> T.mk_code (T.NEVER)
      | Oprim ({prim = "NIL" ; args = t::_})                 -> T.mk_code (T.NIL (to_type t))
      | Oprim ({prim = "NONE" ; args = t::_})                -> T.mk_code (T.NONE (to_type t))
      | Oprim ({prim = "PACK"; _})                           -> T.mk_code (T.PACK)
      | Oprim ({prim = "PAIR"; args = n::_})                 -> T.mk_code (T.PAIR_N (to_int n))
      | Oprim ({prim = "PAIR"; _})                           -> T.mk_code (T.PAIR)
      | Oprim ({prim = "RIGHT" ; args = t::_})               -> T.mk_code (T.RIGHT (to_type t))
      | Oprim ({prim = "SIZE"; _})                           -> T.mk_code (T.SIZE)
      | Oprim ({prim = "SLICE"; _})                          -> T.mk_code (T.SLICE)
      | Oprim ({prim = "SOME"; _})                           -> T.mk_code (T.SOME)
      | Oprim ({prim = "UNIT"; _})                           -> T.mk_code (T.UNIT)
      | Oprim ({prim = "UNPACK" ; args = t::_})              -> T.mk_code (T.UNPACK (to_type t))
      | Oprim ({prim = "UNPAIR"; args = n::_})               -> T.mk_code (T.UNPAIR_N (to_int n))
      | Oprim ({prim = "UNPAIR"; _})                         -> T.mk_code T.UNPAIR
      | Oprim ({prim = "UPDATE"; args = n::_})               -> T.mk_code (T.UPDATE_N (to_int n))
      | Oprim ({prim = "UPDATE"; _})                         -> T.mk_code (T.UPDATE)
      (* Operations on tickets *)
      | Oprim ({prim = "JOIN_TICKETS"; _})                   -> T.mk_code (T.JOIN_TICKETS)
      | Oprim ({prim = "READ_TICKET"; _})                    -> T.mk_code (T.READ_TICKET)
      | Oprim ({prim = "SPLIT_TICKET"; _})                   -> T.mk_code (T.SPLIT_TICKET)
      | Oprim ({prim = "TICKET"; _})                         -> T.mk_code (T.TICKET)
      (* Other *)
      | Oprim ({prim = "CAST"; args = t::_})                 -> T.mk_code (T.CAST (to_type t))
      | Oprim ({prim = "RENAME"; _})                         -> T.mk_code T.RENAME
      | Oprim ({prim = "VIEW"; args = s::t::_})              -> T.mk_code (T.VIEW (to_string s, to_type t))
      | Oprim ({prim = "OPEN_CHEST"; _})                     -> T.mk_code T.OPEN_CHEST
      (* Macro *)
      | Oprim ({prim = "IFCMPEQ"; args = [l; r]})            -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code EQ;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFCMPNEQ"; args = [l; r]})           -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code NEQ; T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFCMPLT"; args = [l; r]})            -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code LT;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFCMPGT"; args = [l; r]})            -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code GT;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFCMPLE"; args = [l; r]})            -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code LE;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFCMPGE"; args = [l; r]})            -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code GE;  T.mk_code (T.IF (seq l, seq r))])

      | Oprim ({prim = "IFEQ"; args = [l; r]})               -> T.mk_code (T.SEQ [T.mk_code EQ;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFNEQ"; args = [l; r]})              -> T.mk_code (T.SEQ [T.mk_code NEQ; T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFLT"; args = [l; r]})               -> T.mk_code (T.SEQ [T.mk_code LT;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFGT"; args = [l; r]})               -> T.mk_code (T.SEQ [T.mk_code GT;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFLE"; args = [l; r]})               -> T.mk_code (T.SEQ [T.mk_code LE;  T.mk_code (T.IF (seq l, seq r))])
      | Oprim ({prim = "IFGE"; args = [l; r]})               -> T.mk_code (T.SEQ [T.mk_code GE;  T.mk_code (T.IF (seq l, seq r))])

      | Oprim ({prim = "CMPEQ"; _})                          -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code EQ])
      | Oprim ({prim = "CMPNEQ"; _})                         -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code NEQ])
      | Oprim ({prim = "CMPLT"; _})                          -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code LT])
      | Oprim ({prim = "CMPGT"; _})                          -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code GT])
      | Oprim ({prim = "CMPLE"; _})                          -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code LE])
      | Oprim ({prim = "CMPGE"; _})                          -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code GE])

      | Oprim ({prim = "ASSERT"; _})                         -> T.mk_code (T.IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))
      | Oprim ({prim = "ASSERT_NONE"; _})                    -> T.mk_code (T.IF_NONE ([], [T.mk_code UNIT; T.mk_code FAILWITH]))
      | Oprim ({prim = "ASSERT_SOME"; _})                    -> T.mk_code (T.IF_NONE ([T.mk_code UNIT; T.mk_code FAILWITH], []))
      | Oprim ({prim = "ASSERT_LEFT"; _})                    -> T.mk_code (T.IF_LEFT ([], [T.mk_code UNIT; T.mk_code FAILWITH]))
      | Oprim ({prim = "ASSERT_RIGHT"; _})                   -> T.mk_code (T.IF_LEFT ([T.mk_code UNIT; T.mk_code FAILWITH], []))

      | Oprim ({prim = "ASSERT_CMPEQ"; _})                   -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code EQ;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_CMPNEQ"; _})                  -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code NEQ; T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_CMPLT"; _})                   -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code LT;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_CMPGT"; _})                   -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code GT;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_CMPLE"; _})                   -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code LE;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_CMPGE"; _})                   -> T.mk_code (T.SEQ [T.mk_code COMPARE; T.mk_code GE;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])

      | Oprim ({prim = "ASSERT_EQ"; _})                      -> T.mk_code (T.SEQ [T.mk_code EQ;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_NEQ"; _})                     -> T.mk_code (T.SEQ [T.mk_code NEQ; T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_LT"; _})                      -> T.mk_code (T.SEQ [T.mk_code LT;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_GT"; _})                      -> T.mk_code (T.SEQ [T.mk_code GT;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_LE"; _})                      -> T.mk_code (T.SEQ [T.mk_code LE;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])
      | Oprim ({prim = "ASSERT_GE"; _})                      -> T.mk_code (T.SEQ [T.mk_code GE;  T.mk_code (IF ([], [T.mk_code UNIT; T.mk_code FAILWITH]))])

      | Oprim ({prim = "SET_CAR"; _})                        -> T.mk_code (T.SEQ [T.mk_code CDR; T.mk_code SWAP; T.mk_code PAIR])
      | Oprim ({prim = "SET_CDR"; _})                        -> T.mk_code (T.SEQ [T.mk_code CAR; T.mk_code PAIR])

      | Oprim ({prim = str; _})         when is_dup  str     -> extract_dup  str
      | Oprim ({prim = str; _})         when is_cadr str     -> extract_cadr str

      | Oprim ({prim = "SET_CAAR"; _})                       -> T.mk_code (T.SEQ [T.mk_code DUP ; T.mk_code (DIP (1, [ T.mk_code CAR; T.mk_code CDR; T.mk_code SWAP; T.mk_code PAIR ])) ; T.mk_code CDR; T.mk_code SWAP ; T.mk_code PAIR])
      | Oprim ({prim = "SET_CADR"; _})                       -> T.mk_code (T.SEQ [T.mk_code DUP ; T.mk_code (DIP (1, [ T.mk_code CAR; T.mk_code CAR; T.mk_code PAIR ])); T.mk_code CDR; T.mk_code SWAP ; T.mk_code PAIR])
      (* | Oprim ({prim = str; _})        when is_set_caadr str -> extract_set_caadr str *)
      (* | Oprim ({prim = str; _})        when is_set_cdadr str -> extract_set_cdadr str *)

      | _ -> Format.eprintf "code unknown: %a@." T.pp_obj_micheline o; assert false
    in
    let seek i l : T.obj_micheline = List.find T.(function | Oprim ({prim = p; _}) -> String.equal i p | _ -> false) l in
    let get_arg = function | T.Oprim ({args=x::_; _}) -> x | _ -> assert false in

    let l = input |> (function | Oarray l -> l | _ -> assert false) in
    let storage   = l |> seek "storage"   |> get_arg |> to_type in
    let parameter = l |> seek "parameter" |> get_arg |> to_type in
    let code      = l |> seek "code"      |> get_arg |> to_code in
    let views     = List.fold_right (fun x accu ->
        match x with
        | T.Oprim ({prim = "view"; args = [T.Ostring (id); param; ret; body]}) -> T.mk_view_struct id (to_type param) (to_type ret) (to_code body)::accu
        | _ -> accu) l [] in
    T.mk_michelson storage parameter (Michelson.Utils.flat code) ~views
  in
  ff input, env

let tycheck_michelson ((input, env) : T.michelson * env) : T.michelson * env =
  let stack = [T.tpair [input.parameter; input.storage]] in
  let mt_env = Mtyping.Env.create input in
  let _ : Mtyping.stack option = Mtyping.tycheck mt_env stack input.code in
  input, env

(* -------------------------------------------------------------------- *)
module Decomp_dir : sig
  val decompile : env -> T.michelson -> env * T.dcode
end = struct
  open Michelson

  (* ------------------------------------------------------------------ *)
  let gen () = Oo.id (object end)

  let fresh_tvar () =
    mk_type (Tvar (gen ()))

  let vlocal  ?(rigid = false) ty : dvar =
    let name = gen () in
    let name = if rigid then -name else name in
    `VLocal (ty, name)

  let vglobal ty x  : dvar = `VGlobal (ty, x)

  let as_vlocal (x : dvar) =
    match x with `VLocal i -> i | _ -> assert false

  let rec vlocal_of_rstack1 (r : rstack1) =
    match r with
    | #dvar as dv ->
      let x = vlocal (ty_of_dvar dv) in
      dvar x, (x :> rstack1)

    | `Paired (r1, r2) ->
      let e1, x1 = vlocal_of_rstack1 r1 in
      let e2, x2 = vlocal_of_rstack1 r2 in
      depair e1 e2, `Paired (x1, x2)

  let rec pp_rstack1 fmt (x : rstack1) =
    match x with
    | `VLocal  (_, i) -> Format.fprintf fmt "#%d" i
    | `VGlobal (ty, n) -> Format.fprintf fmt "(%s : %a)" n Printer_michelson.pp_type ty

    | `Paired(x, y) ->
      Format.fprintf fmt "(%a, %a)" pp_rstack1 x pp_rstack1 y

  let pp_rstack fmt (x : rstack) =
    Format.fprintf fmt "[%a]" (Printer_tools.pp_list ", " pp_rstack1) x

  let as_dvar (x : rstack1) : dvar =
    match x with
    | #dvar as x -> x
    | _ -> assert false

  exception UnificationFailure

  module Item : Ufind.Item with type t = int = struct
    type t = int

    let equal = ((=) : t -> t -> bool)
    let compare = (compare : t -> t -> int)
  end

  type effect_ = [
    | `DvarUnify of dvar * dvar
    | `ExprUnify of dexpr * dexpr
    | `TyUnify   of type_ * type_
  ]

  type ufdata = {
    global : dexpr option;
    type_  : type_;
  }

  module Data : Ufind.Data
    with type effects = effect_ list
     and type data    = ufdata
  = struct
    type data = ufdata

    type effects = effect_ list

    let noeffects : effects = []

    let fresh () : data =
      { global = None; type_ = fresh_tvar (); }

    let union (d1 : data) (d2 : data) : data * effects =
      let global, effects =     (* FIXME: Unify types? *)
        match d1.global, d2.global with
        | None, None ->
          None, []

        | Some i, None | None, Some i ->
          Some i, []

        | Some i, Some j ->
          Some i, [`ExprUnify (i, j)]
      in

      let d = { global; type_ = d1.type_; } in

      (d, `TyUnify (d1.type_, d2.type_) :: effects)
  end

  module UFE = Ufind.Make(Item)(Data)

  module TData : Ufind.Data
    with type effects = effect_ list
     and type data    = type_ option
  = struct
    type data = type_ option

    type effects = effect_ list

    let noeffects : effects = []

    let fresh () : data =
      None

    let union (d1 : data) (d2 : data) : data * effects =
      match d1, d2 with
      | None, None -> None, []
      | Some ty, None | None, Some ty -> Some ty, []
      | Some ty1, Some ty2 -> Some ty2, [`TyUnify (ty1, ty2)]
  end

  module UFT = Ufind.Make(Item)(TData)

  type uf = { ufe: UFE.t; uft: UFT.t; }

  (* -------------------------------------------------------------------- *)
  let rec dcode_propagate (uf : uf) (code : dcode) =
    List.map (icode_propagate uf) code

  and icode_propagate (uf : uf) (code : dinstr) =
    match code with
    | DIAssign (x, e) ->
      let x = var_propagate uf x in
      let e = expr_propagate uf e in
      DIAssign (x, e)

    | DIIf (e, (d1, d2)) ->
      let e  = expr_propagate uf e in
      let d1 = dcode_propagate uf d1 in
      let d2 = dcode_propagate uf d2 in

      DIIf (e, (d1, d2))

    | DIMatch (e, bs) ->
      let e = expr_propagate uf e in

      let bs =
        let for1 (x, ds, d) =
          let d = dcode_propagate uf d in
          let ds = List.map (dpattern_propagate uf) ds in
          (x, ds, d) in
        List.map for1 bs in

      DIMatch (e, bs)

    | DIFailwith e ->
      let e = expr_propagate uf e in
      DIFailwith e

    | DIWhile (e, d) ->
      let e = expr_propagate uf e in
      let d = dcode_propagate uf d in
      DIWhile (e, d)

    | DIIter (x, e, d) ->
      let x = var_propagate uf x in
      let e = expr_propagate uf e in
      let d = dcode_propagate uf d in
      DIIter (x, e, d)

    | DILoop (x, d) ->
      let x = var_propagate uf x in
      let d = dcode_propagate uf d in
      DILoop (x, d)

  and dpattern_propagate (uf : uf) (dp : dpattern) =
    match dp with
    | DVar (ty, x) ->
      DVar (ty_propagate uf ty, UFE.find x uf.ufe)

    | DPair (dp1, dp2) ->
      let dp1 = dpattern_propagate uf dp1 in
      let dp2 = dpattern_propagate uf dp2 in
      DPair (dp1, dp2)

  and expr_propagate (uf : uf) (e : dexpr) =
    let node =
      match e.node with
      | Dvar x -> begin
          let exception Default in

          try
            match x with
            | `VLocal (_, x) -> begin
                match UFE.data x uf.ufe with
                | Some { global = Some e } ->
                  (expr_propagate uf e).node
                | _ ->
                  raise Default
              end

            | _ ->
              raise Default

          with Default ->
            Dvar (var_propagate uf x)
        end

      | Depair (e1, e2) ->
        let e1 = expr_propagate uf e1 in
        let e2 = expr_propagate uf e2 in
        Depair (e1, e2)

      | Deproj (ty, e, i) ->
        let ty = ty_propagate uf ty in
        let e = expr_propagate uf e in
        Deproj (ty, e, i)

      | Ddata (_, _) ->
        e.node

      | Dfun (f, es) ->
        let es = List.map (expr_propagate uf) es in
        Dfun (f, es)

    and type_ = ty_propagate uf e.type_

    in { node; type_ }

  and var_propagate (uf : uf) (x : dvar) =
    match x with
    | `VGlobal _ ->
      x

    | `VLocal (xty, x) ->
      `VLocal (ty_propagate uf xty, UFE.find x uf.ufe)

  and rstack1_propagate (uf : uf) (r : rstack1) =
    match r with
    | #dvar as x ->
      (var_propagate uf x :> rstack1)

    | `Paired (r1, r2) ->
      let r1 = rstack1_propagate uf r1 in
      let r2 = rstack1_propagate uf r2 in
      `Paired (r1, r2)

  and ty_propagate (uf : uf) (xty : type_) =
    let rec aux (t : type_) =
      match t.node with
      | Tvar x -> begin
          match UFT.data x uf.uft with
          | Some (Some data) -> ty_propagate uf data
          | _ -> t
        end
      | _ -> map_type aux t
    in aux xty

  let rec pair_head_normalize (ty : type_) =
    match ty.node with
    | Tpair [] ->
      tunit

    | Tpair [ty] ->
      ty

    | Tpair [_; _] ->
      ty

    | Tpair (ty :: tys) ->
      tpair [ty; pair_head_normalize (tpair tys)]

    | _ ->
      ty

  let rec unify (uf : uf) (effect_ : effect_) : uf =
    pump (unify_r uf effect_)

  and pump ((uf, effects) : uf * effect_ list) : uf =
    unify_all uf effects

  and unify_all (uf : uf) (effects : effect_ list) : uf =
    List.fold_left unify uf effects

  and unify_r (uf : uf) (effect_ : effect_) : uf * effect_ list =
    match effect_ with
    | `TyUnify (ty1, ty2) ->
      unify_type_r uf ty1 ty2

    | `DvarUnify (x, y) ->
      unify_dvar_r uf x y

    | `ExprUnify (e1, e2) ->
      unify_expr_r uf e1 e2

  and unify_type_r (uf : uf) (ty1 : type_) (ty2 : type_) =
    let ty1 = pair_head_normalize (ty_propagate uf ty1) in
    let ty2 = pair_head_normalize (ty_propagate uf ty2) in

    match ty1.node, ty2.node with
    | Tvar x, Tvar y when x = y
      -> uf, []

    | Tvar x, Tvar y
      ->
      let uft, effects = UFT.union x y uf.uft in
      { uf with uft }, effects

    | Tvar x, _
      ->
      let uft, effects = UFT.set x (Some ty2) uf.uft in
      { uf with uft }, effects

    | _, Tvar y
      ->
      let uft, effects = UFT.set y (Some ty1) uf.uft in
      { uf with uft }, effects

    | Tbig_map (t1, u1), Tbig_map (t2, u2)
    | Tmap     (t1, u1), Tbig_map (t2, u2)
    | Tbig_map (t1, u1), Tmap     (t2, u2)
    | Tlambda  (t1, u1), Tlambda  (t2, u2)
    | Tmap     (t1, u1), Tmap     (t2, u2)
    | Tor      (t1, u1), Tor      (t2, u2)
      ->
      let uf, pb1 = unify_type_r uf t1 t2 in
      let uf, pb2 = unify_type_r uf u1 u2 in
      uf, pb1 @ pb2

    | Tcontract t1, Tcontract t2
    | Tlist     t1, Tlist     t2
    | Toption   t1, Toption   t2
    | Tset      t1, Tset      t2
    | Tticket   t1, Tticket   t2
      -> unify_type_r uf t1 t2

    | Tpair l1, Tpair l2 ->
      let aux (uf : uf) ((t1, t2) : type_ * type_) =
        unify_type_r uf t1 t2
      in

      let uf, effects = List.fold_left_map aux uf (List.combine l1 l2) in
      uf, List.flatten effects

    | Tsapling_state       n1, Tsapling_state n2
    | Tsapling_transaction n1, Tsapling_transaction n2
      when n1 = n2 -> uf, []

    | Taddress, Taddress
    | Tbool        , Tbool
    | Tbytes       , Tbytes
    | Tchain_id    , Tchain_id
    | Tint         , Tint
    | Tkey         , Tkey
    | Tkey_hash    , Tkey_hash
    | Tmutez       , Tmutez
    | Tnat         , Tnat
    | Toperation   , Toperation
    | Tsignature   , Tsignature
    | Tstring      , Tstring
    | Ttimestamp   , Ttimestamp
    | Tunit        , Tunit
    | Tbls12_381_fr, Tbls12_381_fr
    | Tbls12_381_g1, Tbls12_381_g1
    | Tbls12_381_g2, Tbls12_381_g2
    | Tnever       , Tnever
    | Tchest       , Tchest
    | Tchest_key   , Tchest_key
      -> uf, []

    | _, _ ->
      raise UnificationFailure

  and unify_type (uf : uf) (ty1 : type_) (ty2 : type_) =
    pump (unify_type_r uf ty1 ty2)

  and unify_dvar_r (uf : uf) (x : dvar) (y : dvar) =
    match x, y with
    | `VLocal (xty, x), `VLocal (yty, y) ->
      let ufe = uf.ufe in

      let x = UFE.find x ufe in
      let y = UFE.find y ufe in
      let prio =
        if x < 0 then Some `Left  else
        if y < 0 then Some `Right else None in

      let ufe, effects = UFE.union ?prio x y ufe in
      let ufe, effx = UFE.set x { global = None; type_ = xty } ufe in
      let ufe, effy = UFE.set y { global = None; type_ = yty } ufe in

      let effty = [`TyUnify (xty, yty)] in

      { uf with ufe }, List.flatten [effects; effx; effy; effty]

    | `VLocal  (xty, x), `VGlobal (yty, y)
    | `VGlobal (yty, y), `VLocal  (xty, x) ->

      let ufe = uf.ufe in

      let ufe, eff1 =
        UFE.set
          x
          { global = Some (dvar (`VGlobal (yty, y)));
            type_  = yty }
          ufe
      in

      let ufe, eff2 = UFE.set x { global = None; type_ = xty; } ufe in

      { uf with ufe }, (`TyUnify (xty, yty) :: eff1 @ eff2)

    | `VGlobal (xty, x), `VGlobal (yty, y) ->
      if x <> y then raise UnificationFailure;
      uf, [`TyUnify (xty, yty)]

  and unify_expr_r (uf : uf) (e1 : dexpr) (e2 : dexpr) =
    let uf = unify_type uf e1.type_ e2.type_ in

    match e1.node, e2.node with
    | Dvar x, Dvar y ->
      unify_dvar_r uf x y

    | Depair (e1, e2), Depair (f1, f2) ->
      (uf, [`ExprUnify (e1, f1); `ExprUnify (e2, f2)])

    | Deproj (ty1, e1, i1), Deproj (ty2, e2, i2) when i1 = i2 ->
      (uf, [`TyUnify (ty1, ty2); `ExprUnify (e1, e2)])

    | Ddata (_, d1), Ddata (_, d2) ->
      if not (cmp_data d1 d2) then
        raise UnificationFailure;
      (uf, [])

    | Dfun (o1, es1), Dfun (o2, es2) ->
      if o1 <> o2 || List.length es1 <> List.length es2 then
        raise UnificationFailure;
      (uf, List.map2 (fun e1 e2 -> `ExprUnify (e1, e2)) es1 es2)

    | _, _ ->
      raise UnificationFailure

(*
  and unify_dinstr_r (uf : uf) (i1 : dinstr) (i2 : dinstr) =
    match i1, i2 with
    | DIAssign (x1, e1), DIAssign (x2, e2) ->
       uf, [`DvarUnify (x1, x2); `ExprUnify (e1, e2)]

    | _, _ -> raise UnificationFailure

  and unify_dinstr (uf : uf) (i1 : dinstr) (i2 : dinstr) =
    pump (unify_dinstr_r uf i1 i2)
*)

(*
  and unify_dcode (uf : uf) (is1 : dcode) (is2 : dcode) =
    if List.length is1 <> List.length is2 then
      raise UnificationFailure;
    List.fold_left2 unify_dinstr uf is1 is2
*)

  and unify_rstack_r (uf : uf) (r1 : rstack) (r2 : rstack) =
    match r1, r2 with
    | i1 :: r1, i2 :: r2 ->
      let uf, eff1 = unify_rstack1_r uf i1 i2 in
      let uf, eff2 = unify_rstack_r uf r1 r2 in
      uf, eff1 @ eff2

    | [], [] ->
      uf, []

    | _, _ ->
      raise UnificationFailure

  and unify_rstack1_r (uf : uf) (i1 : rstack1) (i2 : rstack1) =
    match i1, i2 with
    | `Paired (r1, s1), `Paired (r2, s2) ->
      let uf, eff1 = unify_rstack1_r uf r1 r2 in
      let uf, eff2 = unify_rstack1_r uf s1 s2 in
      uf, eff1 @ eff2

    | (#dvar as i1), (#dvar as i2) ->
      unify_dvar_r uf i1 i2

    | _, _ ->
      uf, []
  (*       raise UnificationFailure *)

  and unify_stack (uf : uf) (r1 : rstack) (r2 : rstack) =
    pump (unify_rstack_r uf r1 r2)

  let norm_var (uf : uf) (x : dvar) : dvar =
    match x with
    | `VGlobal _ -> x
    | `VLocal (_, name) ->
      match UFE.data name uf.ufe with
      | Some { global = Some { node = Dvar x } } -> x
      | _ -> x

  let rec write_var (uf : uf) (e : dexpr) (x : rstack1) =
    let e = expr_propagate uf e in
    let x = rstack1_propagate uf x in
    let uf = unify_type uf (ty_of_rstack1 x) e.type_ in

    match x, e.node with
    | #dvar as x, _ ->
      [DIAssign (x, e)], uf

    | `Paired (x1, x2), Depair (e1, e2) ->
      let i1, uf = write_var uf e1 x1 in
      let i2, uf = write_var uf e2 x2 in
      (i1@i2, uf)

    | `Paired (x1, x2), _ ->
      let ty1 = fresh_tvar () in
      let ty2 = fresh_tvar () in

      let i1, uf = write_var uf (deproj ty1 e 0) x1 in
      let i2, uf = write_var uf (deproj ty2 e 1) x2 in

      let uf = unify_type uf (tpair [ty1; ty2]) e.type_ in

      (i1@i2, uf)

  let rec dexpr_of_rstack1 (x : rstack1) : dexpr =
    match x with
    | #dvar as x -> dvar x
    | `Paired (x, y) -> depair (dexpr_of_rstack1 x) (dexpr_of_rstack1 y)

  let rec merge_rstack (uf : uf) (s1 : rstack) (s2 : rstack) =
    assert (List.length s1 = List.length s2);

    match s1, s2 with
    | (#dvar as x) :: s1, (#dvar as y) :: s2 ->
      let (is1, is2), s, uf = merge_rstack uf s1 s2 in
      let a = vlocal (ty_of_dvar x) in
      (([DIAssign (a, dvar x)] @ is1), is2), y :: s, uf

    | `Paired (x1, y1) :: s1, `Paired (x2, y2) :: s2 ->
      merge_rstack uf (x1 :: y1 :: s1) (x2 :: y2 :: s2)

    | `Paired (x1, y1) :: s1, (#dvar as xy2) :: s2 ->
      let i1, uf = write_var uf (dexpr_of_rstack1 (`Paired (x1, y1))) xy2 in
      let (is1, is2), s, uf = merge_rstack uf s1 s2 in
      ((i1 @ is1), is2), `Paired (x1, y1) :: s, uf

    | #dvar :: _, `Paired _ :: _ ->
      let (is2, is1), s, uf = merge_rstack uf s2 s1 in
      (is1, is2), s, uf

    | [], [] ->
      ([], []), [], uf

    | _, _ -> assert false

  let merge_rstack (uf : uf) (s1 : rstack) (s2 : rstack) =
    merge_rstack uf s1 s2

  let rec dptn_of_rstack1 (r : rstack1) =
    match r with
    | `Paired (r1, r2) ->
      let p1, c1 = dptn_of_rstack1 r1 in
      let p2, c2 = dptn_of_rstack1 r2 in
      (DPair (p1, p2), c1 @ c2)

    | `VLocal (ty, _)  as n ->
      let x = (gen ()) in (DVar (ty, x), [DIAssign (n, dvar (`VLocal (ty, x)))])

    | (`VGlobal (ty, _)) as n ->
      let x = (gen ()) in (DVar (ty, x), [DIAssign (n, dvar (`VLocal (ty, x)))])

  (* ------------------------------------------------------------------ *)
  type decomp = {
    stack   : rstack;
    code    : dcode;
    failure : bool;
  }

  let mkdecomp ?(failure = false) stack code =
    { code; stack; failure; }

  let rec decompile_i (uf : uf) (s : rstack) (i : code) : uf * decomp =
    (*
    Format.eprintf "%s@." (String.make 78 '=');
    Format.eprintf "%a@." Printer_michelson.pp_code i;
    List.iteri (fun i r -> Format.eprintf "%i: %a@." i Michelson.pp_rstack1 r) s;
    *)

    match i.node with

    (* Control structures *)

    | SEQ l -> decompile_s uf s l

    | IF (c1, c2) -> begin
        let uf, { failure = _; stack = s1; code = b1; } = decompile_s uf s c1 in
        let uf, { failure = _; stack = s2; code = b2; } = decompile_s uf s c2 in
        let uf = unify_stack uf s1 s2 in
        let x = vlocal tbool in
        let d = mkdecomp ((x :> rstack1) :: s1) [DIIf (dvar x, (b1, b2))] in
        (uf, d)
      end

    (* Stack manipulation *)

    | DIG n ->
      assert (List.length s >= n + 1);
      let x, s1 = List.hd s, List.tl s in
      let s1, s2 = List.cut n s1 in
      (uf, mkdecomp (s1 @ (x :: s2)) [])

    | DIP (n, c) ->
      assert (List.length s >= n);
      let s1, s2 = List.cut n s in
      let uf, { failure; stack = s2; code = ops; } = decompile_s uf s2 c in
      (uf, mkdecomp ~failure (s1 @ s2) ops)

    | DROP n ->
      let pre =
        List.init n (fun _ -> (vlocal (fresh_tvar ()) :> rstack1)) in
      (uf, mkdecomp (pre @ s) [])

    | DUG n ->
      assert (List.length s >= n + 1);
      let s1, s2 = List.cut n s in
      let x, s2 = List.hd s2, List.tl s2 in
      (uf, mkdecomp (x :: (s1 @ s2)) [])

    | DUP ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      let instrs, uf = write_var uf (dexpr_of_rstack1 y) x in
      (uf, mkdecomp (y :: s) instrs)

    | DUP_N n ->
      assert (1 <= n);
      let x, s = List.pop s in
      let pre, s = List.split_at (n-1) s in
      let y, s = List.pop s in
      let instrs, uf = write_var uf (dexpr_of_rstack1 y) x in
      (uf, mkdecomp (pre @ y :: s) instrs)

    | PUSH (t, d) ->
      let x, s = List.pop s in
      let wri, uf = write_var uf (ddata t d) x in
      (uf, mkdecomp s wri)

    | SWAP ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      (uf, mkdecomp (y :: x :: s) [])

    (* Arthmetic operations *)

    | ABS      -> decompile_op uf s (`Uop Uabs     ) (Option.get !(i.type_))
    | ADD      -> decompile_op uf s (`Bop Badd     ) (Option.get !(i.type_))
    | COMPARE  -> decompile_op uf s (`Bop Bcompare ) (Option.get !(i.type_))
    | EDIV     -> decompile_op uf s (`Bop Bediv    ) (Option.get !(i.type_))
    | EQ       -> decompile_op uf s (`Uop Ueq      ) (Option.get !(i.type_))
    | GE       -> decompile_op uf s (`Uop Uge      ) (Option.get !(i.type_))
    | GT       -> decompile_op uf s (`Uop Ugt      ) (Option.get !(i.type_))
    | INT      -> decompile_op uf s (`Uop Uint     ) (Option.get !(i.type_))
    | ISNAT    -> decompile_op uf s (`Uop Uisnat   ) (Option.get !(i.type_))
    | LE       -> decompile_op uf s (`Uop Ule      ) (Option.get !(i.type_))
    | LSL      -> decompile_op uf s (`Bop Blsl     ) (Option.get !(i.type_))
    | LSR      -> decompile_op uf s (`Bop Blsr     ) (Option.get !(i.type_))
    | LT       -> decompile_op uf s (`Uop Ult      ) (Option.get !(i.type_))
    | MUL      -> decompile_op uf s (`Bop Bmul     ) (Option.get !(i.type_))
    | NEG      -> decompile_op uf s (`Uop Uneg     ) (Option.get !(i.type_))
    | NEQ      -> decompile_op uf s (`Uop Une      ) (Option.get !(i.type_))
    | SUB      -> decompile_op uf s (`Bop Bsub     ) (Option.get !(i.type_))


    (* Boolean operations *)

    | AND     -> decompile_op uf s (`Bop Band ) (Option.get !(i.type_))
    | NOT     -> decompile_op uf s (`Uop Unot ) (Option.get !(i.type_))
    | OR      -> decompile_op uf s (`Bop Bor  ) (Option.get !(i.type_))
    | XOR     -> decompile_op uf s (`Bop Bxor ) (Option.get !(i.type_))

    (* Cryptographic operations *)

    | BLAKE2B          -> decompile_op uf s (`Uop Ublake2b         ) (Option.get !(i.type_))
    | CHECK_SIGNATURE  -> decompile_op uf s (`Top Tcheck_signature ) (Option.get !(i.type_))
    | HASH_KEY         -> decompile_op uf s (`Uop Uhash_key        ) (Option.get !(i.type_))
    | SHA256           -> decompile_op uf s (`Uop Usha256          ) (Option.get !(i.type_))
    | SHA512           -> decompile_op uf s (`Uop Usha512          ) (Option.get !(i.type_))


    (* Blockchain operations *)

    | ADDRESS            -> decompile_op uf s (`Zop  Zaddress          ) (Option.get !(i.type_))
    | AMOUNT             -> decompile_op uf s (`Zop  Zamount           ) (Option.get !(i.type_))
    | BALANCE            -> decompile_op uf s (`Zop  Zbalance          ) (Option.get !(i.type_))
    | CHAIN_ID           -> decompile_op uf s (`Zop  Zchain_id         ) (Option.get !(i.type_))
    | CONTRACT (t, a)    -> decompile_op uf s (`Uop (Ucontract (t, a)) ) (Option.get !(i.type_))
    | CREATE_CONTRACT _  -> assert false
    | IMPLICIT_ACCOUNT   -> decompile_op uf s (`Uop (Uimplicitaccount) ) (Option.get !(i.type_))
    | NOW                -> decompile_op uf s (`Zop Znow               ) (Option.get !(i.type_))
    | SELF a             -> decompile_op uf s (`Zop (Zself a)          ) (Option.get !(i.type_))
    | SENDER             -> decompile_op uf s (`Zop Zsender            ) (Option.get !(i.type_))
    | SET_DELEGATE       -> decompile_op uf s (`Uop Usetdelegate       ) (Option.get !(i.type_))
    | SOURCE             -> decompile_op uf s (`Zop Zsource            ) (Option.get !(i.type_))
    | TRANSFER_TOKENS    -> decompile_op uf s (`Top Ttransfer_tokens   ) (Option.get !(i.type_))


    (* Operations on data structures *)

    | CAR ->
      let x, s = List.pop s in
      let d =
        mkdecomp (`Paired (x, (vlocal (fresh_tvar ()) :> rstack1)) :: s) [] in
      (uf, d)

    | CDR ->
      let y, s = List.pop s in
      let d =
        mkdecomp (`Paired ((vlocal (fresh_tvar ()) :> rstack1), y) :: s) [] in
      (uf, d)

    | CONS ->
      let l, s = List.pop s in
      let a = fresh_tvar () in
      let uf = unify_type uf (ty_of_rstack1 l) (tlist a) in
      let hd = vlocal a in
      let c, uf =
        write_var uf (dfun (tlist a) (`Bop Bcons) [dvar hd; (dexpr_of_rstack1 l)]) l
      in

      let d =
        mkdecomp
          ((hd :> rstack1) :: (l :> rstack1) :: s)
          c
      in
      (uf, d)

    | GET ->
      let tymap = List.nth (fst (Option.get !(i.type_))) 1 in

      let tmap =
        match tymap.node with
        | Tbig_map _ -> tbig_map
        | _ -> tmap in

      let l, s = List.pop s in
      let a = fresh_tvar () in
      let b = fresh_tvar () in
      let uf = unify_type uf (ty_of_rstack1 l) (toption b) in
      let k = vlocal a in
      let m = vlocal (tmap a b) in
      let c, uf = write_var uf (dfun (toption b) (`Bop Bget) [dvar k; dvar m]) l in
      let d =
        mkdecomp
          ((k :> rstack1) :: (m :> rstack1) :: s)
          c
      in
      (uf, d)


    | CONCAT               -> decompile_op uf s (`Bop Bconcat               ) (Option.get !(i.type_))
    | EMPTY_BIG_MAP (k, v) -> decompile_op uf s (`Zop (Zemptybigmap (k, v)) ) (Option.get !(i.type_))
    | EMPTY_MAP (k, v)     -> decompile_op uf s (`Zop (Zemptymap (k, v))    ) (Option.get !(i.type_))
    | EMPTY_SET t          -> decompile_op uf s (`Zop (Zemptyset t)         ) (Option.get !(i.type_))
    | LEFT t               -> decompile_op uf s (`Uop (Uleft t)             ) (Option.get !(i.type_))
    | MAP _cs              -> assert false
    | MEM                  -> decompile_op uf s (`Bop Bmem                  ) (Option.get !(i.type_))
    | NIL t                -> decompile_op uf s (`Zop (Znil t)              ) (Option.get !(i.type_))
    | NONE t               -> decompile_op uf s (`Zop (Znone t)             ) (Option.get !(i.type_))
    | PACK                 -> decompile_op uf s (`Uop Upack                 ) (Option.get !(i.type_))

    | PAIR ->  begin
        let x, s = List.pop s in

        match x with
        | `Paired (x1, x2) ->
          (uf, mkdecomp (x1 :: x2 :: s) [])

        | #dvar as v ->
          let x1 = vlocal (fresh_tvar ()) in
          let x2 = vlocal (fresh_tvar ()) in
          (* FIXME: (x1, x2) is a pair *)
          let op = DIAssign (v, dfun (ty_of_dvar v) (`Bop Bpair) [dvar x1; dvar x2]) in
          let d = mkdecomp ((x1 :> rstack1) :: (x2 :> rstack1) :: s) [op] in
          (uf, d)
      end

    | PAIR_N n ->
      assert (2 <= n);
      let rec doit s n =
        if n <= 1 then s, [] else

          let x, s = List.pop s in

          let x1, s, ops =
            match x with
            | `Paired (x1, x2) ->
              x1, x2 :: s, []

            | #dvar as v ->
              let x1 = vlocal (fresh_tvar ()) in
              let x2 = vlocal (fresh_tvar ()) in
              let op = DIAssign (v, dfun (ty_of_dvar v) (`Bop Bpair) [dvar x1; dvar x2]) in
              (x1 :> rstack1), (x2 :> rstack1) :: s, [op]
          in

          let s, ops' = doit s (n-1) in

          x1 :: s, ops @ ops' in

      let s, ops = doit s n in
      (uf, mkdecomp s ops)

    | RIGHT t  -> decompile_op uf s (`Uop (Uright t)  ) (Option.get !(i.type_))
    | SIZE     -> decompile_op uf s (`Uop Usize       ) (Option.get !(i.type_))
    | SLICE    -> decompile_op uf s (`Top Tslice      ) (Option.get !(i.type_))
    | SOME     -> decompile_op uf s (`Uop (Usome)     ) (Option.get !(i.type_))
    | UNIT     -> decompile_op uf s (`Zop (Zunit)     ) (Option.get !(i.type_))
    | UNPACK t -> decompile_op uf s (`Uop (Uunpack t) ) (Option.get !(i.type_))
    | UPDATE   -> decompile_op uf s (`Top Tupdate     ) (Option.get !(i.type_))

    | UPDATE_N n -> begin
        let x, s = List.pop s in

        let rec doit b n va (a : rstack1) =
          match n with
          | 0 when not b ->
            va
          | 0 ->
            let x2 = vlocal (fresh_tvar ()) in (* FIXME *)
            `Paired (a, (x2 :> rstack1))
          | _ ->
            let x1 = vlocal (fresh_tvar ()) in (* FIXME *)
            let a  = doit b (n-1) va a in
            `Paired ((x1 :> rstack1), a) in

        let a = vlocal (fresh_tvar ()) in (* FIXME *)
        let y = doit (n mod 2 <> 0) (n / 2) (a :> rstack1) x in

        let wri1, uf = write_var uf (dvar a) x in
        let wri2, uf = write_var uf (dvar a) y in

        (uf, mkdecomp ((a :> rstack1) :: x :: s) (wri1 @ wri2))
      end

    | GET_N n -> begin
        let x, s = List.pop s in

        let rec doit (n : int) (a : rstack1) =
          match n with
          | 0 ->
            a
          | 1 ->
            let x2 = vlocal (fresh_tvar ()) in (* FIXME *)
            `Paired (a, (x2 :> rstack1))
          | _ ->
            let x1 = vlocal (fresh_tvar ()) in (* FIXME *)
            let a  = doit (n-2) a in
            `Paired ((x1 :> rstack1), a) in

        (uf, mkdecomp ((doit n x) :: s) [])
      end

    (* Other *)

    | UNPAIR ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      (uf, mkdecomp (`Paired (x, y) :: s) [])

    | UNPAIR_N n ->
      assert (2 <= n);
      let rec doit s n =
        if n <= 1 then
          List.pop s
        else
          let x, s = List.pop s in
          let y, s = doit s (n-1) in
          `Paired (x, y), s in

      let top, s = doit s n in
      (uf, mkdecomp (top :: s) [])

    | SELF_ADDRESS -> decompile_op uf s (`Zop Zself_address) (Option.get !(i.type_))

    | ITER cs ->
      let uf, { failure; stack = s; code = bd1 } = decompile_s uf s cs in

      if failure then
        assert false;

      let x1, s1 = List.pop s in

(*
      let uf, { stack = s; code = _bd2 } = decompile_s uf s cs in
      let x2, _s2 = List.pop s in
*)
(*
      let uf = unify_stack uf s1 s2 in
      let uf = unify_dcode uf bd1 bd2 in
*)

      (*      let uf = pump (unify_rstack1_r uf x1 x2) in *)
      let xs = vlocal (tlist (ty_of_rstack1 x1)) in
      let d =
        mkdecomp
          ((xs :> rstack1) :: s1)
          [DIIter (as_dvar x1, dvar xs, bd1)]
      in (uf, d)

(*
    | LOOP cs ->
      let cond = vlocal (fresh_tvar ()) in (* FIXME *)

      let { failure; stack = s1; code = bd1 } =
        decompile_s ((cond :> rstack1) :: s) cs in

      if failure then
        assert false;

      let bd2 = (decompile_s ((cond :> rstack1) :: s1) cs).code in

      let uf = UF.create () in

      unify_dcode uf bd1 bd2;

      let bd = dcode_apply_uf uf bd1 in
      let s = rstack_apply_uf uf ((cond :> rstack1) :: s1) in
      let cond, _ = List.pop s in

      mkdecomp s [DILoop (as_dvar cond, bd)]
*)

    | IF_CONS (c1, c2) ->
      let ty = fresh_tvar () in

      compile_match uf s
        (tlist ty)
        [ ("cons", [ty; tlist ty]), c1
        ; ("nil" , []), c2
        ]

    | IF_LEFT (c1, c2) ->
      let ty1 = fresh_tvar () in
      let ty2 = fresh_tvar () in

      compile_match uf s
        (tor ty1 ty2)
        [ ("left" , [ty1]), c1
        ; ("right", [ty2]), c2
        ]

    | IF_NONE (c1, c2) ->
      let ty = fresh_tvar () in

      compile_match uf s
        (toption ty)
        [ ("none", []), c1
        ; ("some", [ty]), c2
        ]

    | FAILWITH ->
      let n    = fst (Option.get (!(i.type_))) in
      let s    = List.map (fun ty -> (vlocal ty :> rstack1)) n in
      let x, _ = List.pop s in
      (uf, mkdecomp ~failure:true s [DIFailwith (dexpr_of_rstack1 x)])

    | _ -> (Format.eprintf "%a@\n" pp_code i; assert false)

  and decompile_op
      (uf      : uf)
      (s       : rstack)
      (op      : g_operator)
      (tystack : type_ list * type_ list option)
    =
    let before, after = tystack in
    let after = Option.get after in

    let n = match op with | `Zop _ -> 0 | `Uop _ -> 1 | `Bop _ -> 2 | `Top _ -> 3
                          | `FExec _ | `FApply _ -> assert false (* TODO: FIXME *)
    in
    let x, s = List.pop s in
    let args = List.map (fun ty -> vlocal ty) (fst (List.split_at n before)) in
    let uf = unify_type uf (ty_of_rstack1 x) (List.hd after) in
    let c, uf =
      write_var uf
        (dfun (ty_of_rstack1 x) op (List.map (fun v -> dvar v) args)) x (* FIXME *) in
    let decomp =
      mkdecomp
        ((args :> rstack) @ s)
        c in
    uf, decomp

  and compile_match
      (uf : uf)
      (s  : rstack)
      (ty : type_)
      (bs : ((string * type_ list) * code list) list)
    =
    let x = vlocal ty in

    let uf, subs = List.fold_left_map (fun uf ((name, args), b) ->
        (* FIXME: check for failures *)
        let n = List.length args in
        let (uf, { stack = sc; code = bc }) = decompile_s uf s b in
        assert (List.length sc >= n);
        let p, sc = List.cut n sc in
        let uf = List.fold_left2 (fun uf rs ty -> unify_type uf (ty_of_rstack1 rs) ty) uf p args in
        let p, dp = List.split (List.map dptn_of_rstack1 p) in
        (uf, (sc, (name, p, List.flatten dp @ bc)))
      ) uf bs in

    let scs, subs = List.split subs in
    let sc = List.hd scs in

    let uf =
      List.fold_left
        (fun uf sc' -> pump (unify_rstack_r uf sc sc'))
        uf (List.tl scs) in

    let d = mkdecomp ((x :> rstack1) :: sc) [DIMatch (dvar x, subs)] in
    (uf, d)

  and decompile_s (uf : uf) (s : rstack) (c : code list) : uf * decomp =
    let (failure, uf, stack), code =
      List.fold_left_map (fun (oldfail, uf, stack) code ->
          let uf, { failure; stack; code; } = decompile_i uf stack code in
          (oldfail || failure, uf, stack), code
        ) (false, uf, s) (List.rev c) in

    (uf, { failure; stack; code = List.flatten (List.rev code); })

  (* -------------------------------------------------------------------- *)
  module DvarCompare : Map.OrderedType with type t = dvar = struct
    type t = dvar

    let compare = (Stdlib.compare : t -> t -> int)
  end

  module Mdvar = Tools.Map.Make(DvarCompare)
  module Sdvar = Tools.Set.Make(DvarCompare)

  (* -------------------------------------------------------------------- *)
  let rec expr_fv (e : dexpr) : Sdvar.t =
    match e.node with
    | Dvar x ->
      Sdvar.singleton x

    | Depair (e1, e2) ->
      Sdvar.union (expr_fv e1) (expr_fv e2)

    | Deproj (_, e, (_ : int)) ->
      expr_fv e

    | Ddata _ ->
      Sdvar.empty

    | Dfun (_, es) ->
      Sdvar.unions (List.map expr_fv es)

  (* -------------------------------------------------------------------- *)
  let rec pattern_fv (p : dpattern) =
    match p with
    | DVar i ->
      Sdvar.singleton (`VLocal i)

    | DPair (p1, p2) ->
      Sdvar.union (pattern_fv p1) (pattern_fv p2)

  (* -------------------------------------------------------------------- *)
  let rec instr_wr (i : dinstr) =
    match i with
    | DIAssign (x, {node = Dvar y; _}) when x = y ->
      Sdvar.empty

    | DIAssign (x, _) ->
      Sdvar.singleton x

    | DIIf (_, (c1, c2)) ->
      Sdvar.union (code_wr c1) (code_wr c2)

    | DIWhile (_, c) ->
      code_wr c

    | DIIter (x, _, c) ->
      Sdvar.remove x (code_wr c)

    | DILoop (x, c) ->
      Sdvar.add x (code_wr c)

    | DIMatch (_, bs) ->
      let for1 (_, ps, c) =
        Sdvar.diff
          (code_wr c)
          (Sdvar.unions (List.map pattern_fv ps)) in

      Sdvar.unions (List.map for1 bs)

    | DIFailwith _ ->
      Sdvar.empty

  and code_wr (c : dcode) =
    Sdvar.unions (List.map instr_wr c)

  (* -------------------------------------------------------------------- *)
  let rec expr_cttprop (env : dexpr Mint.t) (e : dexpr) =
    match e.node with
    | Dvar (`VLocal (_, x)) ->
      Option.get_dfl e (Mint.find_opt x env)

    | Dvar _ ->
      e

    | Depair (e1, e2) ->
      let e1 = expr_cttprop env e1 in
      let e2 = expr_cttprop env e2 in
      depair e1 e2

    | Deproj (ty, e, i) ->
      let e = expr_cttprop env e in
      deproj ty e i

    | Ddata _ ->
      e

    | Dfun (op, es) ->
      let es = List.map (expr_cttprop env) es in
      dfun e.type_ op es

  type cttenv = dexpr Mint.t

  let cttenv_remove_wr (env : cttenv) (wr : Sdvar.t) =
    Sdvar.fold (fun x env ->
        match x with
        | `VLocal  (_, i) -> Mint.remove i env
        | `VGlobal _ -> env) wr env

  let rec instr_cttprop (env0 : cttenv) (code : dinstr) =
    match code with
    | DIAssign ((`VLocal i), {node = Dvar (`VLocal j)}) when i = j ->
      env0, []

    | DIAssign ((`VLocal (_, i)) as x, e) ->
      let env = Mint.remove i env0 in
      let e   = expr_cttprop env e in
      let env = Mint.filter (fun _ se -> not (Sdvar.mem x (expr_fv se))) env in

      let env =
        if   (Sdvar.mem x (expr_fv e)) && can_propagate e
        then env
        else Mint.add i e env in

      env, [DIAssign (x, e)]

    | DIAssign ((`VGlobal _) as x, e) ->
      let e   = expr_cttprop env0 e in
      let env = Mint.filter (fun _ se -> not (Sdvar.mem x (expr_fv se))) env0 in
      env, [DIAssign (x, e)]

    | DIIf (e, (c1, c2)) ->
      let wr    = Sdvar.union (code_wr c1) (code_wr c2) in
      let env   = cttenv_remove_wr env0 wr in
      let _, c1 = code_cttprop env0 c1 in
      let _, c2 = code_cttprop env0 c2 in

      env, [DIIf (expr_cttprop env0 e, (c1, c2))]

    | DIWhile (e, c) ->
      let wr   = code_wr c in
      let env  = cttenv_remove_wr env0 wr in
      let _, c = code_cttprop env c in

      env, [DIWhile (expr_cttprop env0 e, c)]

    | DIIter (x, e, c) ->
      let wr   = Sdvar.remove x (code_wr c) in
      let env  = cttenv_remove_wr env0 wr in
      let _, c = code_cttprop env c in

      env, [DIIter (x, expr_cttprop env0 e, c)]

    | DILoop (x, c) ->
      let wr   = Sdvar.remove x (code_wr c) in
      let env  = cttenv_remove_wr env0 wr in
      let _, c = code_cttprop env c in

      env, [DILoop (x, c)]

    | DIMatch (e, bs) ->
      let wr =
        let for1 (_, p, c) =
          let fv = Sdvar.unions (List.map pattern_fv p) in
          Sdvar.fold Sdvar.remove fv (code_wr c) in
        Sdvar.unions (List.map for1 bs) in

      let env = cttenv_remove_wr env0 wr in

      let bs =
        let for1 (x, p, c) = (x, p, snd (code_cttprop env0 c)) in
        List.map for1 bs in

      env, [DIMatch (expr_cttprop env0 e, bs)]

    | DIFailwith e ->
      env0, [DIFailwith (expr_cttprop env0 e)]

  and code_cttprop (env : cttenv) (code : dcode) =
    let env, code =
      List.fold_left_map instr_cttprop env code
    in env, List.flatten code

  and can_propagate (e : dexpr) : bool =
    match e.node with
    | Ddata (_, d) -> can_propagate_data d
    | Dvar _ -> true
    | _ -> false

  and can_propagate_data (d : data): bool =
    match d with
    | Dint _
    | Dstring _
    | Dbytes _
    | Dunit
    | Dtrue
    | Dfalse
    | Dnone
    | Dconstant _ -> true

    | Dleft d | Dright d ->
      can_propagate_data d

    | _ -> false

  (* -------------------------------------------------------------------- *)
  let rec instr_kill (keep : Sdvar.t) (instr : dinstr) =
    match instr with
    | DIAssign (x, { node = Dvar y }) when x = y
      -> keep, []

    | DIAssign ((`VLocal _) as x, e)
      when not (Sdvar.mem x (Sdvar.union keep (expr_fv e)))
      -> keep, []

    | DIAssign ((`VLocal _) as x, e) ->
      Sdvar.add x (Sdvar.union keep (expr_fv e)), [instr]

    | DIAssign (_, e) ->
      Sdvar.union keep (expr_fv e), [instr]

    | DIIf (e, (c1, c2)) ->
      let keep1, c1 = code_kill keep c1 in
      let keep2, c2 = code_kill keep c2 in

      Sdvar.union (expr_fv e) (Sdvar.union keep1 keep2),
      [DIIf (e, (c1, c2))]

    | DIWhile (e, c) ->
      let keep, c =
        code_kill (Sdvar.union keep (expr_fv e)) c
      in keep, [DIWhile (e, c)]

    | DIIter (x, e, c) ->
      let keep, c =
        code_kill (Sdvar.add x (Sdvar.union keep (expr_fv e))) c in

      Sdvar.remove x keep, [DIIter (x, e, c)]

    | DILoop (x, c) ->
      let keep, c = code_kill (Sdvar.add x keep) c in
      keep, [DILoop (x, c)]

    | DIMatch (e, bs) ->
      let for1 (x, pv, c) =
        let pfv = List.map pattern_fv pv in
        let pfv = Sdvar.unions pfv in
        let keep, c = code_kill (Sdvar.union keep pfv) c in
        Sdvar.diff keep pfv, (x, pv, c) in

      let keep, bs = List.split (List.map for1 bs) in
      let keep = Sdvar.unions keep in
      let keep = Sdvar.union keep (expr_fv e) in

      keep, [DIMatch (e, bs)]

    | DIFailwith e ->
      Sdvar.union keep (expr_fv e), [instr]

  and code_kill (keep : Sdvar.t) (code : dcode) =
    let keep, code = List.fold_left_map instr_kill keep (List.rev code) in
    keep, List.flatten (List.rev code)

  (* -------------------------------------------------------------------- *)
  let decompile (env : env) (michelson : michelson) : env * dcode =
    let aty = michelson.storage in
    let pty = michelson.parameter in
    let code = let c = michelson.code in match c.node with | SEQ l -> l | _ -> [c] in

    let create_args with_annot (name : string) (ty : type_) =
      let i = ref 0 in
      let rec aux (ty : T.type_) =
        let mkvar ty : rstack1 =
          let fresh_id _ =
            i := !i + 1;
            Printf.sprintf "%s_%d" name !i
          in
          let str = match with_annot, ty.annotation with | true, Some a -> remove_prefix_annot a | _ -> fresh_id () in
          `VGlobal (ty, str)
        in
        match ty.node with
        | Tpair [t1; t2] -> `Paired (mkvar t1, aux t2)
        | Tpair (t1::ts) -> `Paired (mkvar t1, aux (tpair ts))
        | _ -> mkvar ty
      in
      aux ty

    in

    let pst = create_args false "arg" pty in
    let ast = create_args true "sto" aty in

    let uf, { stack = ost; code = dc; } =
      decompile_s
        { ufe = UFE.initial; uft = UFT.initial; }
        [`Paired (`VGlobal (tlist toperation, "operations"), ast)] code
    in

    let code, uf =
      match ost with
      | [`Paired (px, ax)] ->
        let pr1, uf = write_var uf (dexpr_of_rstack1 pst) px in
        let pr2, uf = write_var uf (dexpr_of_rstack1 ast) ax in
        pr1 @ pr2 @ dc, uf
      | _ -> Format.eprintf "%a@." pp_rstack ost; assert false in

    (*    let _, code = code_kill Sdvar.empty code in*)
    let code = dcode_propagate uf code in
    let _, code = code_cttprop Mint.empty code in
    let _, code = code_kill Sdvar.empty code in

    let rec f (rstack : rstack1) : (Ident.ident * T.type_) list =
      match rstack with
      | `VGlobal (ty, id) -> [(id, ty)]
      | `Paired (p1, p2) -> f p1 @ f p2
      | _ -> []
    in
    let env = {env with storage_list = f ast; parameter_list = f pst  } in
    env, code
end

let to_dir (michelson, env : T.michelson * env) =
  let tstorage   = michelson.storage in
  let tparameter = michelson.parameter in
  let name = env.name in

  let storage_data =
    match tstorage.node with
    | T.Tunit   -> T.Dunit
    | T.Tnat
    | T.Tint    -> T.Dint Big_int.zero_big_int
    | T.Tstring -> T.Dstring ""
    | _ -> T.Dunit (* FIXME*)
  in

  let env, code = Decomp_dir.decompile env michelson in

  (T.mk_dprogram tstorage tparameter storage_data name code), env

let rec ttype_to_mtype (t : T.type_) : M.type_ =
  let f = ttype_to_mtype in
  match t.node with
  | Tkey                   -> M.tkey
  | Tunit                  -> M.tunit
  | Tsignature             -> M.tsignature
  | Toption    t           -> M.toption (f t)
  | Tlist      t           -> M.tlist   (f t)
  | Tset       t           -> M.tset    (f t)
  | Toperation             -> M.toperation
  | Tcontract  t           -> M.tcontract (f t)
  | Tpair      l           -> M.ttuple (List.map f l)
  | Tor        (lt, rt)    -> M.tor(f lt) (f rt)
  | Tlambda    (at, rt)    -> M.tlambda (f at) (f rt)
  | Tmap       (kt, vt)    -> M.tmap (f kt) (f vt)
  | Tbig_map   (kt, vt)    -> M.tbig_map (f kt) (f vt)
  | Tchain_id              -> M.tchainid
  | Tint                   -> M.tint
  | Tnat                   -> M.tnat
  | Tstring                -> M.tstring
  | Tbytes                 -> M.tbytes
  | Tmutez                 -> M.ttez
  | Tbool                  -> M.tbool
  | Tkey_hash              -> M.tkeyhash
  | Ttimestamp             -> M.ttimestamp
  | Taddress               -> M.taddress
  | Tticket t              -> M.tticket (f t)
  | Tsapling_transaction n -> M.tsapling_transaction n
  | Tsapling_state       n -> M.tsapling_state n
  | Tbls12_381_fr          -> M.tbls12_381_fr
  | Tbls12_381_g1          -> M.tbls12_381_g1
  | Tbls12_381_g2          -> M.tbls12_381_g2
  | Tnever                 -> M.tnever
  | Tchest                 -> M.tchest
  | Tchest_key             -> M.tchest_key
  | Tvar _                 -> M.tunit

let rec data_to_mterm ?omap_var ?t ?(ft : (T.type_ -> M.type_) option) (d : T.data) : M.mterm =
  let f = data_to_mterm ?omap_var in
  let is_nat = Option.map_dfl (fun (t : T.type_) -> match t.node with | T.Tnat -> true | _ -> false) false in
  let is_mutez = Option.map_dfl (fun (t : T.type_) -> match t.node with | T.Tmutez -> true | _ -> false) false in
  let is_address = Option.map_dfl (fun (t : T.type_) -> match t.node with | T.Taddress -> true | _ -> false) false in
  let is_gen f x = x |> f |> Option.is_some in
  let as_set (ty : T.type_ option) = match ty with | Some ({node = T.Tset ty}) -> Some ty | _ -> None in
  let is_set  (ty : T.type_ option) = ty |> is_gen as_set in
  let as_list (ty : T.type_ option) = match ty with | Some ({node = T.Tlist ty}) -> Some ty | _ -> None in
  let is_list  (ty : T.type_ option) = ty |> is_gen as_list in
  let as_map (ty : T.type_ option) = match ty with | Some ({node = T.Tmap (kt, vt)}) -> Some (kt, vt) | _ -> None in
  let is_map  (ty : T.type_ option) = ty |> is_gen as_map in
  let as_big_map (ty : T.type_ option) = match ty with | Some ({node = T.Tbig_map (kt, vt)}) -> Some (kt, vt) | _ -> None in
  let is_big_map  (ty : T.type_ option) = ty |> is_gen as_big_map in
  let as_option (ty : T.type_ option) = match ty with | Some ({node = T.Toption ty}) -> Some ty | _ -> None in
  let is_option  (ty : T.type_ option) = ty |> is_gen as_option in
  (* let as_or (ty : T.type_ option) = match ty with | Some ({node = T.Tor (kt, vt)}) -> Some (kt, vt) | _ -> None in
     let is_or  (ty : T.type_ option) = ty |> is_gen as_or in *)
  let do_elt (d : T.data) : M.mterm * M.mterm = match d with | Delt (a, b) -> (f a, f b) | _ -> assert false in
  let ft ty = match ft with Some ft -> ft ty | None -> M.tnever in
  match d with
  | Dint    v when is_nat t     -> M.mk_bnat v
  | Dint    v when is_mutez t   -> M.mk_btez v
  | Dint    v                   -> M.mk_bint v
  | Dstring v when is_address t -> M.mk_address v
  | Dstring v                   -> M.mk_string v
  | Dbytes  v                   -> M.mk_bytes v
  | Dunit                       -> M.unit
  | Dtrue                       -> M.mtrue
  | Dfalse                      -> M.mfalse
  | Dpair   l                   -> M.mk_pair (List.map f l)
  | Dleft    d                  -> begin
      let tl, tr = (match t with
          | Some { node = Tor (tl, tr) } -> tl, tr
          | _ -> T.tnever, T.tnever)
      in
      M.mk_left (ttype_to_mtype tr) (f ~t:tl d)
    end
  | Dright d          -> begin
      let tl, tr = (match t with
          | Some { node = Tor (tl, tr) } -> tl, tr
          | _ -> T.tnever, T.tnever)
      in
      M.mk_right (ttype_to_mtype tl) (f ~t:tr d)
    end
  | Dsome d  -> M.mk_some (f d)
  | Dnone  when is_option t   -> M.mk_none (ft (Option.get (as_option t)))
  | Dnone                     -> assert false
  | Dlist l when is_set t     -> M.mk_litset  (ft (Option.get (as_set t))) (List.map f l)
  | Dlist l when is_list t    -> M.mk_litlist (ft (Option.get (as_list t))) (List.map f l)
  | Dlist l when is_map t     -> let kt, vt = t |> as_map     |> Option.get  in M.mk_litmap (ft kt) (ft vt) (List.map do_elt l)
  | Dlist l when is_big_map t -> let kt, vt = t |> as_big_map |> Option.get  in M.mk_litbig_map (ft kt) (ft vt) (List.map do_elt l)
  | Dlist l          -> M.mk_litlist M.tunit (List.map f l)
  | Delt _           -> assert false
  | Dvar (id, _, _)  -> (match omap_var with Some map_var -> map_var id | None -> assert false)
  | DIrCode _        -> assert false
  | Dcode _          -> assert false
  | Dlambda_rec _    -> assert false
  | Dconstant _      -> assert false

module Decomp_model : sig
  val decompile : T.dprogram * env -> M.model * env
end = struct
  open Ident
  open Michelson
  open Model

  let for_type (t : T.type_) : M.type_ = ttype_to_mtype t

  let int_to_var (n : int) : string = "x" ^ (string_of_int n)

  let to_duration x = mk_mterm (Mmult (x, (mk_duration (Core.mk_duration ~seconds:Big_int.unit_big_int ())))) tduration

  let for_code (code : dcode) : mterm =
    let ft = for_type in

    let for_dvar (v : dvar) : ident * T.type_ =
      match v with
      | `VLocal  (ty, x ) -> int_to_var x, ty
      | `VGlobal (ty, id) -> id, ty
    in

    let rec for_expr (e : dexpr) : mterm =
      let f = for_expr in
      let tunknown = tunit in
      let mk_map = MKMap in
      match e.node with
      | Dvar v          -> let id, ty = for_dvar v in mk_mvar (M.mk_mident (dumloc (id))) (ttype_to_mtype ty)
      | Ddata (t, d)    -> data_to_mterm ~t ~ft:ttype_to_mtype d
      | Depair (e1, e2) -> mk_pair [for_expr e1; for_expr e2]
      | Deproj (_ty, e, i) -> mk_tupleaccess i (f e)
      | Dfun (`Uop Ueq, [{node = Dfun (`Bop Bcompare, [a; b])}]) -> mk_mterm (Mequal  (tint, f a, f b)) tbool
      | Dfun (`Uop Une, [{node = Dfun (`Bop Bcompare, [a; b])}]) -> mk_mterm (Mnequal (tint, f a, f b)) tbool
      | Dfun (`Uop Ugt, [{node = Dfun (`Bop Bcompare, [a; b])}]) -> mk_mterm (Mgt     (f a, f b)) tbool
      | Dfun (`Uop Uge, [{node = Dfun (`Bop Bcompare, [a; b])}]) -> mk_mterm (Mge     (f a, f b)) tbool
      | Dfun (`Uop Ult, [{node = Dfun (`Bop Bcompare, [a; b])}]) -> mk_mterm (Mlt     (f a, f b)) tbool
      | Dfun (`Uop Ule, [{node = Dfun (`Bop Bcompare, [a; b])}]) -> mk_mterm (Mle     (f a, f b)) tbool
      | Dfun (op, args) -> begin
          match op, args with
          | `Zop Znow,                       [] -> mnow
          | `Zop Zamount,                    [] -> mtransferred
          | `Zop Zbalance,                   [] -> mbalance
          | `Zop Zsource,                    [] -> msource
          | `Zop Zsender,                    [] -> mcaller
          | `Zop Zaddress,                   [] -> assert false
          | `Zop Zchain_id,                  [] -> mselfchainid
          | `Zop Zself a,                    [] -> let ty = ft e.type_ in let cty = match M.get_ntype ty with | M.Tcontract ty -> ty | _ -> assert false in mk_mterm (Mselfcontract (cty, Option.map (fun x -> dumloc (remove_prefix_annot x)) a)) ty
          | `Zop Zself_address,              [] -> mselfaddress
          | `Zop Znone t,                    [] -> mk_none (ft t)
          | `Zop Zunit,                      [] -> unit
          | `Zop Znil t,                     [] -> mk_mterm (Mlitlist []) (tlist (ft t))
          | `Zop Zemptyset  t,               [] -> mk_mterm (Mlitset [])  (tlist (ft t))
          | `Zop Zemptymap (tk, tv),         [] -> mk_mterm (Mlitmap (MKMap, [])) (tmap (ft tk) (ft tv))
          | `Zop Zemptybigmap (tk, tv),      [] -> mk_mterm (Mlitmap (MKBigMap, [])) (tmap (ft tk) (ft tv))
          | `Uop Ucar,                    [ a ] -> mk_tupleaccess 0 (f a)
          | `Uop Ucdr,                    [ a ] -> mk_tupleaccess 1 (f a)
          | `Uop Uleft t,                 [ a ] -> mk_left  (ft t) (f a)
          | `Uop Uright t,                [ a ] -> mk_right (ft t) (f a)
          | `Uop Uneg,                    [ a ] -> mk_mterm (Muminus (f a)) tint
          | `Uop Uint,                    [ a ] -> mk_nat_to_int (f a)
          | `Uop Unot,                    [ a ] -> mnot (f a)
          | `Uop Uabs,                    [ a ] -> mk_mterm (Mabs (f a)) tnat
          | `Uop Uisnat,                  [ a ] -> mk_mterm (Minttonat (f a)) (toption tnat)
          | `Uop Usome,                   [ a ] -> mk_some (f a)
          | `Uop Usize,                   [ a ] -> mk_mterm (Mlistlength (tunknown, f a)) tnat
          | `Uop Upack,                   [ a ] -> mk_pack (f a)
          | `Uop Uunpack t,               [ a ] -> mk_unpack (ft t) (f a)
          | `Uop Ublake2b,                [ a ] -> mk_blake2b (f a)
          | `Uop Usha256,                 [ a ] -> mk_sha256 (f a)
          | `Uop Usha512,                 [ a ] -> mk_sha512 (f a)
          | `Uop Uhash_key,               [ a ] -> mk_keytokeyhash (f a)
          | `Uop Ufail,                   [ a ] -> failg (f a)
          | `Uop Ucontract (t, an),       [ a ] -> let an = (match an with | Some an -> an | None -> "default") in mk_get_entrypoint (ft t) (dumloc an) (f a)
          | `Uop Usetdelegate,            [ a ] -> mk_setdelegate (f a)
          | `Uop Uimplicitaccount,        [ a ] -> mk_keyhashtocontract (f a)
          | `Uop Ueq,                     [ _ ] -> assert false
          | `Uop Une,                     [ _ ] -> assert false
          | `Uop Ugt,                     [ _ ] -> assert false
          | `Uop Uge,                     [ _ ] -> assert false
          | `Uop Ult,                     [ _ ] -> assert false
          | `Uop Ule,                     [ _ ] -> assert false
          | `Bop Badd,                 [ a; b ] -> begin
              let a = f a in
              let b = f b in
              match get_ntype a.type_, get_ntype b.type_ with
              | M.Tbuiltin Bnat, M.Tbuiltin Bnat  -> mk_mterm (Mplus (a, b)) tnat
              | M.Tbuiltin Bint, M.Tbuiltin Bnat  -> mk_mterm (Mplus (a, b)) tint
              | M.Tbuiltin Bnat, M.Tbuiltin Bint  -> mk_mterm (Mplus (a, b)) tint
              | M.Tbuiltin Bint, M.Tbuiltin Bint  -> mk_mterm (Mplus (a, b)) tint
              | M.Tbuiltin Btez, M.Tbuiltin Btez  -> mk_mterm (Mplus (a, b)) ttez
              | M.Tbuiltin Bbls12_381_fr, M.Tbuiltin Bbls12_381_fr  -> mk_mterm (Mplus (a, b)) tbls12_381_fr
              | M.Tbuiltin Bbls12_381_g1, M.Tbuiltin Bbls12_381_g1  -> mk_mterm (Mplus (a, b)) tbls12_381_g1
              | M.Tbuiltin Bbls12_381_g2, M.Tbuiltin Bbls12_381_g2  -> mk_mterm (Mplus (a, b)) tbls12_381_g2
              | M.Tbuiltin (Bnat | Bint), M.Tbuiltin (Bdate | Btimestamp) -> mk_mterm (Mplus (to_duration a, b)) tdate
              | M.Tbuiltin (Bdate | Btimestamp), M.Tbuiltin (Bnat | Bint) -> mk_mterm (Mplus (a, to_duration b)) tdate
              | _ -> mk_mterm (Mplus (a, b)) tint
            end
          | `Bop Bsub,                 [ a; b ] -> mk_mterm (Mminus (f a, f b)) tint
          | `Bop Bmul,                 [ a; b ] -> mk_mterm (Mmult (f a, f b)) tint
          | `Bop Bediv,                [ a; b ] -> mk_mterm (Mdivmod (f a, f b)) (ttuple [tint; tint])
          | `Bop Blsl,                 [ a; b ] -> mk_mterm (Mshiftleft (f a, f b)) tbytes
          | `Bop Blsr,                 [ a; b ] -> mk_mterm (Mshiftright (f a, f b)) tbytes
          | `Bop Bor,                  [ a; b ] -> mk_mterm (Mor  (f a, f b)) tbool
          | `Bop Band,                 [ a; b ] -> mk_mterm (Mgreedyand (f a, f b)) tbool
          | `Bop Bxor,                 [ a; b ] -> mk_mterm (Mgreedyor (f a, f b)) tbool
          | `Bop Bcompare,             [ _; _ ] -> assert false
          | `Bop Bget,                 [ a; b ] -> mk_mterm (Mmapget (mk_map, tunknown, tunknown, f b, f a, None)) tunknown
          | `Bop Bmem,                 [ a; b ] -> mk_mterm (Mmapcontains(mk_map, tunknown, tunknown, f a, f b)) tunknown
          | `Bop Bconcat,              [ a; b ] -> mk_mterm (Mconcat (f a, f b)) tunknown
          | `Bop Bcons,                [ a; b ] -> mk_mterm (Mlistprepend (tunknown, f b, f a)) tunknown
          | `Bop Bpair,                [ a; b ] -> mk_tuple [f a; f b]
          | `Bop Bexec,                [ _a; _b ] -> assert false
          | `Bop Bapply,               [ _a; _b ] -> assert false
          | `Top Tcheck_signature,  [ a; b; c ] -> mk_checksignature (f a) (f b) (f c)
          | `Top Tslice,            [ a; b; c ] -> mk_mterm (Mslice (f a, f b, f c)) tunknown
          | `Top Tupdate,           [ a; b; c ] -> begin
              let mty = ft c.type_ in
              match c.type_ with
              | {node = T.Tset ty} -> mk_mterm (Msetupdate (ft ty, f c, f a, f b)) mty
              | {node = T.Tmap (kt, vt)}
              | {node = T.Tbig_map (kt, vt)} -> mk_mterm (Mmapupdate (mk_map, ft kt, ft vt, f c, f a, f b)) mty
              | _ -> mk_mterm (Mmapupdate (mk_map, tunknown, tunknown, f c, f a, f b)) tunknown
            end
          | `Top Ttransfer_tokens,  [ a; b; c ] -> mk_mterm (Mmakeoperation (f b, f c, f a)) toperation
          | _ -> assert false
        end

    in

    let rec for_instr i : mterm =
      let f = for_instr in
      let g = for_expr in
      let seq (c : dcode) : mterm =
        let instrs = List.map f c in
        seq instrs
      in
      let extract_or_pattern (input : T.dpattern list) : M.dpattern =
        let rec aux = function
          | DVar (_ty, n) -> M.DPid (int_to_var n)
          | DPair (p1, p2) -> M.DPlist [aux p1; aux p2]
        in
        let rec flat_dpattern = function
          | M.DPlist [x] -> flat_dpattern x
          | M.DPlist l -> begin
              match List.rev l with
              | (M.DPlist l2)::tl -> flat_dpattern (M.DPlist ((List.rev tl) @ l2))
              | _ -> M.DPlist (List.map flat_dpattern l)
            end
          | (_ as x) -> x
        in
        M.DPlist (List.map aux input) |> flat_dpattern
      in
      begin
        match i with
        | DIAssign (x, e) ->
          let id, _ = for_dvar x in
          let e = g e in
          mk_mterm (Massign (ValueAssign, tunit, Avar (M.mk_mident (dumloc id)), e)) tunit

        | DIIf (c, (b1, b2)) ->
          mk_mterm (Mif (g c, seq b1, Some (seq b2))) tunit

        | DIMatch (c, [("left", lv, lc) ; ("right", rv, rc)]) ->
          let lis = extract_or_pattern lv in
          let ris = extract_or_pattern rv in
          mk_mterm (Mdmatchor (g c, lis, seq lc, ris, seq rc)) tunit

        | DIMatch (c, [("none", _nv, nc) ; ("some", sv, sc)]) ->
          let sis = extract_or_pattern sv in
          mk_mterm (Mdmatchoption (g c, sis, seq sc, seq nc)) tunit

        (* | DIMatch (c, bs) ->
           let rec pp_pattern fmt = function
            | DVar x ->
              Format.fprintf fmt "%a" pp_var (`VLocal x)
            | DPair (p1, p2) ->
              Format.fprintf fmt "(%a, %a)" pp_pattern p1 pp_pattern p2 in

           let pp_branch fmt (c, ptn, body)  =
            Format.fprintf fmt "@[<v 2>%s %a =>@\n%a@]" c
              (Format.pp_print_list
                 ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
                 pp_pattern) ptn
              pp_dcode body
           in

           Format.fprintf fmt "match (%a) with@\n%a@\nend" pp_expr c
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
               pp_branch) bs *)

        | DIMatch    (_c, _) -> Format.eprintf "%a@\n" pp_dinstr i; assert false
        | DIFailwith e -> failg (for_expr e)
        | DIWhile    (cond, body) ->  mk_mterm (Mwhile (g cond, seq body)) tunit
        | DIIter     (id, coll, body) ->
          let fid = match id with | `VLocal (_, n) -> int_to_var n | `VGlobal (_, id) -> id in
          let b = Model.seq (List.map f body) in
          mk_mterm (Mfor (FIsimple (mk_mident (dumloc fid)), ICKlist (g coll), b)) tunit

        | DILoop     (_id, _body) -> Format.eprintf "%a@\n" pp_dinstr i; assert false
        (* | _ -> Format.eprintf "%a@\n" pp_dinstr i; assert false *)
      end
    in
    let instrs = List.map for_instr code in
    seq instrs

  let decompile (dprogram, env : dprogram * env) =
    let code = for_code dprogram.code in
    let functions = [Entry (mk_function_struct (M.mk_mident (dumloc "default")) code ~args:[M.mk_mident (dumloc "arg"), for_type dprogram.parameter, None])] in

    let parameters =
      match env.storage_list with
      | [_, ({node = T.Tunit})] -> []
      | storage_list ->
        List.map (fun (id, t) ->
            M.mk_parameter (M.mk_mident (dumloc id)) (for_type t)
          ) storage_list
    in
    let model = M.mk_model (dumloc dprogram.name) ~functions ~parameters in
    model, {env with type_storage = Some dprogram.storage; type_parameter = Some dprogram.parameter}
end

let dir_to_model (dir, env : T.dprogram * env) : M.model * env =
  Decomp_model.decompile (dir, env)

let to_archetype (model, _env : M.model * env) : A.archetype =
  let to_assign_operator = function
    | M.ValueAssign -> A.ValueAssign
    | M.PlusAssign  -> A.PlusAssign
    | M.MinusAssign -> A.MinusAssign
    | M.MultAssign  -> A.MultAssign
    | M.DivAssign   -> A.DivAssign
    | M.AndAssign   -> A.AndAssign
    | M.OrAssign    -> A.OrAssign
  in

  let rec for_type (t : M.type_) : A.type_t =
    let f = for_type in
    match M.get_ntype t with
    | Tasset id                  -> A.tref (M.unloc_mident id)
    | Tenum id                   -> A.tref (M.unloc_mident id)
    | Tstate                     -> assert false
    | Tbuiltin Bunit             -> A.tunit
    | Tbuiltin Bbool             -> A.tbool
    | Tbuiltin Bint              -> A.tint
    | Tbuiltin Brational         -> A.trational
    | Tbuiltin Bdate             -> A.tdate
    | Tbuiltin Bduration         -> A.tduration
    | Tbuiltin Btimestamp        -> A.tdate
    | Tbuiltin Bstring           -> A.tstring
    | Tbuiltin Baddress          -> A.taddress
    | Tbuiltin Btez              -> A.ttez
    | Tbuiltin Bsignature        -> A.tsignature
    | Tbuiltin Bkey              -> A.tkey
    | Tbuiltin Bkeyhash          -> A.tkey_hash
    | Tbuiltin Bbytes            -> A.tbytes
    | Tbuiltin Bnat              -> A.tnat
    | Tbuiltin Bchainid          -> A.tchain_id
    | Tbuiltin Bbls12_381_fr     -> A.tbls12_381_fr
    | Tbuiltin Bbls12_381_g1     -> A.tbls12_381_g1
    | Tbuiltin Bbls12_381_g2     -> A.tbls12_381_g2
    | Tbuiltin Bnever            -> A.tnever
    | Tbuiltin Bchest            -> A.tchest
    | Tbuiltin Bchest_key        -> A.tchest_key
    | Tcontainer (t, c)          -> A.mk_tcontainer (f t)
                                      (match c with
                                       | Collection      -> assert false
                                       | Aggregate       -> A.Aggregate
                                       | Partition       -> A.Partition
                                       | AssetContainer  -> A.AssetContainer
                                       | AssetKey        -> A.AssetKey
                                       | AssetValue      -> A.AssetValue
                                       | View            -> A.AssetView)
    | Tlist t                    -> A.mk_tlist (f t)
    | Toption t                  -> A.mk_toption (f t)
    | Ttuple tl                  -> A.mk_ttuple (List.map f tl)
    | Tset t                     -> A.mk_tset (f t)
    | Tmap (kt, vt)              -> A.mk_tmap (f kt) (f vt)
    | Tbig_map (kt, vt)          -> A.mk_tbig_map (f kt) (f vt)
    | Titerable_big_map (kt, vt) -> A.mk_titerable_big_map (f kt) (f vt)
    | Tor (lt, rt)               -> A.mk_tor (f lt) (f rt)
    | Trecord id                 -> A.tref (M.unloc_mident id)
    | Tevent id                  -> A.tref (M.unloc_mident id)
    | Tlambda (a, b)             -> A.mk_tlambda (f a) (f b)
    | Tunit                      -> A.tunit
    | Toperation                 -> A.toperation
    | Tcontract t                -> A.mk_tcontract (f t)
    | Tticket t                  -> A.mk_tticket (f t)
    | Tsapling_state n           -> A.mk_sapling_state (Big_int.big_int_of_int n)
    | Tsapling_transaction n     -> A.mk_sapling_transaction (Big_int.big_int_of_int n)
  in

  let for_op = function
    | M.ValueAssign -> A.ValueAssign
    | M.PlusAssign  -> A.PlusAssign
    | M.MinusAssign -> A.MinusAssign
    | M.MultAssign  -> A.MultAssign
    | M.DivAssign   -> A.DivAssign
    | M.AndAssign   -> A.AndAssign
    | M.OrAssign    -> A.OrAssign
  in

  let mident_to_sid ((sc, id) : M.mident) : (A.id_scope * A.lident) =
    match sc with
    | None -> (SINone, id)
    | Some s -> (SIId s, id)
  in

  let id_to_sid (id : Ident.ident) : (A.id_scope * A.lident) = (SINone, dumloc id) in

  let rec for_expr (mt : M.mterm) : A.expr =
    let f = for_expr in
    let ft = for_type in
    let f_cst id = A.eterm (dumloc id) in
    let f_app ?(ts = []) id args =
      match ts with
      | [] -> A.eapp (A.Fident (id_to_sid id)) args
      | _ -> A.eappt (A.Fident (id_to_sid id)) ts args
    in

    let to_ck an = function
      | M.CKcoll -> (A.eterm (dumloc an))
      | M.CKview v -> f v
      | M.CKfield (_oan, _fn, k) -> f k
    in

    let to_ck2 an = function
      | M.CKcoll -> (A.eterm (dumloc an))
      | M.CKview v -> f v
      | M.CKfield (an, fn, k) -> A.edot (A.esqapp (A.eterm (dumloc an)) (f k)) (id_to_sid fn)
    in

    let as_set (ty : M.type_) = match fst ty with | M.Tset ty -> Some ty | _ -> None in
    let as_list (ty : M.type_) = match fst ty with | M.Tlist ty -> Some ty | _ -> None in
    let as_map (ty : M.type_) = match fst ty with | M.Tmap (k, v) | M.Tbig_map (k, v) -> Some (k, v) | _ -> None in

    let to_ak = function
      | M.Avar id                   -> A.eterm (snd id)
      | M.Avarstore id              -> A.eterm (snd id)
      | M.Aasset (an, fn, k)        -> A.edot (A.esqapp (A.eterm2 (mident_to_sid an)) (f k)) (mident_to_sid fn)
      | M.Arecord (lv, _rn, fn)     -> A.edot (f lv) (mident_to_sid fn)
      | M.Atuple (lv, n, _l)        -> A.esqapp (f lv) (A.ebnat (Big_int.big_int_of_int n))
      | M.Astate                    -> A.eterm (dumloc "state")
      | M.Aoperations               -> A.eterm (dumloc "operations")
    in

    match mt.node with
    (* lambda *)

    | Mletin (ids, a, t, b, o)            -> begin
        let lv = match a with | LVsimple v -> v | LVreplace _ -> assert false in
        let id = match ids with id::_ -> id | _ -> assert false in
        A.eletin (snd id) ?t:(Option.map ft t) ?o:(Option.map f o) (f lv) (f b)
      end
    | Mdeclvar (ids, t, v, c)             -> A.evar (List.map snd ids) ?t:(Option.map ft t) (f v) VDKbasic c
    | Mdeclvaropt (ids, t, v, fa, c)      -> A.evar (List.map snd ids) ?t:(Option.map ft t) (f v) (VDKoption (Option.map f fa)) c
    | Mapp (e, args)                      -> A.eapp (A.Fident (mident_to_sid e)) (List.map f args)


    (* assign *)

    | Massign (op, _, ak, v)              -> A.eassign (for_op op) (to_ak ak) (f v)
    | Massignopt (_op, _t, ak, v, fa)     -> A.eassignopt (to_ak ak) (f v) (f fa)

    (* control *)

    | Mif (c, t, e)              -> A.eif ?e:(Option.map f e) (f c) (f t)
    | Mmatchwith (x, l) ->
      A.ematchwith (f x) (List.map (fun (x, y : (M.pattern * M.mterm)) -> ([
          match x.node with
          | Pconst (id, ids) -> dumloc (A.Pref (dumloc (A.PIdent (unloc (snd id))), ids))
          | Pwild            -> dumloc (A.Pwild)
        ], f y)) l) MKbasic

    | Minstrmatchoption (x, i, ve, ne) ->
      A.ematchwith (f x) [
        ([dumloc (A.Pref (dumloc A.PSome, List.map snd i))], f ve);
        ([dumloc (A.Pref (dumloc A.PNone, []))], f ne)
      ] MKbasic

    | Minstrmatchor (x, xl, bl, xr, br) ->
      A.ematchwith (f x) [
        ([dumloc (A.Pref (dumloc A.PLeft , List.map snd xl))], f bl);
        ([dumloc (A.Pref (dumloc A.PRight, List.map snd xr))], f br)
      ] MKbasic

    | Minstrmatchlist (x, hid, tid, hte, ee) ->
      A.ematchwith (f x) [
        ([dumloc (A.Pref (dumloc A.PCons, [snd hid; snd tid]))], f hte);
        ([dumloc (A.Pref (dumloc A.PNil, []))], f ee)
      ] A.MKbasic

    | Minstrmatchdetach  (dk, i, ve, ne) ->
      let to_dk dk =
        match dk with
        | M.DK_option (_, x) -> A.eterm (dumloc x)
        | M.DK_map (_, i, k) -> A.esqapp (A.eterm (dumloc i)) (f k)
      in
      A.ematchwith (to_dk dk) [
        ([dumloc (A.Pref (dumloc A.PSome, [snd i]))], f ve);
        ([dumloc (A.Pref (dumloc A.PNone, []))], f ne)
      ] A.MKdetach

    | Mfor (i, c, b)             -> begin
        let to_for_ident (i : M.for_ident) : A.for_ident =
          let v =
            match i with
            | FIsimple id -> A.FIsimple (snd id)
            | FIdouble (i1, i2) -> A.FIdouble (snd i1, snd i2)
          in
          dumloc v
        in
        let to_iter_container (c : M.iter_container_kind) : A.expr =
          match c with
          | ICKcoll  an -> A.eterm (dumloc an)
          | ICKview  v -> f v
          | ICKfield (_an, _fn, v) -> f v (*A.edot (A.esqapp (A.eterm (dumloc an)) (f v)) ((SINone, dumloc fn))*)
          | ICKset   v -> f v
          | ICKlist  v -> f v
          | ICKmap   v -> f v
        in
        A.efor (to_for_ident i) (to_iter_container c) (f b)
      end
    | Miter (i, a, b, c, _n)     -> A.eiter (snd i) ~min:(f a) (f b) (f c)
    | Mwhile (c, b)              -> A.ewhile (f c) (f b)
    | Mseq l                     -> begin
        match List.rev l with
        | []   -> A.enothing ()
        | [e]  -> f e
        | e::t -> List.fold_left (fun accu x -> A.eseq (f x) accu) (f e) t
      end
    | Mreturn x                  -> A.ereturn (f x)


    (* effect *)

    | Mfail ft           -> begin
        let v =
          match ft with
          | Invalid e                -> f e
          | InvalidCaller            -> A.estring M.fail_msg_INVALID_CALLER
          | InvalidSource            -> A.estring M.fail_msg_INVALID_SOURCE
          | InvalidCondition (id, c) -> let v = match c with | None -> A.estring id | Some v -> f v in A.etuple [A.estring M.fail_msg_INVALID_CONDITION; v]
          | NotFound                 -> A.estring M.fail_msg_INVALID_STATE
          | AssetNotFound id         -> A.etuple [A.estring M.fail_msg_ASSET_NOT_FOUND; A.estring id]
          | KeyExists id             -> A.etuple [A.estring M.fail_msg_KEY_EXISTS; A.estring id]
          | KeyExistsOrNotFound id   -> A.etuple [A.estring M.fail_msg_KEY_EXISTS_OR_NOT_FOUND; A.estring id]
          | DivByZero                -> A.estring M.fail_msg_DIV_BY_ZERO
          | NatNegAssign             -> A.estring M.fail_msg_NAT_NEG_ASSIGN
          | NoTransfer               -> A.estring M.fail_msg_NO_TRANSFER
          | InvalidState             -> A.estring M.fail_msg_INVALID_STATE
        in
        A.efail v
      end
    | Mfailexpr x        -> A.efailexpr (f x)
    | Mfailsome x        -> A.efailsome (f x)
    | Mdorequire (c, v)  -> A.edorequire (f c) (f v)
    | Mdofailif (c, v)   -> A.edofailif (f c) (f v)
    | Mtransfer tk       -> begin
        let tr =
          match tk with
          | TKsimple (a, dst) -> A.TTsimple (f a, f dst)
          | TKcall (e, id, ty, dst, args) -> A.TTcontract (f e, f dst, dumloc id, ft ty, f args)
          | TKentry (x, e, arg) ->
            let id =
              match e.node with
              | Mvar (id, _) -> snd id
              | _ -> assert false
            in
            A.TTentry (f x, id, f arg)
          | TKgen  (_a, _cn, _, _address_arg, _en, _arg) -> assert false
          | TKself (e, name, args) -> A.TTself (f e, dumloc name, List.map (fun (_, y) -> f y) args)
          | TKoperation op -> A.TToperation (f op)
        in
        A.etransfer tr
      end
    | Memit (a, b) -> A.eemit (dumloc (A.Tref (mident_to_sid a)), None) (f b)
    | Msandboxexec (_a, _b, _c) -> assert false
    | Mdetach (a, b, _c, d) ->
      let to_detach_kind = function
        | M.DK_option (_, id) -> A.eterm (dumloc id)
        | M.DK_map (_, id, k) -> A.esqapp (A.eterm (dumloc id)) (f k)
      in
      A.edetach (snd a) (to_detach_kind b) (f d)
    | Mmicheline m -> A.emicheline (Micheline_tools.obj_to_micheline_t m)


    (* entrypoint *)

    | Mgetentrypoint (t, a, s)         -> A.eentrypoint (ft t) (A.estring (M.unloc_mident a)) (f s) None
    | Mcallview (t, a, b, c)           -> A.ecallview (ft t) (f a) (A.estring (M.unloc_mident b)) (f c)
    | Mimportcallview (_t, _a, _b, _c) -> assert false
    | Mself id                         -> A.eself (snd id)
    | Mselfcallview (_t, id, args)     -> A.emethod A.MKself id (List.map f args)
    | Mselfcontract (ty, oid)          -> A.eself_contract (ft ty) (oid)


    (* operation *)

    | Moperations                       -> f_cst "operations"
    | Mmakeoperation (v, d, a)          -> f_app "make_operation" [f v; f d; f a]
    | Mmakeevent (t, id, a)             -> f_app "make_event" ~ts:[ft t] [(A.estring (M.unloc_mident id)); f a]
    | Mmakesandboxexecoperation (a, b, c) -> f_app "make_sandbox_exec_operation" [f a; f c; f b]
    | Mcreatecontract (cc, d, a) -> begin
        let code, storage =
          match cc with
          | CCTz (m, a)    -> (A.emicheline (Micheline_tools.obj_to_pt m.ms_content), f a)
          | CCArl (_id, _l) -> assert false
        in
        f_app "create_contract" [code; f d; f a; storage]
      end


    (* literals *)

    | Mint v             -> A.ebint v
    | Mnat v             -> A.ebnat v
    | Mbool true         -> A.etrue
    | Mbool false        -> A.efalse
    | Mrational (n, d)   -> A.eapp (A.Foperator (dumloc (A.Arith A.DivRat))) [A.ebint n; A.ebint d]
    | Mstring v          -> A.estring v
    | Mmutez v           -> A.eutz (Big_int.string_of_big_int v)
    | Maddress v         -> A.eaddress v
    | Mdate v            -> let d = Core.date_to_string v in A.edate d
    | Mduration v        -> let d = Core.duration_to_string v in A.eduration d
    | Mtimestamp _v      -> assert false
    | Mbytes v           -> A.ebytes v
    | Mchain_id v        -> A.estring v
    | Mkey v             -> A.estring v
    | Mkey_hash v        -> A.estring v
    | Msignature v       -> A.estring v
    | Mbls12_381_fr v    -> A.ebytesFr v
    | Mbls12_381_fr_n v  -> A.enumberFr v
    | Mbls12_381_g1 v    -> A.ebytesG1 v
    | Mbls12_381_g2 v    -> A.ebytesG2 v
    | Munit              -> A.eunit ()
    | MsaplingStateEmpty _v -> assert false
    | MsaplingTransaction (_, v) -> A.ebytes v
    | Mchest v           -> A.ebytes v
    | Mchest_key v       -> A.ebytes v
    | Mtz_expr v         -> A.etz_expr v


    (* control expression *)

    | Mexprif (c, t, e) -> A.eif (f c) (f t) ~e:(f e)
    | Mexprmatchwith (x, l) ->
      A.ematchwith (f x) (List.map (fun (x, y : (M.pattern * M.mterm)) -> ([
          match x.node with
          | Pconst (id, ids) -> dumloc (A.Pref (dumloc (A.PIdent (unloc (snd id))), ids))
          | Pwild            -> dumloc (A.Pwild)
        ], f y)) l) MKbasic

    | Mmatchoption (x, i, ve, ne) ->
      A.ematchwith (f x) [
        ([dumloc (A.Pref (dumloc A.PSome, List.map snd i))], f ve);
        ([dumloc (A.Pref (dumloc A.PNone, []))], f ne)
      ] MKbasic

    | Mmatchor (x, lid, le, rid, re) ->
      A.ematchwith (f x) [
        ([dumloc (A.Pref (dumloc A.PLeft,  List.map snd lid))], f le);
        ([dumloc (A.Pref (dumloc A.PRight, List.map snd rid))], f re)
      ] MKbasic

    | Mmatchlist (x, hid, tid, hte, ee)      ->
      A.ematchwith (f x) [
        ([dumloc (A.Pref (dumloc A.PCons, [snd hid; snd tid]))], f hte);
        ([dumloc (A.Pref (dumloc A.PNil,  []))], f ee)
      ] A.MKbasic

    | Mternarybool   (c, a, b)               -> A.eternary (f c) (f a) (f b)
    | Mternaryoption (c, a, b)               -> A.eternary (f c) (f a) (f b)
    | Mfold (e, i, l)                        -> A.efold (f e) (snd i) (f l)
    | Mmap (e, i, l)                         -> A.emap (f e) (snd i) (f l)
    | Mexeclambda (l, a)                     -> f_app "exec_lambda"  [f l; f a]
    | Mapplylambda (l, a)                    -> f_app "apply_lambda" [f l; f a]


    (* composite type constructors *)

    | Mleft (t, x)    -> A.eleft (ft t) (f x)
    | Mright (t, x)   -> A.eright (ft t) (f x)
    | Mnone           -> let a = match fst mt.type_ with | Toption ty -> Some (ft ty) | _ -> None in A.eoption (ONone a)
    | Msome v         -> A.eoption (OSome (f v))
    | Mtuple l        -> A.etuple (List.map f l)
    | Masset l        -> begin
        let extract_asset_name (ty : M.type_) =
          let an =
            match fst ty with
            | Tasset an -> an
            | _ -> assert false
          in
          an
        in
        let get_asset_value an : string list =
          let asset = M.Utils.get_asset model an in
          (* Format.eprintf "%a@\n" M.pp_mident asset.name; *)
          (* let lvalues = List.filter (fun (x : M.asset_item) -> if List.exists (fun a -> String.equal (M.unloc_mident x.name) a) asset.keys then false else true ) asset.values in *)
          let res = List.map (fun (x : M.asset_item) -> M.unloc_mident x.name) asset.values in
          (* List.iter (Format.eprintf "%s@\n") res; *)
          res
        in
        let an = extract_asset_name mt.type_ in let lnames = get_asset_value an in A.erecord (List.map2 (fun x y -> (Some (A.ValueAssign, dumloc x), f y)) lnames l)
      end
    | Massets l       -> A.earray (List.map f l)
    | Mlitset l       -> let g : A.expr -> A.expr = match as_set mt.type_ with | Some ty -> (fun x -> f_app "make_set" ~ts:[ft ty] [x]) | _ -> (fun x -> x) in g (A.earray (List.map f l))
    | Mlitlist l      -> let g : A.expr -> A.expr = match as_list mt.type_ with | Some ty -> (fun x -> f_app "make_list" ~ts:[ft ty] [x]) | _ -> (fun x -> x) in g (A.earray (List.map f l))
    | Mlitmap (b, l)  -> let g : A.expr -> A.expr = match as_map mt.type_ with | Some (kt, vt) -> (fun x -> f_app (match b with | MKMap -> "make_map" | MKBigMap -> "make_big_map" | MKIterableBigMap -> "make_iterable_big_map") ~ts:[ft kt; ft vt] [x]) | _ -> (fun x -> x) in g (A.earray (List.map (fun (x, y) -> A.etuple [f x; f y]) l))
    | Mlitrecord l    -> A.erecord (List.map (fun (id, x) -> (Some (A.ValueAssign, dumloc id), f x)) l)
    | Mlitevent  l    -> A.erecord (List.map (fun (id, x) -> (Some (A.ValueAssign, dumloc id), f x)) l)
    | Mlambda (rt, id, at, e) -> A.elambda (Some (ft at)) (snd id) (Some (ft rt)) (f e)
    | Mlambda_michelson (it, rt, body) -> A.elambda_michelson (ft it) (ft rt) (Micheline_tools.obj_to_micheline_t body)
    | Mmicheline_expr (t, m, a) -> A.emicheline_expr (ft t) (Micheline_tools.obj_to_micheline_t m) (List.map f a)

    (* access *)

    | Mdot (e, i)                -> A.edot (f e) (mident_to_sid i)
    | Mdotassetfield (an, k, fn) -> A.edot (A.esqapp (A.eterm (snd an)) (f k)) (mident_to_sid fn)
    | Mquestionoption (e, i)     -> A.equestiondot (f e) (mident_to_sid i)


    (* comparison operators *)

    | Mequal (_t, l, r)  -> A.eapp (Foperator (dumloc (A.Cmp Equal))) [f l; f r]
    | Mnequal (_t, l, r) -> A.eapp (Foperator (dumloc (A.Cmp Nequal))) [f l; f r]
    | Mgt (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Gt))) [f l; f r]
    | Mge (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Ge))) [f l; f r]
    | Mlt (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Lt))) [f l; f r]
    | Mle (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Le))) [f l; f r]
    | Mmulticomp (_e, _l)  -> assert false


    (* arithmetic operators *)

    | Mand         (l, r) -> A.eapp (A.Foperator (dumloc (A.Logical A.And)))       [f l; f r]
    | Mor          (l, r) -> A.eapp (A.Foperator (dumloc (A.Logical A.Or)))        [f l; f r]
    | Mgreedyand   (l, r) -> f_app "greedy_and" [f l; f r]
    | Mgreedyor    (l, r) -> f_app "greedy_or"  [f l; f r]
    | Mxor         (l, r) -> A.eapp (A.Foperator (dumloc (A.Logical A.Xor)))       [f l; f r]
    | Mnot          e     -> A.eapp (A.Foperator (dumloc (A.Unary A.Not)))         [f e]
    | Mplus        (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.Plus)))        [f l; f r]
    | Mminus       (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.Minus)))       [f l; f r]
    | Mmult        (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.Mult)))        [f l; f r]
    | Mdivrat      (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.DivRat)))      [f l; f r]
    | Mdiveuc      (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.DivEuc)))      [f l; f r]
    | Mmodulo      (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.Modulo)))      [f l; f r]
    | Mdivmod      (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.DivMod)))      [f l; f r]
    | Muminus       e     -> A.eapp (A.Foperator (dumloc (A.Unary A.Uminus)))      [f e]
    | MthreeWayCmp (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.ShiftLeft)))   [f l; f r]
    | Mshiftleft   (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.ShiftLeft)))   [f l; f r]
    | Mshiftright  (l, r) -> A.eapp (A.Foperator (dumloc (A.Arith A.ShiftRight)))  [f l; f r]
    | Msubnat      (l, r) -> f_app "sub_nat"   [f l; f r]
    | Msubmutez    (l, r) -> f_app "sub_mutez" [f l; f r]

    (* asset api effect *)

    | Maddasset       (an, i)             -> A.emethod (MKexpr (A.eterm (dumloc an))) (dumloc "add") [f i]
    | Mputsingleasset (an, i)             -> A.emethod (MKexpr (A.eterm (dumloc an))) (dumloc "put") [f i]
    | Mputasset       (_an, _k, _v)          -> assert false
    | Maddfield       (an, fn, c, i)      -> A.emethod (MKexpr (A.edot (A.esqapp (A.eterm (dumloc an)) (f c)) (id_to_sid fn))) (dumloc "add") [f i]
    | Mremoveasset    (an, i)             -> A.emethod (MKexpr (A.eterm (dumloc an))) (dumloc "remove") [f i]
    | Mremovefield    (an, fn, c, i)      -> A.emethod (MKexpr (A.edot (A.esqapp (A.eterm (dumloc an)) (f c)) (id_to_sid fn))) (dumloc "remove") [f i]
    | Mremoveall      (an, ck)            -> A.emethod (MKexpr (to_ck2 an ck)) (dumloc "remove_all") []
    | Mremoveif       (an, ck, _la, lb, _a) -> A.emethod (MKexpr (to_ck2 an ck)) (dumloc "remove_if") [f lb]
    | Mclear          (an, ck)            -> A.emethod (MKexpr (to_ck an ck)) (dumloc "clear") []
    | Mset            (_c, _l, _k, _v)       -> assert false
    | Mupdate         (an, k, l)          -> A.emethod (MKexpr (A.eterm (dumloc an))) (dumloc "update") [f k; A.erecord (List.map (fun (id, op, x) : A.record_item -> (Some (to_assign_operator op, snd id), f x)) l)]
    | Mupdateall      (an, ck, l)         -> A.emethod (MKexpr (to_ck an ck)) (dumloc "update_all") [A.erecord (List.map (fun (id, op, x) : A.record_item -> (Some (to_assign_operator op, snd id), f x)) l)]
    | Maddupdate      (an, ck, k, l)      -> A.emethod (MKexpr (to_ck an ck)) (dumloc "add_update") [f k; A.erecord (List.map (fun (id, op, x) : A.record_item -> (Some (to_assign_operator op, snd id), f x)) l)]
    | Mputremove      (an, ck, k, v)      -> A.emethod (MKexpr (to_ck an ck)) (dumloc "put_remove") [f k; f v]


    (* asset api expression *)

    | Mget      (an, ck, k)              -> A.esqapp (to_ck an ck) (f k)
    | Mgetsome  (an, ck, k)              -> A.esqapp (to_ck an ck) (f k)
    | Mselect   (an, ck, _la, lb, _a)    -> A.emethod (MKexpr (to_ck an ck)) (dumloc "select")   [f lb]
    | Msort     (an, ck, _l)             -> A.emethod (MKexpr (to_ck an ck)) (dumloc "sort")     []
    | Mcontains (an, ck, i)              -> A.emethod (MKexpr (to_ck an ck)) (dumloc "contains") [f i]
    | Mnth      (an, ck, i)              -> A.emethod (MKexpr (to_ck an ck)) (dumloc "nth")      [f i]
    | Mcount    (an, ck)                 -> A.emethod (MKexpr (to_ck an ck)) (dumloc "count")    []
    | Msum      (an, ck, p)              -> A.emethod (MKexpr (to_ck an ck)) (dumloc "sum")      [f p]
    | Mhead     (an, ck, i)              -> A.emethod (MKexpr (to_ck an ck)) (dumloc "head")     [f i]
    | Mtail     (an, ck, i)              -> A.emethod (MKexpr (to_ck an ck)) (dumloc "tail")     [f i]


    (* utils *)

    | Mcast (_src, _dst, v)    -> f v
    | Mtupleaccess (x, k)      -> A.esqapp (f x) (A.ebnat k)
    | Mrecupdate (x, l)        -> A.erecupdate (f x) (List.map (fun (id, x) -> (dumloc id, f x)) l)
    | Mmakeasset (_an, _k, _v) -> assert false
    | Mtocontainer _an         -> assert false
    | Mglobal_constant (t, v)  -> f_app "global_constant" ~ts:[ft t] [f v]

    (* set api expression *)

    | Msetadd (_t, c, a)                  -> f_app "add"      [f c; f a]
    | Msetremove (_t, c, a)               -> f_app "remove"   [f c; f a]
    | Msetupdate (_t, c, v, b)            -> f_app "update"   [f c; f b; f v]
    | Msetcontains (_t, c, a)             -> f_app "contains" [f c; f a]
    | Msetlength (_t, c)                  -> f_app "length"   [f c]
    | Msetfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* set api instruction *)

    | Msetinstradd    _                   -> assert false
    | Msetinstrremove (_, _ck, _a)        -> assert false


    (* list api expression *)

    | Mlistprepend (_, c, a)               -> f_app "prepend"  [f c; f a]
    | Mlistlength (_, c)                   -> f_app "length"   [f c]
    | Mlistcontains (_, c, a)              -> f_app "contains" [f c; f a]
    | Mlistnth (_, c, a)                   -> f_app "nth"      [f c; f a]
    | Mlisthead (_, c, a)                  -> f_app "head"     [f c; f a]
    | Mlisttail (_, c, a)                  -> f_app "tail"     [f c; f a]
    | Mlistreverse (_, l)                  -> f_app "reverse"  [f l]
    | Mlistconcat (_, l, m)                -> f_app "concat"   [f l; f m]
    | Mlistfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* list api instruction *)

    | Mlistinstrprepend _                  -> assert false
    | Mlistinstrconcat  _                  -> assert false

    (* map api expression *)

    | Mmapput (_, _, _, c, k, v)                  -> f_app "put"      [f c; f k; f v]
    | Mmapremove (_, _, _, c, k)                  -> f_app "remove"   [f c; f k]
    | Mmapupdate (_, _, _, c, k, v)               -> f_app "update"   [f c; f k; f v]
    | Mmapget (_, _, _, c, k, _an)                -> A.esqapp (f c) (f k)
    | Mmapgetopt (_, _, _, c, k)                  -> A.esqapp (f c) (f k)
    | Mmapcontains (_, _, _, c, k)                -> f_app "contains" [f k; f c]
    | Mmaplength (_, _, _, c)                     -> f_app "length"   [f c]
    | Mmapfold (_, _t, _ik, _iv, _ia, _c, _a, _b) -> assert false


    (* map api instruction *)

    | Mmapinstrput    (_, _, _, _c, _k, _v)       -> assert false
    | Mmapinstrremove (_, _, _, _c, _k)           -> assert false
    | Mmapinstrupdate (_, _, _, _c, _k, _v)       -> assert false


    (* builtin functions *)

    | Mmax (l, r)                 -> f_app "max" [f l; f r]
    | Mmin (l, r)                 -> f_app "min" [f l; f r]
    | Mabs a                      -> f_app "abs" [f a]
    | Mconcat (x, y)              -> f_app "concat" [f x; f y]
    | Mconcatlist x               -> f_app "concat" [f x]
    | Mslice (x, s, e)            -> f_app "slice" [f x; f s; f e]
    | Mlength x                   -> f_app "length" [f x]
    | Misnone x                   -> f_app "is_none" [f x]
    | Missome x                   -> f_app "is_some" [f x]
    | Minttonat x                 -> f_app "int_to_nat" [f x]
    | Mfloor  x                   -> f_app "floor" [f x]
    | Mceil   x                   -> f_app "ceil" [f x]
    | Mnattostring x              -> f_app "nat_to_string" [f x]
    | Mbytestonat x               -> f_app "bytes_to_nat" [f x]
    | Mnattobytes x               -> f_app "nat_to_bytes" [f x]
    | Mbytestoint x               -> f_app "bytes_to_int" [f x]
    | Minttobytes x               -> f_app "int_to_bytes" [f x]
    | Mpack x                     -> f_app "pack" [f x]
    | Munpack (t, x)              -> A.eunpack (ft t) (f x)
    | Msetdelegate x              -> f_app "set_delegate" [f x]
    | Mkeyhashtocontract x        -> f_app "key_hash_to_contract" [f x]
    | Mcontracttoaddress x        -> f_app "contract_to_address" [f x]
    | Maddresstocontract (t, x)   -> f_app "address_to_contract" [f x] ~ts:[ft t]
    | Mkeytoaddress x             -> f_app "key_to_address" [f x]
    | Msimplify_rational x        -> f_app "simplify_rational" [f x]
    | Mget_numerator x            -> f_app "get_numerator" [f x]
    | Mget_denominator x          -> f_app "get_denominator" [f x]
    | Misimplicitaddress x        -> f_app "is_implicit_address" [f x]
    | Mexp_horner (x, s)          -> f_app "exp_horner" [f x; f s]


    (* crypto functions *)

    | Mblake2b         x        -> f_app "blake2b" [f x]
    | Msha256          x        -> f_app "sha256" [f x]
    | Msha512          x        -> f_app "sha512" [f x]
    | Msha3            x        -> f_app "sha3" [f x]
    | Mkeccak          x        -> f_app "keccak" [f x]
    | Mkeytokeyhash    x        -> f_app "key_to_key_hash" [f x]
    | Mchecksignature (k, s, x) -> f_app "check_signature" [f k; f s; f x]


    (* voting *)

    | Mtotalvotingpower         -> f_cst "total_voting_power"
    | Mvotingpower x            -> f_app "voting_power" [f x]


    (* ticket *)

    | Mcreateticket (x, a)   -> f_app "create_ticket" [f x; f a]
    | Mreadticket x          -> f_app "read_ticket"   [f x]
    | Msplitticket (x, a, b) -> f_app "split_ticket"  [f x; f a; f b]
    | Mjointickets (x, y)    -> f_app "join_tickets"  [f x; f y]


    (* sapling *)

    | Msapling_empty_state       n  -> f_app "sapling_empty_state" [A.ebnat (Big_int.big_int_of_int n)]
    | Msapling_verify_update (s, t) -> f_app "sapling_verify_update" [f s; f t]


    (* bls curve *)

    | Mpairing_check x -> f_app "pairing_check" [f x]


    (* timelock *)

    | Mopen_chest (a, b, c) -> f_app "open_chest" [f a; f b; f c]


    (* constants *)

    | Mnow           -> f_cst A.cst_now
    | Mtransferred   -> f_cst A.cst_transferred
    | Mcaller        -> f_cst A.cst_caller
    | Mbalance       -> f_cst A.cst_balance
    | Msource        -> f_cst A.cst_source
    | Mselfaddress   -> f_cst A.cst_self_address
    | Mselfchainid   -> f_cst A.cst_self_chain_id
    | Mmetadata      -> f_cst A.cst_metadata
    | Mlevel         -> f_cst A.cst_level
    | Mminblocktime  -> f_cst A.cst_min_block_time


    (* variable *)

    | Mvar (_an, Vassetstate _k) -> assert false
    | Mvar(v, Vstorevar)         -> A.eterm (snd v)
    | Mvar(v, Vstorecol)         -> A.eterm (snd v)
    | Mvar(v, Vlocal)            -> A.eterm (snd v)
    | Mvar(v, Vparam)            -> A.eterm (snd v)
    | Mvar(_v, Vfield)           -> assert false
    | Mvar(_, Vthe)              -> A.eterm (dumloc "the")
    | Mvar(_, Vstate)            -> A.eterm (dumloc "state")
    | Mvar(v, Vparameter)        -> A.eterm (snd v)
    | Menumval (id, args, _e) -> begin
        match args with
        | [] -> A.eterm (snd id)
        | args  -> A.eapp (A.Fident (mident_to_sid id)) (List.map f args)
      end

    (* rational *)

    | Mrateq (_l, _r)         -> assert false
    | Mratcmp (_op, _l, _r)   -> assert false
    | Mratarith (_op, _l, _r) -> assert false
    | Mratuminus _v           -> assert false
    | Mrattez (_c, _t)        -> assert false
    | Mnattoint e             -> f e
    | Mnattorat e             -> f e
    | Minttorat e             -> f e
    | Mratdur (_c, _t)        -> assert false


    (* utils *)

    | Minttodate         x -> f_app "int_to_date" [f x]
    | Mmuteztonat        x -> f_app "mutez_to_nat" [f x]
    | Mdmatchor          _ -> assert false
    | Mdmatchoption      _ -> assert false
  in

  let for_decl (x : M.decl_node) : A.declaration =
    match x with
    | Dvar {name; type_; kind; default; _} -> begin
        let k = match kind with | VKconstant -> A.VKconstant | VKvariable -> A.VKvariable in
        A.mk_variable (A.mk_variable_decl ?dv:(Option.map for_expr default) (snd name) (for_type type_) k)
      end
    | Denum {name; values; _} -> begin
        let ek =
          match M.unloc_mident name with
          | "state" -> A.EKstate
          | v -> A.EKenum (dumloc v)
        in
        let l : (A.lident * A.type_t list * A.enum_option list) list =
          List.map (fun (x : M.enum_item) -> (snd x.name, List.map for_type x.args, [])) values
        in
        A.mk_enum ek l
      end
    | Dasset dasset ->  begin
        let for_field (x : M.asset_item) : A.field = dumloc (A.Ffield (snd x.name, for_type x.type_, Option.map for_expr x.default)) in
        let a : A.lident = snd dasset.name in
        let b : A.field list = List.map for_field dasset.values in
        let c : A.asset_option list =
          []
          |> (fun accu -> let a = match dasset.map_kind with | M.MKMap -> A.MKMap | M.MKBigMap -> A.MKBigMap | M.MKIterableBigMap -> A.MKIterableBigMap in A.AOtoMapKind a :: accu )
          |> (fun accu -> match dasset.keys with | [] -> accu | l -> A.AOidentifiedby (List.map dumloc l)::accu )
          |> (fun accu -> (List.map (fun x -> A.AOsortedby (snd x)) dasset.sort) @ accu)
        in
        let d : A.asset_post_option list =
          match dasset.init with
          | IAident id -> [A.APOinit (A.IAident id)]
          | IAliteral xs -> let l = List.map for_expr xs in [A.APOinit (A.IAliteral l)]
        in
        let e : A.asset_operation option = None in
        A.mk_asset (a, b, c, d, e)
      end
    | Drecord drecord -> begin
        let for_field (x : M.record_field) : A.field = dumloc (A.Ffield (snd x.name, for_type x.type_, None)) in
        let a : A.lident = snd drecord.name in
        let b : A.field list = List.map for_field drecord.fields in
        let c : A.expr option = None in
        let rd : A.record_decl = (a, b, c) in
        A.mk_record rd
      end
    | Devent  drecord ->  begin
        let for_field (x : M.record_field) : A.field = dumloc (A.Ffield (snd x.name, for_type x.type_, None)) in
        let a : A.lident = snd drecord.name in
        let b : A.field list = List.map for_field drecord.fields in
        let c : A.expr option = None in
        let rd : A.record_decl = (a, b, c) in
        A.mk_event rd
      end
  in

  let for_storage_item (si : M.storage_item) : A.declaration =
    let id = si.id in
    let t  = for_type si.typ in
    let dv = for_expr si.default in
    match si.model_type with
    | MTvar       -> A.mk_variable (A.mk_variable_decl ~dv:dv (snd id) t VKvariable)
    | MTconst     -> A.mk_variable (A.mk_variable_decl ~dv:dv (snd id) t VKconstant)
    | MTasset  id -> A.mk_variable (A.mk_variable_decl ~dv:dv (snd id) t VKvariable)
    | MTstate     -> A.mk_variable (A.mk_variable_decl ~dv:dv (dumloc "state") t VKvariable)
    | MTenum   id -> A.mk_variable (A.mk_variable_decl ~dv:dv (dumloc id) t VKvariable)
  in

  let for_fun (f : M.function_node) : A.declaration =
    match f with
    | Function (fs, t) -> begin
        let sf : A.s_function = {
          name  = snd fs.name;
          args  = List.map (fun (id, ty, _dv) -> (snd id, for_type ty)) fs.args;
          ret_t = (match t with | Void -> None | Typed t -> Some (for_type t));
          body  = for_expr fs.body;
          view  = false;
          view_visibility = A.VVnone;
        } in
        A.mk_function sf
      end
    | Getter (fs, t) -> begin
        let id = fs.name in
        let body = for_expr fs.body in
        let args = List.map (fun (id, t, _) -> (id, for_type t) ) fs.args in

        let ep = A.mk_entry_properties () in
        let ed = A.mk_getter_decl (snd id) (List.map (fun (x, y) -> (snd x, y)) args) (for_type t) ep body in

        A.mk_getter ed
      end
    | View (fs, rt, vv) -> begin
        let sf : A.s_function = {
          name  = snd fs.name;
          args  = List.map (fun (id, ty, _dv) -> (snd id, for_type ty)) fs.args;
          ret_t = Some (for_type rt);
          body  = for_expr fs.body;
          view  = true;
          view_visibility = match vv with | VVonchain -> A.VVonchain | VVoffchain -> A.VVoffchain | VVonoffchain -> A.VVonoffchain;
        } in
        A.mk_function sf
      end
    | Entry fs -> begin
        let id = fs.name in
        let body = for_expr fs.body in
        let args = List.map (fun (id, t, _) -> (id, for_type t) ) fs.args in

        let ep = A.mk_entry_properties () in
        let ed = A.mk_entry_decl ~args:(List.map (fun (x, y) -> (snd x, y)) args) (snd id) ep ~body in

        A.mk_entry ed
      end
  in

  let decls =
    List.map for_decl model.decls
    @ List.map for_storage_item model.storage
    @ List.map for_fun model.functions
  in

  let parameters : A.parameters =
    match model.parameters with
    | [] -> None
    | ps -> begin
        let parameters : A.parameter list = List.map (fun (x : M.parameter) ->
            dumloc (snd x.name, for_type x.typ, Option.map for_expr x.default, x.const)) ps
        in
        Some (dumloc parameters)
      end
  in

  A.mk_archetype () ~decls:((A.mk_darchetype ?parameters model.name)::decls)
