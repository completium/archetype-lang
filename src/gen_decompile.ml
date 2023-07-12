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
}

let mk_env ?(name="") _ : env = { name }

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
  let env = mk_env ~name:name () in

  let input =
    if match ijson with | Some v -> v | _ -> !Options.opt_json  then
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
    let to_data = T.to_data in

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
  val decompile : T.michelson -> T.dcode
end = struct
  open Michelson

  (* ------------------------------------------------------------------ *)
  let gen () = Oo.id (object end)

  type dvar = [
    | `VLocal  of int
    | `VGlobal of string
  ]

  let vlocal  () : dvar = `VLocal (gen ())
  let vglobal x  : dvar = `VGlobal x

  let as_vlocal (x : dvar) =
    match x with `VLocal i -> i | _ -> assert false

  let rec pp_rstack1 fmt (x : rstack1) =
    match x with
    | `VLocal  i -> Format.fprintf fmt "#%d" i
    | `VGlobal n -> Format.fprintf fmt "%s" n

    | `Paired(x, y) ->
      Format.fprintf fmt "(%a, %a)" pp_rstack1 x pp_rstack1 y

  let pp_rstack fmt (x : rstack) =
    Format.fprintf fmt "[%a]" (Printer_tools.pp_list ", " pp_rstack1) x

  let as_dvar (x : rstack1) : dvar =
    match x with
    | #dvar as x -> x
    | _ -> assert false

  let rec write_var (e : dexpr) (x : rstack1) =
    match x, e.node with
    | #dvar as x, _ -> [DIAssign (x, e)]
    | `Paired (x1, x2), Depair (e1, e2) ->
      let a = vlocal () in
      write_var e1 (a :> rstack1)
      @ write_var e2 x2
      @ write_var (dvar a) x1
    | _ -> assert false

  let rec dexpr_of_rstack1 (x : rstack1) : dexpr =
    match x with
    | #dvar as x -> dvar x
    | `Paired (x, y) -> depair (dexpr_of_rstack1 x) (dexpr_of_rstack1 y)

  let rec merge_rstack (s1 : rstack) (s2 : rstack) =
    assert (List.length s1 = List.length s2);

    match s1, s2 with
    | (#dvar as x) :: s1, (#dvar as y) :: s2 ->
      let (is1, is2), s = merge_rstack s1 s2 in
      let a = vlocal () in
      (([DIAssign (a, dvar x)] @ is1), is2), y :: s

    | `Paired (x1, y1) :: s1, `Paired (x2, y2) :: s2 ->
      merge_rstack (x1 :: y1 :: s1) (x2 :: y2 :: s2)

    | `Paired (x1, y1) :: s1, (#dvar as xy2) :: s2 ->
      let i1 = write_var (dexpr_of_rstack1 (`Paired (x1, y1))) xy2 in
      let (is1, is2), s = merge_rstack s1 s2 in
      ((i1 @ is1), is2), `Paired (x1, y1) :: s

    | #dvar :: _, `Paired _ :: _ ->
      let (is2, is1), s = merge_rstack s2 s1 in
      (is1, is2), s

    | [], [] ->
      ([], []), []

    | _, _ -> assert false

  let merge_rstack (s1 : rstack) (s2 : rstack) =
    merge_rstack s1 s2

  let rec dptn_of_rstack1 (r : rstack1) =
    match r with
    | `Paired (r1, r2) ->
      let p1, c1 = dptn_of_rstack1 r1 in
      let p2, c2 = dptn_of_rstack1 r2 in
      (DPair (p1, p2), c1 @ c2)

    | `VLocal x ->
      (DVar x, [])

    | (`VGlobal _) as n ->
      let x = gen () in (DVar x, [DIAssign (n, dvar (`VLocal x))])

  exception UnificationFailure

  let unify_dvar (uf : UF.uf) (x : dvar) (y : dvar) =
    match x, y with
    | `VGlobal m, `VGlobal n ->
      if m <> n then raise UnificationFailure

    | `VLocal i, `VLocal j ->
      ignore (UF.union uf i j : int)

    | _, _ ->
      raise UnificationFailure

  let rec unify_dexpr (uf : UF.uf) (e1 : dexpr) (e2 : dexpr) =
    match e1.node, e2.node with
    | Dvar x, Dvar y ->
      unify_dvar uf x y

    | Depair (e1, e2), Depair (f1, f2) ->
      unify_dexpr uf e1 f1;
      unify_dexpr uf e2 f2

    | Ddata (_, d1), Ddata (_, d2) ->
      if not (cmp_data d1 d2) then
        raise UnificationFailure

    | Dfun (o1, es1), Dfun (o2, es2) ->
      if o1 <> o2 || List.length es1 <> List.length es2 then
        raise UnificationFailure;
      List.iter2 (unify_dexpr uf) es1 es2

    | _, _ ->
      raise UnificationFailure

  let unify_dinstr (uf : UF.uf) (i1 : dinstr) (i2 : dinstr) =
    match i1, i2 with
    | DIAssign (x1, e1), DIAssign (x2, e2) ->
      unify_dvar  uf x1 x2;
      unify_dexpr uf e1 e2

    | _, _ -> raise UnificationFailure

  let unify_dcode (uf : UF.uf) (is1 : dcode) (is2 : dcode) =
    if List.length is1 <> List.length is2 then
      raise UnificationFailure;
    List.iter2 (unify_dinstr uf) is1 is2

  let dvar_apply_uf (uf : UF.uf) (x : dvar) : dvar =
    match x with
    | `VGlobal _ -> x
    | `VLocal  i -> `VLocal (UF.find uf i)

  let rec dexpr_apply_uf (uf : UF.uf) (e : dexpr) : dexpr =
    match e.node with
    | Dvar x ->
      dvar (dvar_apply_uf uf x)

    | Depair (e1, e2) ->
      let e1 = dexpr_apply_uf uf e1 in
      let e2 = dexpr_apply_uf uf e2 in
      depair e1 e2

    | Ddata _ ->
      e

    | Dfun (o, es) ->
      dfun o (List.map (dexpr_apply_uf uf) es)

  let dpattern_apply_uf (_uf : UF.uf) (p : dpattern) : dpattern =
    p

  let rec dinstr_apply_uf (uf : UF.uf) (is : dinstr) : dinstr =
    match is with
    | DIAssign (x, e) ->
      DIAssign (dvar_apply_uf uf x, dexpr_apply_uf uf e)

    | DIIf (e, (c1, c2)) ->
      let e  = dexpr_apply_uf uf e  in
      let c1 = dcode_apply_uf uf c1 in
      let c2 = dcode_apply_uf uf c2 in
      DIIf (e, (c1, c2))

    | DIMatch (e, bs) ->
      let for_branch (x, ps, c) =
        let ps = List.map (dpattern_apply_uf uf) ps in
        let c  = dcode_apply_uf uf c in
        (x, ps, c) in

      let e  = dexpr_apply_uf uf e in
      let bs = List.map for_branch bs in
      DIMatch (e, bs)

    | DIFailwith e ->
      DIFailwith (dexpr_apply_uf uf e)

    | DIWhile (e, c) ->
      let e = dexpr_apply_uf uf e in
      let c = dcode_apply_uf uf c in
      DIWhile (e, c)

    | DIIter (x, e, c) ->
      let x = dvar_apply_uf  uf x in
      let e = dexpr_apply_uf uf e in
      let c = dcode_apply_uf uf c in
      DIIter (x, e, c)

    | DILoop (x, c) ->
      let x = dvar_apply_uf  uf x in
      let c = dcode_apply_uf uf c in
      DILoop (x, c)

  and dcode_apply_uf (uf : UF.uf) (is : dcode) : dcode =
    List.map (dinstr_apply_uf uf) is

  let rec rstack1_apply_uf (uf : UF.uf) (r : rstack1) : rstack1 =
    match r with
    | `Paired (r1, r2) ->
      let r1 = rstack1_apply_uf uf r1 in
      let r2 = rstack1_apply_uf uf r2 in
      `Paired (r1, r2)

    | #dvar as r ->
      (dvar_apply_uf uf r :> rstack1)

  let rstack_apply_uf (uf : UF.uf) (r : rstack) : rstack =
    List.map (rstack1_apply_uf uf) r

  (* ------------------------------------------------------------------ *)
  type decomp = {
    stack   : rstack;
    code    : dcode;
    failure : bool;
  }

  let mkdecomp ?(failure = false) stack code =
    { code; stack; failure; }

  let rec decompile_i (s : rstack) (i : code) : decomp =
    match i.node with

    (* Control structures *)

    | SEQ l -> decompile_s s l

    | IF (c1, c2) -> begin
        let { failure = f1; stack = s1; code = b1; } = decompile_s s c1 in
        let { failure = f2; stack = s2; code = b2; } = decompile_s s c2 in

        let (pr1, pr2), s =
          match f1, f2 with
          | false, false -> merge_rstack s1 s2
          | true , false -> ([], []), s2
          | false, true  -> ([], []), s1
          | true , true  -> assert false in
        let x = vlocal () in
        mkdecomp ((x :> rstack1) :: s) [DIIf (dvar x, (pr1 @ b1, pr2 @ b2))]
      end

    (* Stack manipulation *)

    | DIG n ->
      assert (List.length s >= n + 1);
      let x, s1 = List.hd s, List.tl s in
      let s1, s2 = List.cut n s1 in
      mkdecomp (s1 @ (x :: s2)) []

    | DIP (n, c) ->
      assert (List.length s >= n);
      let s1, s2 = List.cut n s in
      let { failure; stack = s2; code = ops; } = decompile_s s2 c in
      mkdecomp ~failure (s1 @ s2) ops

    | DROP n ->
      let pre = List.init n (fun _ -> (vlocal () :> rstack1)) in
      mkdecomp (pre @ s) []

    | DUG n ->
      assert (List.length s >= n + 1);
      let s1, s2 = List.cut n s in
      let x, s2 = List.hd s2, List.tl s2 in
      mkdecomp (x :: (s1 @ s2)) []

    | DUP ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      let a = vlocal () in
      let wri1 = write_var (dvar a) x in
      let wri2 = write_var (dvar a) y in
      mkdecomp ((a :> rstack1) :: s) (wri1 @ wri2)

    | DUP_N n ->
      assert (1 <= n);
      let x, s = List.pop s in
      let pre, s = List.split_at (n-1) s in
      let y, s = List.pop s in
      let a = vlocal () in
      let wri1 = write_var (dvar a) x in
      let wri2 = write_var (dvar a) y in
      mkdecomp (pre @ (a :> rstack1) :: s) (wri1 @ wri2)

    | PUSH (t, d) ->
      let x, s = List.pop s in
      let wri = write_var (ddata t d) x in
      mkdecomp s wri

    | SWAP ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      mkdecomp (y :: x :: s) []

    (* Arthmetic operations *)

    | ABS      -> decompile_op s (`Uop Uabs     )
    | ADD      -> decompile_op s (`Bop Badd     )
    | COMPARE  -> decompile_op s (`Bop Bcompare )
    | EDIV     -> decompile_op s (`Bop Bediv    )
    | EQ       -> decompile_op s (`Uop Ueq      )
    | GE       -> decompile_op s (`Uop Uge      )
    | GT       -> decompile_op s (`Uop Ugt      )
    | INT      -> decompile_op s (`Uop Uint     )
    | ISNAT    -> decompile_op s (`Uop Uisnat   )
    | LE       -> decompile_op s (`Uop Ule      )
    | LSL      -> decompile_op s (`Bop Blsl     )
    | LSR      -> decompile_op s (`Bop Blsr     )
    | LT       -> decompile_op s (`Uop Ult      )
    | MUL      -> decompile_op s (`Bop Bmul     )
    | NEG      -> decompile_op s (`Uop Uneg     )
    | NEQ      -> decompile_op s (`Uop Une      )
    | SUB      -> decompile_op s (`Bop Bsub     )


    (* Boolean operations *)

    | AND     -> decompile_op s (`Bop Band )
    | NOT     -> decompile_op s (`Uop Unot )
    | OR      -> decompile_op s (`Bop Bor  )
    | XOR     -> decompile_op s (`Bop Bxor )


    (* Cryptographic operations *)

    | BLAKE2B          -> decompile_op s (`Uop Ublake2b         )
    | CHECK_SIGNATURE  -> decompile_op s (`Top Tcheck_signature )
    | HASH_KEY         -> decompile_op s (`Uop Uhash_key        )
    | SHA256           -> decompile_op s (`Uop Usha256          )
    | SHA512           -> decompile_op s (`Uop Usha512          )


    (* Blockchain operations *)

    | ADDRESS            -> decompile_op s (`Zop  Zaddress          )
    | AMOUNT             -> decompile_op s (`Zop  Zamount           )
    | BALANCE            -> decompile_op s (`Zop  Zbalance          )
    | CHAIN_ID           -> decompile_op s (`Zop  Zchain_id         )
    | CONTRACT (t, a)    -> decompile_op s (`Uop (Ucontract (t, a)) )
    | CREATE_CONTRACT _  -> assert false
    | IMPLICIT_ACCOUNT   -> decompile_op s (`Uop (Uimplicitaccount) )
    | NOW                -> decompile_op s (`Zop Znow               )
    | SELF a             -> decompile_op s (`Zop (Zself a)          )
    | SENDER             -> decompile_op s (`Zop Zsender            )
    | SET_DELEGATE       -> decompile_op s (`Uop Usetdelegate       )
    | SOURCE             -> decompile_op s (`Zop Zsource            )
    | TRANSFER_TOKENS    -> decompile_op s (`Top Ttransfer_tokens   )


    (* Operations on data structures *)

    | CAR ->
      let x, s = List.pop s in
      mkdecomp (`Paired (x, (vlocal () :> rstack1)) :: s) []
    | CDR ->
      let y, s = List.pop s in
      mkdecomp (`Paired ((vlocal () :> rstack1), y) :: s) []
    | CONCAT               -> decompile_op s (`Bop Bconcat               )
    | CONS                 -> decompile_op s (`Bop Bcons                 )
    | EMPTY_BIG_MAP (k, v) -> decompile_op s (`Zop (Zemptybigmap (k, v)) )
    | EMPTY_MAP (k, v)     -> decompile_op s (`Zop (Zemptymap (k, v))    )
    | EMPTY_SET t          -> decompile_op s (`Zop (Zemptyset t)         )
    | GET                  -> decompile_op s (`Bop Bget                  )
    | LEFT t               -> decompile_op s (`Uop (Uleft t)             )
    | MAP _cs              -> assert false
    | MEM                  -> decompile_op s (`Bop Bmem                  )
    | NIL t                -> decompile_op s (`Zop (Znil t)              )
    | NONE t               -> decompile_op s (`Zop (Znone t)             )
    | PACK                 -> decompile_op s (`Uop Upack                 )
    | PAIR                 ->  begin
        let x, s = List.pop s in

        match x with
        | `Paired (x1, x2) ->
          mkdecomp (x1 :: x2 :: s) []

        | #dvar as v ->
          let x1 = vlocal () in
          let x2 = vlocal () in
          let op = DIAssign (v, dfun (`Bop Bpair) [dvar x1; dvar x2]) in
          mkdecomp ((x1 :> rstack1) :: (x2 :> rstack1) :: s) [op]
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
              let x1 = vlocal () in
              let x2 = vlocal () in
              let op = DIAssign (v, dfun (`Bop Bpair) [dvar x1; dvar x2]) in
              (x1 :> rstack1), (x2 :> rstack1) :: s, [op]
          in

          let s, ops' = doit s (n-1) in

          x1 :: s, ops @ ops' in

      let s, ops = doit s n in
      mkdecomp s ops

    | RIGHT t              -> decompile_op s (`Uop (Uright t)  )
    | SIZE                 -> decompile_op s (`Uop Usize       )
    | SLICE                -> decompile_op s (`Top Tslice      )
    | SOME                 -> decompile_op s (`Uop (Usome)     )
    | UNIT                 -> decompile_op s (`Zop (Zunit)     )
    | UNPACK t             -> decompile_op s (`Uop (Uunpack t) )
    | UPDATE               -> decompile_op s (`Top Tupdate     )

    | UPDATE_N n -> begin
        let x, s = List.pop s in

        let rec doit b n va (a : rstack1) =
          match n with
          | 0 when not b ->
            va
          | 0 ->
            let x2 = vlocal () in
            `Paired (a, (x2 :> rstack1))
          | _ ->
            let x1 = vlocal () in
            let a  = doit b (n-1) va a in
            `Paired ((x1 :> rstack1), a) in

        let a = vlocal () in
        let y = doit (n mod 2 <> 0) (n / 2) (a :> rstack1) x in

        let wri1 = write_var (dvar a) x in
        let wri2 = write_var (dvar a) y in

        mkdecomp ((a :> rstack1) :: x :: s) (wri1 @ wri2)
      end

    | GET_N n -> begin
        let x, s = List.pop s in

        let rec doit b n (a : rstack1) =
          match n with
          | 0 when not b ->
            a
          | 0 ->
            let x2 = vlocal () in
            `Paired (a, (x2 :> rstack1))
          | _ ->
            let x1 = vlocal () in
            let a  = doit b (n-1) a in
            `Paired ((x1 :> rstack1), a) in

        mkdecomp ((doit (n mod 2 <> 0) (n / 2) x) :: s) []
      end

    (* Other *)

    | UNPAIR ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      mkdecomp (`Paired (x, y) :: s) []

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
      mkdecomp (top :: s) []

    | SELF_ADDRESS -> decompile_op s (`Zop Zself_address)

    | ITER cs ->
      let { failure; stack = s; code = bd1 } = decompile_s s cs in

      if failure then
        assert false;

      (* FIXME: iterate this ad nauseum: we need to find a fixpoint *)

      let x1, s1  = List.pop s in
      let bd2 = (decompile_s s1 cs).code in

      let uf = UF.create () in

      unify_dcode uf bd1 bd2;

      let bd   = dcode_apply_uf uf bd1 in
      let s    = rstack_apply_uf uf (x1 :: s1) in
      let x, s = List.pop s in

      let xs = vlocal () in
      mkdecomp
        ((xs :> rstack1) :: s)
        [DIIter (as_dvar x, dvar xs, bd)]

    | LOOP cs ->
      let cond = vlocal () in

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

    | IF_CONS (c1, c2) ->
      compile_match s [("cons", 1), c1; ("nil", 0), c2]

    | IF_LEFT (c1, c2) ->
      compile_match s [("left", 1), c1; ("right", 1), c2]

    | IF_NONE (c1, c2) ->
      compile_match s [("none", 0), c1; ("some", 1), c2]

    | FAILWITH ->
      let s    = List.map (fun _ -> (vlocal () :> rstack1)) s in
      let x, _ = List.pop s in
      mkdecomp ~failure:true s [DIFailwith (dexpr_of_rstack1 x)]

    | _ -> (Format.eprintf "%a@\n" pp_code i; assert false)

  (***************************************************************************************** *)

  and decompile_op (s : rstack) (op : g_operator) =
    let n = match op with | `Zop _ -> 0 | `Uop _ -> 1 | `Bop _ -> 2 | `Top _ -> 3 in
    let x, s = List.pop s in
    let args = List.init n (fun _ -> vlocal ()) in
    mkdecomp
      ((args :> rstack) @ s)
      (write_var (dfun op (List.map (fun v -> dvar v) args)) x)

  and compile_match (s : rstack) (bs : ((string * int) * code list) list) =
    let sc, subs = List.split (List.map (fun ((name, n), b) ->
        (* FIXME: check for failures *)
        let { stack = sc; code = bc } = decompile_s s b in
        assert (List.length sc >= n);
        let p, sc = List.cut n sc in
        let p, dp = List.split (List.map dptn_of_rstack1 p) in
        (sc, (name, p, List.flatten dp @ bc))) bs) in

    let x  = vlocal () in
    let sc = List.fold_left (fun x y -> snd (merge_rstack x y)) (List.hd sc) (List.tl sc) in

    mkdecomp ((x :> rstack1) :: sc) [DIMatch (dvar x, subs)]

  and decompile_s (s : rstack) (c : code list) : decomp =
    let (failure, stack), code = List.fold_left_map (fun (oldfail, stack) code ->
        let { failure; stack; code; } = decompile_i stack code in
        (oldfail || failure, stack), code) (false, s) (List.rev c) in

    { failure; stack; code = List.flatten (List.rev code); }

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
    | Dvar (`VLocal x) ->
      Option.get_dfl e (Mint.find_opt x env)

    | Dvar _ ->
      e

    | Depair (e1, e2) ->
      let e1 = expr_cttprop env e1 in
      let e2 = expr_cttprop env e2 in
      depair e1 e2

    | Ddata _ ->
      e

    | Dfun (op, es) ->
      let es = List.map (expr_cttprop env) es in
      dfun op es

  type cttenv = dexpr Mint.t

  let cttenv_remove_wr (env : cttenv) (wr : Sdvar.t) =
    Sdvar.fold (fun x env ->
        match x with
        | `VLocal  i -> Mint.remove i env
        | `VGlobal _ -> env) wr env

  let rec instr_cttprop (env0 : cttenv) (code : dinstr) =
    match code with
    | DIAssign ((`VLocal i), {node = Dvar (`VLocal j)}) when i = j ->
      env0, []

    | DIAssign ((`VLocal i) as x, e) ->
      let env = Mint.remove i env0 in
      let e   = expr_cttprop env e in
      let env = Mint.filter (fun _ se -> not (Sdvar.mem x (expr_fv se))) env in
      let env = Mint.add i e env in

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

  (* -------------------------------------------------------------------- *)
  let rec instr_kill (keep : Sdvar.t) (instr : dinstr) =
    match instr with
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

      keep, [DIMatch (e, bs)]

    | DIFailwith e ->
      Sdvar.union keep (expr_fv e), [instr]

  and code_kill (keep : Sdvar.t) (code : dcode) =
    let keep, code = List.fold_left_map instr_kill keep (List.rev code) in
    keep, List.flatten (List.rev code)

  (* -------------------------------------------------------------------- *)
  let decompile (michelson : michelson) =
    let aty = michelson.storage in
    let pty = michelson.parameter in
    let code = let c = michelson.code in match c.node with | SEQ l -> l | _ -> [c] in

    let args prefix =
      let mkvar i : rstack1 =
        `VGlobal (Printf.sprintf "%s%d" prefix i) in

      let rec create i : _ -> rstack1 = fun (ty : T.type_) ->
        match ty with
        | { node = Tpair (_::ty::_); _} ->
          `Paired (mkvar i, create (i + 1) ty)
        | _ ->
          mkvar i
      in create 1 in

    let pst = args "args_" pty in
    let ast = args "sto_"  aty in

    let { stack = ost; code = dc; } =
      decompile_s [`Paired (`VGlobal "ops", ast)] code in

    let code =
      match ost with
      | [`Paired (px, ax)] ->
        let pr1 = write_var (dexpr_of_rstack1 pst) px in
        let pr2 = write_var (dexpr_of_rstack1 ast) ax in
        pr1 @ dc @ pr2
      | _ -> Format.eprintf "%a@." pp_rstack ost; assert false in

    (*    let _, code = code_kill Sdvar.empty code in*)
    let _, code = code_cttprop Mint.empty code in
    let _, code = code_kill Sdvar.empty code in

    code
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

  let code = Decomp_dir.decompile michelson in

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

let rec data_to_mterm ?omap_var ?t (d : T.data) : M.mterm =
  let f = data_to_mterm ?omap_var in
  let is_nat = Option.map_dfl (fun (t : T.type_) -> match t.node with | T.Tnat -> true | _ -> false) false in
  match d with
  | Dint v when is_nat t -> M.mk_bnat v
  | Dint    v        -> M.mk_bint v
  | Dstring v        -> M.mk_string v
  | Dbytes  v        -> M.mk_bytes v
  | Dunit            -> M.unit
  | Dtrue            -> M.mtrue
  | Dfalse           -> M.mfalse
  | Dpair   l        -> M.mk_pair (List.map f l)
  | Dleft    d       -> begin
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
  | Dsome   _d       -> assert false
  | Dnone            -> assert false
  | Dlist  _l        -> assert false
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

  let get_storage_list tstorage =
    let rec aux (x : T.type_) =
      match x.node, x.annotation with
      | _, Some a  -> [a, x]
      | T.Tpair [a; b], _ -> begin
          match aux a, aux b with
          | [], _
          | _, [] -> []
          | x, y  -> x @ y
        end
      | _ -> []
    in
    let r = aux tstorage in
    match r with
    | [] -> ["storage", tstorage]
    | _  -> r

  let rec get_default_value (t : T.type_) =
    let f = get_default_value in
    match t.node with
    | Tnat
    | Tint         -> T.Dint Big_int.zero_big_int
    | Tstring      -> T.Dstring ""
    | Tpair l      -> T.Dpair (List.map f l)
    | _ -> T.Dint Big_int.zero_big_int(* assert false *)

  let for_code (code : dcode) : mterm =
    let ft = for_type in

    let for_dvar (v : dvar) : ident =
      match v with
      | `VLocal  x  -> "$" ^ (string_of_int x)
      | `VGlobal id -> id
    in

    let rec for_expr (e : dexpr) : mterm =
      let f = for_expr in
      let tunknown = tunit in
      let mk_map = MKMap in
      match e.node with
      | Dvar v          -> mk_mvar (M.mk_mident (dumloc (for_dvar v))) tunit
      | Ddata (t, d)    -> data_to_mterm ~t d
      | Depair (e1, e2) -> mk_pair [for_expr e1; for_expr e2]
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
          | `Zop Zself _a,                   [] -> assert false
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
          | `Uop Uisnat,                  [ _a ] -> assert false
          | `Uop Usome,                   [ a ] -> mk_some (f a)
          | `Uop Usize,                   [ a ] -> mk_mterm (Mlistlength (tunknown, f a)) tnat
          | `Uop Upack,                   [ a ] -> mk_pack (f a)
          | `Uop Uunpack t,               [ a ] -> mk_unpack (ft t) (f a)
          | `Uop Ublake2b,                [ a ] -> mk_blake2b (f a)
          | `Uop Usha256,                 [ a ] -> mk_sha256 (f a)
          | `Uop Usha512,                 [ a ] -> mk_sha512 (f a)
          | `Uop Uhash_key,               [ a ] -> mk_keytokeyhash (f a)
          | `Uop Ufail,                   [ a ] -> failg (f a)
          | `Uop Ucontract (_t, _an),     [ _a ] -> assert false
          | `Uop Usetdelegate,            [ _a ] -> assert false
          | `Uop Uimplicitaccount,        [ _a ] -> assert false
          | `Uop Ueq,                     [ _ ] -> assert false
          | `Uop Une,                     [ _ ] -> assert false
          | `Uop Ugt,                     [ _ ] -> assert false
          | `Uop Uge,                     [ _ ] -> assert false
          | `Uop Ult,                     [ _ ] -> assert false
          | `Uop Ule,                     [ _ ] -> assert false
          | `Bop Badd,                 [ a; b ] -> mk_mterm (Mplus (f a, f b)) tint
          | `Bop Bsub,                 [ a; b ] -> mk_mterm (Mminus (f a, f b)) tint
          | `Bop Bmul,                 [ a; b ] -> mk_mterm (Mmult (f a, f b)) tint
          | `Bop Bediv,                [ _a; _b ] -> assert false
          | `Bop Blsl,                 [ _a; _b ] -> assert false
          | `Bop Blsr,                 [ _a; _b ] -> assert false
          | `Bop Bor,                  [ a; b ] -> mk_mterm (Mor  (f a, f b)) tbool
          | `Bop Band,                 [ a; b ] -> mk_mterm (Mgreedyand (f a, f b)) tbool
          | `Bop Bxor,                 [ a; b ] -> mk_mterm (Mgreedyor (f a, f b)) tbool
          | `Bop Bcompare,             [ _; _ ] -> assert false
          | `Bop Bget,                 [ a; b ] -> mk_mterm (Mmapget (mk_map, tunknown, tunknown, f a, f b, None)) tunknown
          | `Bop Bmem,                 [ a; b ] -> mk_mterm (Mmapcontains(mk_map, tunknown, tunknown, f a, f b)) tunknown
          | `Bop Bconcat,              [ a; b ] -> mk_mterm (Mconcat (f a, f b)) tunknown
          | `Bop Bcons,                [ a; b ] -> mk_mterm (Mlistprepend (tunknown, f a, f b)) tunknown
          | `Bop Bpair,                [ a; b ] -> mk_tuple [f a; f b]
          | `Bop Bexec,                [ _a; _b ] -> assert false
          | `Bop Bapply,               [ _a; _b ] -> assert false
          | `Top Tcheck_signature,  [ a; b; c ] -> mk_checksignature (f a) (f b) (f c)
          | `Top Tslice,            [ a; b; c ] -> mk_mterm (Mslice (f a, f b, f c)) tunknown
          | `Top Tupdate,           [ a; b; c ] -> mk_mterm (Mmapput (mk_map, tunknown, tunknown, f a, f b, f c)) tunknown
          | `Top Ttransfer_tokens,  [ _a; _b; _c ] -> assert false
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
      begin
        match i with
        | DIAssign (x, e) ->
          let id = for_dvar x in
          let e = g e in
          mk_mterm (Massign (ValueAssign, tunit, Avar (M.mk_mident (dumloc id)), e)) tunit

        | DIIf (c, (b1, b2)) ->
          mk_mterm (Mif (g c, seq b1, Some (seq b2))) tunit

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

        | DIMatch (c, [("left", _lv, lc) ; ("right", _rv, rc)]) ->
          mk_mterm (Minstrmatchor (g c, M.mk_mident (dumloc "_"), seq rc, M.mk_mident (dumloc "_"), seq lc)) tunit

        | DIFailwith e -> failg (for_expr e)
        | _ -> assert false
      end
    in
    let instrs = List.map for_instr code in
    seq instrs

  let decompile (dprogram, env : dprogram * env) =
    let code = for_code dprogram.code in
    let functions = [Entry (mk_function_struct (M.mk_mident (dumloc "default")) code ~args:[M.mk_mident (dumloc "arg"), for_type dprogram.parameter, None])] in

    let storage_list = get_storage_list dprogram.storage in

    let storage =
      List.mapi (fun n (_id, t) ->
          (* let id = if String.length id > 0 then String.sub id 1 (String.length id - 1) else id in *)
          let id = Format.asprintf "sto_%d" (n + 1)  in
          M.mk_storage_item (M.mk_mident (dumloc id)) MTvar (for_type t) (data_to_mterm ~t:t (get_default_value t))
        ) storage_list
    in
    let model = M.mk_model (dumloc dprogram.name) ~functions:functions ~storage:storage in
    model, env
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
    | Tbuiltin Btimestamp        -> assert false
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
    | Tlambda _                  -> assert false
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

  let rec for_expr (mt : M.mterm) : A.expr =
    let f = for_expr in

    let to_ck an = function
      | M.CKcoll -> assert false
      | M.CKview v -> f v
      | M.CKfield (_oan, _fn, k) -> f k
    in

    match mt.node with
    (* lambda *)

    | Mletin (_ids, _a, _t, _b, _o)       -> assert false
    | Mdeclvar (ids, t, v, c)             -> A.evar (List.map snd ids) ?t:(Option.map for_type t) (f v) VDKbasic c
    | Mdeclvaropt (_ids, _t, _v, _fa, _c) -> assert false
    | Mapp (_e, _args)                    -> assert false


    (* assign *)

    | Massign (op, _, Avar id, v)                   -> A.eassign (for_op op) (A.eterm (snd id)) (f v)
    | Massign (op, _, Avarstore id, v)              -> A.eassign (for_op op) (A.eterm (snd id)) (f v)
    | Massign (_op, _, Aasset (_an, _fn, _k), _v)   -> assert false
    | Massign (_op, _, Arecord (_lv, _rn, _fn), _v) -> assert false
    | Massign (_op, _, Atuple (_lv, _n, _l), _v)    -> assert false
    | Massign (_op, _, Astate, _x)                  -> assert false
    | Massign (_op, _, Aoperations, _v)             -> assert false
    | Massignopt _ -> assert false

    (* control *)

    | Mif (c, t, e)              -> A.eif ?e:(Option.map f e) (f c) (f t)
    | Mmatchwith (_e, _l)        -> assert false
    | Minstrmatchoption _        -> assert false

    | Minstrmatchor (e, xl, bl, xr, br) ->
      A.ematchwith (f e) [
        ([dumloc (A.Pref (dumloc A.PLeft , [snd xl]))], f bl);
        ([dumloc (A.Pref (dumloc A.PRight, [snd xr]))], f br)
      ] MKbasic

    | Minstrmatchlist   _        -> assert false
    | Minstrmatchdetach  _       -> assert false
    | Mfor (_i, _c, _b)          -> assert false
    | Miter (_i, _a, _b, _c, _n) -> assert false
    | Mwhile (_c, _b)            -> assert false
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
          | Invalid e -> f e
          | _ -> assert false
        in
        A.efail v
      end
    | Mfailsome _        -> assert false
    | Mtransfer _tr      -> assert false
    | Memit (_, _)       -> assert false
    | Mdetach (_, _, _, _)  -> assert false
    | Mmicheline _       -> assert false


    (* entrypoint *)

    | Mgetentrypoint (_t, _a, _s)      -> assert false
    | Mcallview (_t, _a, _b, _c)       -> assert false
    | Mimportcallview (_t, _a, _b, _c) -> assert false
    | Mself _id                        -> assert false
    | Mselfcallview (_t, _id, _args)   -> assert false


    (* operation *)

    | Moperations                       -> assert false
    | Mmakeoperation (_v, _d, _a)       -> assert false
    | Mmakeevent (_t, _id, _a)          -> assert false
    | Mcreatecontract (_cc, _d, _a) -> assert false


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
    | Munit              -> A.etuple []
    | MsaplingStateEmpty _v -> assert false
    | MsaplingTransaction (_, v) -> A.ebytes v
    | Mchest v           -> A.ebytes v
    | Mchest_key v       -> A.ebytes v
    | Mtz_expr v         -> A.etz_expr v


    (* control expression *)

    | Mexprif (_c, _t, _e)                   -> assert false
    | Mexprmatchwith (_e, _l)                -> assert false
    | Mmatchoption (_x, _i, _ve, _ne)        -> assert false
    | Mmatchor (_x, _lid, _le, _rid, _re)    -> assert false
    | Mmatchlist (_x, _hid, _tid, _hte, _ee) -> assert false
    | Mternarybool (_c, _a, _b)              -> assert false
    | Mternaryoption (_c, _a, _b)            -> assert false
    | Mfold (_e, _i, _l)                     -> assert false
    | Mmap (_e, _i, _l)                      -> assert false
    | Mexeclambda (_l, _a)                   -> assert false
    | Mapplylambda (_l, _a)                  -> assert false


    (* composite type constructors *)

    | Mleft (t, x)    -> A.eleft (for_type t) (f x)
    | Mright (t, x)   -> A.eright (for_type t) (f x)
    | Mnone           -> A.eoption (ONone None)
    | Msome v         -> A.eoption (OSome (f v))
    | Mtuple l        -> A.etuple (List.map f l)
    | Masset l        -> A.erecord (List.map (fun x -> (None, f x)) l)
    | Massets _l      -> assert false
    | Mlitset _l      -> assert false
    | Mlitlist l      -> A.earray (List.map f l)
    | Mlitmap (_b, _l)-> assert false
    | Mlitrecord _l   -> assert false
    | Mlitevent  _l   -> assert false
    | Mlambda (_rt, _id, _at, _e) -> assert false
    | Mlambda_michelson (_it, _rt, _body) -> assert false
    | Mmicheline_expr (_t, _m, _a) -> assert false

    (* access *)

    | Mdot (_e, _i)              -> assert false
    | Mdotassetfield (an, k, fn) -> A.edot (A.esqapp (A.eterm (snd an)) (f k)) ((SINone, snd fn))
    | Mquestionoption (_a, _fn)  -> assert false


    (* comparison operators *)

    | Mequal (_t, l, r)  -> A.eapp (Foperator (dumloc (A.Cmp Equal))) [f l; f r]
    | Mnequal (_t, l, r) -> A.eapp (Foperator (dumloc (A.Cmp Nequal))) [f l; f r]
    | Mgt (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Gt))) [f l; f r]
    | Mge (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Ge))) [f l; f r]
    | Mlt (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Lt))) [f l; f r]
    | Mle (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Le))) [f l; f r]
    | Mmulticomp (_e, _l)  -> assert false


    (* arithmetic operators *)

    | Mand (_l, _r)    -> assert false
    | Mor (_l, _r)     -> assert false
    | Mgreedyand (_l, _r)-> assert false
    | Mgreedyor (_l, _r) -> assert false
    | Mxor (_l, _r)    -> assert false
    | Mnot _e          -> assert false
    | Mplus (l, r)     -> A.eapp (A.Foperator (dumloc (A.Arith A.Plus))) [f l; f r]
    | Mminus (_l, _r)  -> assert false
    | Mmult (l, r)     -> A.eapp (A.Foperator (dumloc (A.Arith A.Mult))) [f l; f r]
    | Mdivrat (_l, _r) -> assert false
    | Mdiveuc (_l, _r) -> assert false
    | Mmodulo (_l, _r) -> assert false
    | Mdivmod (_l, _r) -> assert false
    | Muminus  e       -> A.eapp (A.Foperator (dumloc (A.Unary A.Uminus))) [f e]
    | MthreeWayCmp (_l, _r) -> assert false
    | Mshiftleft   (_l, _r) -> assert false
    | Mshiftright  (_l, _r) -> assert false
    | Msubnat      (_l, _r) -> assert false
    | Msubmutez    (_l, _r) -> assert false

    (* asset api effect *)

    | Maddasset       (_an, _i)               -> assert false
    | Mputsingleasset (_an, _i)               -> assert false
    | Mputasset       (_an, _k, _v)           -> assert false
    | Maddfield       (_an, _fn, _c, _i)      -> assert false
    | Mremoveasset    (_an, _i)               -> assert false
    | Mremovefield    (_an, _fn, _c, _i)      -> assert false
    | Mremoveall      (_an, _c)               -> assert false
    | Mremoveif       (_an, _c, _la, _lb, _a) -> assert false
    | Mclear          (_an, _v)               -> assert false
    | Mset            (_c,  _l, _k, _v)       -> assert false
    | Mupdate         (_an, _k, _l)           -> assert false
    | Mupdateall      (_an, _c, _l)           -> assert false
    | Maddupdate      (an, ck, k, l)          -> begin
        A.emethod (MKexpr (to_ck an ck)) (dumloc "add_update") [f k; A.erecord (List.map (fun (id, op, x) : A.record_item -> (Some (to_assign_operator op, snd id), f x)) l)]
      end
    | Mputremove      (_an, _c, _k, _v)       -> assert false


    (* asset api expression *)

    | Mget      (_an, _c, _k)           -> assert false
    | Mgetsome  (_an, _c, _k)           -> assert false
    | Mselect   (_an, _c, _la, _lb, _a) -> assert false
    | Msort     (_an, _c, _l)           -> assert false
    | Mcontains (_an, _c, _i)           -> assert false
    | Mnth      (_an, _c, _i)           -> assert false
    | Mcount    (_an, _c)               -> assert false
    | Msum      (_an, _c, _p)           -> assert false
    | Mhead     (_an, _c, _i)           -> assert false
    | Mtail     (_an, _c, _i)           -> assert false


    (* utils *)

    | Mcast (_src, _dst, _v) -> assert false
    | Mtupleaccess (x, k)    -> A.esqapp (f x) (A.ebnat k)
    | Mrecupdate (_x, _l)    -> assert false
    | Mmakeasset (_an, _k, _v) -> assert false
    | Mtocontainer _an         -> assert false
    | Mglobal_constant (_t, _v)-> assert false


    (* set api expression *)

    | Msetadd (_t, _c, _a)                -> assert false
    | Msetremove (_t, _c, _a)             -> assert false
    | Msetupdate (_t, _c, _b, _v)         -> assert false
    | Msetcontains (_t, _c, _a)           -> assert false
    | Msetlength (_t, _c)                 -> assert false
    | Msetfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* set api instruction *)

    | Msetinstradd    _                   -> assert false
    | Msetinstrremove _                   -> assert false


    (* list api expression *)

    | Mlistprepend (_, _c, _a)             -> assert false
    | Mlistlength (_, c)                   -> A.eapp (A.Fident (SINone, (dumloc "size"))) [f c]
    | Mlistcontains (_, _c, _a)            -> assert false
    | Mlistnth (_, _c, _a)                 -> assert false
    | Mlisthead (_, _c, _a)                -> assert false
    | Mlisttail (_, _c, _a)                -> assert false
    | Mlistreverse (_, _l)                 -> assert false
    | Mlistconcat (_, _l, _m)              -> assert false
    | Mlistfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* list api instruction *)

    | Mlistinstrprepend _                  -> assert false
    | Mlistinstrconcat  _                  -> assert false

    (* map api expression *)

    | Mmapput (_, _, _, c, k, v)                  -> A.eapp (A.Fident (SINone, (dumloc "put"))) [f c; f k; f v]
    | Mmapremove (_, _, _, _c, _k)                -> assert false
    | Mmapupdate (_, _, _, _c, _k, _v)            -> assert false
    | Mmapget (_, _, _, _c, _k, _an)              -> assert false
    | Mmapgetopt (_, _, _, _c, _k)                -> assert false
    | Mmapcontains (_, _, _, _c, _k)              -> assert false
    | Mmaplength (_, _, _, _c)                    -> assert false
    | Mmapfold (_, _t, _ik, _iv, _ia, _c, _a, _b) -> assert false


    (* map api instruction *)

    | Mmapinstrput    (_, _, _, _c, _k, _v)       -> assert false
    | Mmapinstrremove (_, _, _, _c, _k)           -> assert false
    | Mmapinstrupdate (_, _, _, _c, _k, _v)       -> assert false


    (* builtin functions *)

    | Mmax (_l, _r)       -> assert false
    | Mmin (_l, _r)       -> assert false
    | Mabs _a             -> assert false
    | Mconcat (_x, _y)    -> assert false
    | Mconcatlist _x      -> assert false
    | Mslice (_x, _s, _e) -> assert false
    | Mlength x           -> A.eapp (A.Fident (SINone, (dumloc "length"))) [f x]
    | Misnone _x          -> assert false
    | Missome _x          -> assert false
    | Minttonat _x        -> assert false
    | Mfloor  _x          -> assert false
    | Mceil   _x          -> assert false
    | Mnattostring _x     -> assert false
    | Mbytestonat _x      -> assert false
    | Mnattobytes _x      -> assert false
    | Mbytestoint _x      -> assert false
    | Minttobytes _x      -> assert false
    | Mpack _x            -> assert false
    | Munpack (_t, _x)    -> assert false
    | Msetdelegate _x     -> assert false
    | Mkeyhashtocontract _x -> assert false
    | Mcontracttoaddress _x -> assert false
    | Maddresstocontract (_t, _x) -> assert false
    | Mkeytoaddress _x      -> assert false
    | Msimplify_rational _x -> assert false
    | Mget_numerator _x     -> assert false
    | Mget_denominator _x   -> assert false
    | Misimplicitaddress _x -> assert false
    | Mexp_horner (_x, _s)  -> assert false


    (* crypto functions *)

    | Mblake2b _x                  -> assert false
    | Msha256  _x                  -> assert false
    | Msha512  _x                  -> assert false
    | Msha3    _x                  -> assert false
    | Mkeccak  _x                  -> assert false
    | Mkeytokeyhash _x             -> assert false
    | Mchecksignature (_k, _s, _x) -> assert false


    (* voting *)

    | Mtotalvotingpower            -> assert false
    | Mvotingpower _x              -> assert false


    (* ticket *)

    | Mcreateticket (_x, _a)    -> assert false
    | Mreadticket _x            -> assert false
    | Msplitticket (_x, _a, _b) -> assert false
    | Mjointickets (_x, _y)     -> assert false


    (* sapling *)

    | Msapling_empty_state   _ -> assert false
    | Msapling_verify_update _ -> assert false


    (* bls curve *)

    | Mpairing_check x -> A.eapp (A.Fident (SINone, (dumloc "pairing_check"))) [f x]


    (* timelock *)

    | Mopen_chest _ -> assert false


    (* constants *)

    | Mnow           -> A.eterm (dumloc A.cst_now)
    | Mtransferred   -> A.eterm (dumloc A.cst_transferred)
    | Mcaller        -> A.eterm (dumloc A.cst_caller)
    | Mbalance       -> A.eterm (dumloc A.cst_balance)
    | Msource        -> A.eterm (dumloc A.cst_source)
    | Mselfaddress   -> A.eterm (dumloc A.cst_self_address)
    | Mselfchainid   -> A.eterm (dumloc A.cst_self_chain_id)
    | Mmetadata      -> A.eterm (dumloc A.cst_metadata)
    | Mlevel         -> A.eterm (dumloc A.cst_level)
    | Mminblocktime  -> A.eterm (dumloc A.cst_min_block_time)


    (* variable *)

    | Mvar (_an, Vassetstate _k) -> assert false
    | Mvar(v, Vstorevar)         -> A.eterm (snd v)
    | Mvar(v, Vstorecol)         -> A.eterm (snd v)
    | Mvar(v, Vlocal)            -> A.eterm (snd v)
    | Mvar(v, Vparam)            -> A.eterm (snd v)
    | Mvar(_v, Vfield)           -> assert false
    | Mvar(_, Vthe)              -> assert false
    | Mvar(_, Vstate)            -> assert false
    | Mvar(v, Vparameter)        -> A.eterm (snd v)
    | Menumval (id, args, _e)            -> begin
        match args with
        | [] -> A.eterm (snd id)
        | _  -> A.eapp (A.Fident (SINone, (snd id))) []
      end

    (* rational *)

    | Mrateq (_l, _r)         -> assert false
    | Mratcmp (_op, _l, _r)   -> assert false
    | Mratarith (_op, _l, _r) -> assert false
    | Mratuminus _v           -> assert false
    | Mrattez (_c, _t)        -> assert false
    | Mnattoint e             -> f e
    | Mnattorat _e            -> assert false
    | Minttorat _e            -> assert false
    | Mratdur (_c, _t)        -> assert false


    (* utils *)

    | Minttodate         _ -> assert false
    | Mmuteztonat        _ -> assert false

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
          | [] -> []
          | xs -> let l = List.map for_expr xs in [A.APOinit l]
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
    | MTvar -> A.mk_variable (A.mk_variable_decl ~dv:dv (snd id) t VKvariable)
    | _ -> assert false
  in

  let for_fun (f : M.function_node) : A.declaration =
    match f with
    | Function (_fs, _t) -> assert false
    | Getter (_fs, _t) -> assert false
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

  A.mk_archetype () ~decls:((A.mk_darchetype model.name)::decls)
