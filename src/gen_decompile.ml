(* open Tezos_micheline *)
open Location

module T = Michelson
module M = Model
module A = ParseTree

let parse_michelson (_filename, _channel) : T.michelson =
  (* let source = "{ storage int; parameter unit; code { UNPAIR; DROP; PUSH int 2; SWAP; DROP; NIL operation; PAIR }; }" in *)
  (* let (tokens, _lexing_errors) = Micheline_parser.tokenize source in *)
  (* let (_asts, _parsing_errors) = Micheline_parser.parse_toplevel tokens in *)
  (* let _ : Micheline_printer.node list = [] in *)
  (* List.iter (fun (x : Micheline_parser.node) -> Format.printf "%a@." Micheline_printer.print_expr x) asts; *)
  (* Format.printf "%a@." Micheline_printer.print asts *)

  let storage = T.tint in
  let parameter = T.tunit in
  let code = T.SEQ T.[UNPAIR; DROP 1; PUSH (tint, Dint (Big_int.big_int_of_int 2)); SWAP; DROP 1; NIL toperation; PAIR] in
  T.mk_michelson storage parameter code


let to_ir (_m : T.michelson) : T.ir =
  let storage_type = T.tint in
  let storage_data = T.Dint (Big_int.zero_big_int) in
  let storage_list = [] in
  let parameter = T.tunit in
  let funs = [] in
  let entries = [] in
  T.mk_ir storage_type storage_data storage_list parameter funs entries

let to_model (_ir : T.ir) : M.model =
  let storage =
    let si = M.mk_storage_item (dumloc "n") MTvar M.tnat (M.mk_nat 0) in
    [si]
  in
  let f : M.function__ =
    let name = dumloc "default" in
    let args = [] in
    let body = M.mk_mterm (M.Massign (ValueAssign, M.tnat, Avarstore (dumloc "n"), M.mk_nat 2)) M.tunit in
    let fn : M.function_struct = M.mk_function_struct name body ~args:args in
    let node : M.function_node = M.Entry fn in
    M.mk_function node
  in
  let functions = [f] in
  M.mk_model (dumloc "hello") ~functions:functions ~storage:storage

let to_archetype (model : M.model) : A.archetype =
  let name = model.name in
  let exts = None in
  let d0 = A.Darchetype (name, exts) in
  let z : A.expr = dumloc (A.Eliteral (Lnat (Big_int.zero_big_int))) in
  let d1 = A.Dvariable (dumloc "n", dumloc (A.Tref(dumloc "nat")), Some z, VKvariable, [], exts) in
  let two : A.expr = dumloc (A.Eliteral (Lnat (Big_int.big_int_of_int 2))) in
  let a : A.expr = dumloc (A.Eterm ((None, None), dumloc "n")) in
  let body : A.expr = dumloc (A.Eassign (ValueAssign, a, two)) in
  let d2 = A.Dentry (dumloc "default", [], A.mk_entry_properties (), Some (body, None), exts) in
  let decls = [d0; d1; d2] |> List.map dumloc in
  A.mk_archetype () ~decls:decls
