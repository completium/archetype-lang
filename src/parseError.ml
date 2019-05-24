(**

   This module demonstrates that we can easily generate contextual
   error message thanks to the introspection API. Indeed, when a
   parsing error occurs, there exist items in the stack whose analysis
   is not completed. Therefore, the non terminals of their productions
   are probably the syntactically classes that are being recognized
   when the error occurred.

*)

open Parser.MenhirInterpreter
open PureLexer

let rec pop_until pred env =
  match top env with
  | None -> []
  | Some elt ->
    match pred elt with
    | [] -> begin match pop env with
        | None -> assert false
        | Some env -> pop_until pred env
      end
    | l -> l

let keep_predictions predictions (production, focus) =
  if focus < List.length (rhs production) then
    (lhs production) :: predictions
  else
    predictions

let element_contains_prediction_items elt =
  match elt with
  | Element (state, _, _, _) ->
    items state
    |> List.fold_left keep_predictions []

let find_context = function
  | InputNeeded env ->
    pop_until element_contains_prediction_items env
  | _ ->
    assert false (* By the specification of [on_error]. *)

let parse_error pos msg cont =
  Error.error "during parsing" pos msg cont

let contextual_error_msg lexer checkpoint continuation =
  find_context checkpoint |> fun nonterminals ->
  Error.error "parsing" (Lexer.current_position lexer)
    (Printf.sprintf "Error while analyzing %s."
       (String.concat " or " (List.map Symbol.string_of_symbol nonterminals))
    )
  @@ continuation
