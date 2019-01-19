(* opening the Why3 library *)
open Why3
open Format
open Miles

(* a ground propositional goal: true or false *)
let fmla_true : Term.term = Term.t_true
let fmla_false : Term.term = Term.t_false
let fmla1 : Term.term = Term.t_or fmla_true fmla_false


let main () =
printf "@[formula 1 is:@ %a@]@." Pretty.print_term fmla1

let _ = main (); let _ = mk_miles_model () in ()
