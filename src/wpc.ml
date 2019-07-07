open Tools
open Mlwtree

(* Weakest Precondition Calculus -----------------------------------------------*)

(* f is the formula,
   code is the code *)
let rec gen_wpc (code : term) (f : term) : term =
  match code with
  | Tseq []    -> f
  | Tseq [e]   -> gen_wpc f e
  | Tseq l     -> (* length l >= 2 *)
    begin
      match List.rev l with
      | e::tl -> gen_wpc (Tseq (List.rev tl)) (gen_wpc e f)
      | _ -> assert false
    end
  | Tassign (e1, e2)                -> replace e1 e2 f
  | Tif (test,Traise _,Some else_t) -> Timpl(Tnot test, gen_wpc else_t f)
  | _ -> f
