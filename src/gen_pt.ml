open Location
(* open Tools *)

open ParseTree

let remove_spec_decl (pt : archetype) : archetype =
  (* let for_archetype () *)
  match unloc pt with
  (* | Marchetype a -> begin
          List.fold_left () pt.de
     end *)
  | _ -> pt
