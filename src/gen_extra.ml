open Location

module PT = ParseTree
module T  = Michelson

let rec to_model_expr (e : PT.expr) : T.data =
  let f = to_model_expr in

  let to_one (l : PT.expr list) =
    match List.rev l with
    | [] -> T.Dunit
    | i::q -> List.fold_left (fun accu x -> T.Dpair (f x, accu)) (f i) q
  in

  match unloc e with
  (* | Eterm                _ -> assert false *)
  | Eliteral (Lint      n) -> Dint n
  | Eliteral (Lnat      n) -> Dint n
  | Eliteral (Ldecimal  s) -> let n, d = Core.decimal_string_to_rational s in Dpair (Dint n, Dint d)
  | Eliteral (Ltz       n) -> Dint (Big_int.mult_int_big_int 1000000 n)
  | Eliteral (Lmtz      n) -> Dint (Big_int.mult_int_big_int    1000 n)
  | Eliteral (Lutz      n) -> Dint n
  | Eliteral (Laddress  s) -> Dstring s
  | Eliteral (Lstring   s) -> Dstring s
  | Eliteral (Lbool  true) -> Dtrue
  | Eliteral (Lbool false) -> Dfalse
  | Eliteral (Lduration s) -> Dint (s |> Core.string_to_duration |> Core.duration_to_timestamp)
  | Eliteral (Ldate     s) -> Dint (s |> Core.string_to_date |> Core.date_to_timestamp)
  | Eliteral (Lbytes    s) -> Dbytes s
  | Eliteral (Lpercent  n) -> let n, d = Core.compute_irr_fract (n, Big_int.big_int_of_int 100) in Dpair (Dint n, Dint d)
  | Earray         l       -> Dlist (List.map f l)
  (* | Erecord        l       -> assert false *)
  | Etuple         l       -> to_one l
  | Eoption (OSome e)      -> Dsome (f e)
  | Eoption (ONone _)      -> Dnone
  | Eor     (Oleft _)      -> Dleft (f e)
  | Eor     (Oright _)     -> Dright (f e)
  (* | Elambda        _       -> assert false *)
  | Enothing               -> Dunit
  | _ -> assert false
