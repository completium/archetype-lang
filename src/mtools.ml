open Core
open Location

module P = ParseTree
module M = Model

let expr_to_mterm (e : P.expr) : M.mterm =
  match unloc e with
  | Eliteral (Lint      n) -> M.mk_bint n
  | Eliteral (Lnat      n) -> M.mk_bnat n
  | Eliteral (Ldecimal  s) -> begin
      let n, d = Core.decimal_string_to_rational s in
      M.mk_brat n d
    end
  | Eliteral (Ltz       n) -> M.mk_btez (string_to_big_int_tz Ktz  n)
  | Eliteral (Lmtz      n) -> M.mk_btez (string_to_big_int_tz Kmtz n)
  | Eliteral (Lutz      n) -> M.mk_btez (string_to_big_int_tz Kutz n)
  | Eliteral (Laddress  s) -> M.mk_address s
  | Eliteral (Lstring   s) -> M.mk_string s
  | Eliteral (Lbool     v) -> M.mk_bool v
  | Eliteral (Lduration s) -> M.mk_duration (s |> Core.string_to_duration)
  | Eliteral (Ldate     s) -> M.mk_date (s |> Core.string_to_date)
  | Eliteral (Lbytes    s) -> M.mk_bytes s
  | Eliteral (Lpercent  n) -> begin
      let n, d = string_to_big_int_percent n in
      M.mk_brat n d
    end
  | Enothing               -> M.unit
  (* | Earray         l       -> begin
      let ll =
        match typ with
        | Some ({node = (T.Tset t | T.Tlist t); annotation = _}) -> List.map (f ~typ:t) l
        | Some ({node = (T.Tmap (tk, tv) | T.Tbig_map (tk, tv)); annotation = _} as tm) -> begin
            List.map ( fun (x : PT.expr) ->
                match unloc x with
                | Etuple [a; b] -> T.Delt (f ~typ:tk a, f ~typ:tv b)
                | _ -> error_cc tm) l
          end
        | Some t -> error_cc t
        | None -> List.map f l
      in
      Dlist ll
     end *)
  (* | Etuple l -> to_one ?typ l *)
  (* | Eoption o -> begin
      let g =
        match typ with
        | Some ({node = T.Toption t; annotation = _}) -> f ~typ:t
        | Some t -> error_cc t
        | None -> f ?typ:None
      in
      match o with
      | OSome x -> Dsome (g x)
      | ONone _ -> Dnone
     end *)
  (* | Eor o -> begin
      let g =
        match typ with
        | Some ({node = T.Tor (tl, tr); annotation = _}) -> (match o with | Oleft _ -> f ~typ:tl | Oright _ -> f ~typ:tr)
        | Some t -> error_cc t
        | None -> f ?typ:None
      in
      match o with
      | Oleft  (_, _, x) -> Dleft  (g x)
      | Oright (_, _, x) -> Dright (g x)
     end *)
  (* | Elambda        _       -> assert false *)
  | _ -> assert false

let string_to_mterm (input : string) : M.mterm =
  let path = "." in
  FIString (path, input)
  |> Io.parse_expr
  |> expr_to_mterm
