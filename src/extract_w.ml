module M = Model

module MapString = Map.Make(String)

type prover = {
  id: int;
  name: string;
  version: string;
  timelimit: int;
  steplimit: int;
  memlimit: int;
}
[@@deriving yojson, show {with_path = false}]

type result = {
  status: string;
  time: string;
  steps: int;
}
[@@deriving yojson, show {with_path = false}]

type proof = {
  prover_id: string;
  result: result;
}
[@@deriving yojson, show {with_path = false}]

type goal = {
  name: string;
  expl: string;
  proved: bool;
  proofs: proof list;
  transf: transf option;
}
[@@deriving yojson, show {with_path = false}]

and transf = {
  name: string;
  goals: goal list;
}

type theory = {
  name: string;
  proved: bool;
  goals: goal list;
}
[@@deriving yojson, show {with_path = false}]

type file = {
  format: string;
  theory: theory list;
}
[@@deriving yojson, show {with_path = false}]

type why3session = {
  shape_version : string;
  provers: prover list;
  file: file;
}
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)

let mk_prover id name version timelimit steplimit memlimit =
  { id; name; version; timelimit; steplimit; memlimit }

let mk_result status time steps : result =
  { status; time; steps }

let mk_proof prover_id result : proof =
  { prover_id; result }

let mk_goal ?(proofs = []) ?transf name expl proved : goal =
  { name; expl; proved; proofs; transf }

let mk_transf name goals : transf =
  { name; goals }

let mk_theory ?(goals = []) name proved : theory =
  { name; proved; goals }

let mk_file ?(theory = []) format : file =
  { format; theory }

let mk_why3session ?(provers = []) shape_version file : why3session =
  { shape_version; provers; file }

(* -------------------------------------------------------------------- *)

let doit (i : Xmlm.signal) =
  match i with
  | `El_start ((a, b), attrs) ->
    Format.eprintf "(%s, %s): %a@\n"
      a b (Printer_tools.pp_list ", " (fun fmt (k, v) -> Format.fprintf fmt "(%s, %s) -> %s" (fst k) (snd k) v )) attrs
  | `El_end -> Format.eprintf "El_end@\n"
  | `Data s -> Format.eprintf "data: %s@\n" s
  | `Dtd _  -> Format.eprintf "dtd"

let is_tag id = function
  | `El_start ((_, a), _) -> String.equal id a
  | _ -> false

let is_end = function
  | `El_end -> true
  | _ -> false

let skip tag c i =
  while not (is_tag tag !c) do
    c := Xmlm.input i
  done

let extract_attributes (attrs : Xmlm.attribute list) : string MapString.t =
  let map = MapString.empty in
  List.fold_left (fun accu (a : Xmlm.attribute) ->
      let k = fst a |> snd in
      let v = snd a in
      MapString.add k v accu)
    map attrs

(* -------------------------------------------------------------------- *)

let extract_cb f dft key map =
  MapString.find_opt key map |> (function | Some v -> f v | None -> dft)

let extract_int key map =
  extract_cb int_of_string 0 key map

let extract_bool key map =
  extract_cb bool_of_string false key map

let extract_string key map =
  extract_cb (fun x -> x) "" key map

let extract_prover (c : Xmlm.signal)=
  match c with
  | `El_start ((_, a), attrs) when String.equal a "prover" -> begin
      let map = extract_attributes attrs in
      let id        = MapString.find "id"        map |> int_of_string in
      let name      = MapString.find "name"      map in
      let version   = MapString.find "version"   map in
      let timelimit = MapString.find "timelimit" map |> int_of_string in
      let steplimit = MapString.find "steplimit" map |> int_of_string in
      let memlimit  = MapString.find "memlimit"  map |> int_of_string in

      mk_prover id name version timelimit steplimit memlimit
    end
  | _ -> assert false

let extract_provers c i =
  let provers = ref [] in
  while is_tag "prover" !c do
    let p = extract_prover !c in
    provers := p::!provers;
    c := Xmlm.input i;
    c := Xmlm.input i
  done;
  List.rev !provers

let extract_result c i : result =
  match !c with
  | `El_start ((_, a), attrs) when String.equal a "result" -> begin
      let map = extract_attributes attrs in
      let status = extract_string "status" map in
      let time   = extract_string "time"   map in
      let steps  = extract_int    "steps"  map in
      c := Xmlm.input i;
      mk_result status time steps
    end
  | _ -> assert false

let extract_proof (c : Xmlm.signal ref) i =
  match !c with
  | `El_start ((_, a), attrs) when String.equal a "proof" -> begin
      let map = extract_attributes attrs in
      let prover = extract_string "prover" map in

      c := Xmlm.input i;
      let results = extract_result c i in

      c := Xmlm.input i;
      (* doit !c; *)
      c := Xmlm.input i;
      mk_proof prover results
    end
  | _ -> assert false

let extract_proofs c i =
  let proofs = ref [] in
  while is_tag "proof" !c do
    let t = extract_proof c i in
    proofs := t::!proofs
  done;
  List.rev !proofs

let rec extract_goal c i : goal =
  match !c with
  | `El_start ((_, a), attrs) when String.equal a "goal" -> begin
      let map = extract_attributes attrs in
      let name   = extract_string "name" map in
      let expl   = extract_string "expl" map in
      let proved = extract_bool "proved" map in

      c := Xmlm.input i;
      let proofs = extract_proofs c i in

      (* c := Xmlm.input i; *)
      (* doit !c; *)
      let transf =
        if (is_tag "transf" !c)
        then Some (extract_trans c i)
        else None
      in

      c := Xmlm.input i;
      mk_goal ~proofs ?transf name expl proved
    end
  | _ -> assert false
and extract_trans c i : transf =
  match !c with
  | `El_start ((_, a), attrs) when String.equal a "transf" -> begin
      let map = extract_attributes attrs in
      let name   = extract_string "name" map in

      c := Xmlm.input i;
      let goals = extract_goals c i in

      c := Xmlm.input i;
      mk_transf name goals
    end
  | _ -> assert false

and extract_goals c i : goal list =
  let goals = ref [] in
  while is_tag "goal" !c do
    let t = extract_goal c i in
    goals := t::!goals;
  done;
  List.rev !goals

let extract_theory c i : theory =
  match !c with
  | `El_start ((_, a), attrs) when String.equal a "theory" -> begin
      let map    = extract_attributes attrs in
      let name   = extract_string "name" map in
      let proved = extract_bool "proved" map in

      c := Xmlm.input i;
      let goals : goal list = extract_goals c i in

      c := Xmlm.input i;
      (* doit !c; *)
      mk_theory ~goals name proved
    end
  | _ -> assert false

let extract_theories c i : theory list =
  let theories = ref [] in
  while is_tag "theory" !c do
    let t = extract_theory c i in
    theories := t::!theories
  done;
  List.rev !theories

let to_whysession (ic : in_channel) : why3session =
  let i = Xmlm.make_input ~strip:true (`Channel ic) in
  let _ = Xmlm.input i in
  let w3s =
    match Xmlm.input i with
    | `El_start ((_, _), _attr) -> begin
        (* Format.printf "(%s, %s)@\n" a b; *)
        let c = ref (Xmlm.input i) in

        let provers = extract_provers c i in
        while not (is_tag "theory" !c) do
          c := Xmlm.input i
        done;

        let theory : theory list = extract_theories c i in

        mk_why3session ~provers "" (mk_file ~theory "")
      end
    | _ -> assert false
  in
  w3s

let test path =
  let id ic oc =
    let i = Xmlm.make_input (`Channel ic) in
    let o = Xmlm.make_output (`Channel oc) in
    let rec pull i o depth =
      Xmlm.output o (Xmlm.peek i);
      match Xmlm.input i with
      | `El_start _ -> pull i o (depth + 1)
      | `El_end -> if depth = 1 then () else pull i o (depth - 1)
      | `Data _ -> pull i o depth
      | `Dtd _ -> assert false
    in
    Xmlm.output o (Xmlm.input i); (* `Dtd *)
    pull i o 0;
    if not (Xmlm.eoi i) then invalid_arg "document not well-formed"
  in
  let ch = open_in path in
  let oc = Stdlib.stdout in
  id ch oc

let process (_m : M.model) (path_xml : string) =
  let w3s: why3session =
    path_xml
    |> open_in
    |> to_whysession
  in

  Format.printf "%s@\n" (Yojson.Safe.to_string (why3session_to_yojson w3s))
