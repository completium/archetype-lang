let flag = ref `Raise

let exit_on_error () = flag := `Exit

let raise_on_error () = flag := `Raise

let resume_on_error () = flag := `Resume

let errors = ref []
let warnings : (Position.t list * string) list ref = ref []

exception Error of Position.t list * string

exception ParseError of (Position.t list * string) list

exception Stop of int

let print_error positions msg =
  Printf.sprintf "%s%s"
    (String.concat "\n"
       (List.map (fun p -> Position.string_of_pos p ^": ") positions))
    msg

let error_alert positions msg continue =
  if not !Options.quiet
  then output_string stderr (print_error positions msg);
  match !flag with
  | `Exit -> exit 1
  | `Raise -> raise (Error (positions, msg))
  | `Resume -> errors := (positions, msg)::!errors; continue ()

let add_warning positions msg continue =
  warnings := (positions, msg)::!warnings; continue ()

let global_error kind msg =
  error_alert [] (Printf.sprintf "Global Error (%s)\n  %s"  kind msg)

let errorN kind poss msg =
  error_alert poss (Printf.sprintf "Error (%s)\n  %s" kind msg)

let error kind pos = errorN kind [pos]
let error2 kind pos1 pos2 = errorN kind [pos1; pos2]
