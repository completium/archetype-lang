(* -------------------------------------------------------------------- *)
module Enum   = BatEnum
module Option = BatOption
module Set    = BatSet
module Map    = BatMap

(* -------------------------------------------------------------------- *)
module Format : sig
  include module type of BatFormat

  val pp_option :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a option -> unit
end = struct
  include BatFormat

  let pp_option pp fmt x =
    match x with
    | None   -> ()
    | Some x -> Format.fprintf fmt "%a" pp x
end

(* -------------------------------------------------------------------- *)
module List : sig
  include module type of BatList

  val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
  val pop    : 'a list -> 'a * 'a list
end = struct
  include BatList

  let pop (xs : 'a list) =
    match xs with
    | [] -> invalid_arg "List.pop"
    | x :: xs -> (x, xs)

  let split3 =
    let rec doit (a1, a2, a3) = function
      | [] ->
        (List.rev a1, List.rev a2, List.rev a3)
      | (x1, x2, x3) :: s ->
        doit (x1 :: a1, x2 :: a2, x3 :: a3)  s
    in fun s -> doit ([], [], []) s
end

(* -------------------------------------------------------------------- *)
module BigInt : sig
  include module type of BatBig_int

  val compare0 : t -> int
  val pp : Format.formatter -> t -> unit
end = struct
  include BatBig_int

  let compare0 (x : t) =
    compare zero x

  let pp fmt i =
    Format.fprintf fmt "%s" (string_of_big_int i)
end

(* -------------------------------------------------------------------- *)
let gen () = Oo.id (object end)

module Trace : sig
  val set_trace : bool -> unit
  val get_trace : unit -> bool
  val print_trace : (Format.formatter -> 'a -> unit) -> 'a -> unit
end = struct
  let trace : bool ref = ref true
  let set_trace v = trace := v
  let get_trace _ = !trace
  let print_trace pp x =
    if !trace
    then Format.printf "%a" pp x
end


(* -------------------------------------------------------------------- *)
type symbol = string
[@@deriving show {with_path = false}]

type pcode = pinstr list

and pinstr =
  { pi_name : symbol; pi_args : parg list }

and parg =
  | PA_Value of pvalue
  | PA_Type  of ptype
  | PA_Code  of pcode

and pvalue =
  | PV_Int of BigInt.t
  | PV_Str of string

and ptype = {
  pt_name : symbol;
  pt_args : ptype list;
}

(* -------------------------------------------------------------------- *)
type code = instr list [@@deriving show]

and instr =
  (* Stack manipulation *)
  | DROP of int
  | DUP
  | SWAP
  | DIG  of int
  | DUG  of int
  | PUSH of value

  (* Comparisons *)
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE

  (* Operations on integers *)
  | NEG
  | ABS
  | ADD
  | SUB
  | MUL
  | EDIV

  (* Operations on booleans *)
  | AND
  | OR
  | NOT

  (* Generic comparison operator *)
  | COMPARE

  (* Operations on unit value(s) *)
  | UNIT

  (* Operations on optional values *)
  | NONE    of type_
  | SOME    of type_
  | IF_NONE of code * code

  (* Operations on pairs *)
  | PAIR
  | UNPAIR
  | CAR
  | CDR

  (* Operations on sums *)
  | LEFT    of type_
  | RIGHT   of type_
  | IF_LEFT of code * code

  (* Operations on lists *)
  | NIL     of type_
  | CONS
  | IF_CONS of code * code
  | SIZE
  | MAP     of code
  | ITER    of code

  (* Control structures *)
  | FAILWITH of stack
  | IF  of code * code
  | DIP of int * code

[@@deriving show]

and value =
  | V_Int of BigInt.t
  | V_Str of string

and type_ =
  | T_Unit
  | T_Bool
  | T_Int
  | T_String
  | T_Option of type_
  | T_List   of type_
  | T_Pair   of type_ * type_
  | T_Sum    of type_ * type_

and arg =
  | A_Value of value
  | A_Type  of type_
  | A_Code  of code

and stack = type_ list

(* -------------------------------------------------------------------- *)
let rec pp_type (fmt : Format.formatter) (ty : type_) =
  match ty with
  | T_Unit    -> Format.fprintf fmt "%s" "unit"
  | T_Bool    -> Format.fprintf fmt "%s" "bool"
  | T_Int     -> Format.fprintf fmt "%s" "int"
  | T_String  -> Format.fprintf fmt "%s" "string"

  | T_Option ty         -> Format.fprintf fmt "(%a) option" pp_type ty
  | T_List   ty         -> Format.fprintf fmt "(%a) list" pp_type ty
  | T_Pair   (ty1, ty2) -> Format.fprintf fmt "(%a)*(%a)" pp_type ty1 pp_type ty2
  | T_Sum    (ty1, ty2) -> Format.fprintf fmt "(%a)+(%a)" pp_type ty1 pp_type ty2

(* -------------------------------------------------------------------- *)
let pp_stack (fmt : Format.formatter) (st : stack) =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
       pp_type) st

(* -------------------------------------------------------------------- *)
exception CompilationError

(* -------------------------------------------------------------------- *)
let ty_eq (ty1 : type_) (ty2 : type_) =
  ty1 = ty2

(* -------------------------------------------------------------------- *)
let stack_eq (s1 : stack) (s2 : stack) =
  let module E = struct exception NEQ end in

  try
    let s =
      try  List.combine s1 s2
      with Failure _ | Invalid_argument _ -> raise E.NEQ in
    List.for_all (fun (t1, t2) -> ty_eq t1 t2) s

  with E.NEQ -> false

(* -------------------------------------------------------------------- *)
let rec compile_type (pty : ptype) : type_ =
  match pty.pt_name, pty.pt_args with
  | "unit"  , []       -> T_Unit
  | "bool"  , []       -> T_Bool
  | "int"   , []       -> T_Int
  | "string", []       -> T_String
  | "option", [a]      -> T_Option (compile_type a)
  | "list"  , [a]      -> T_List (compile_type a)
  | "pair"  , [a1; a2] -> T_Pair (compile_type a1, compile_type a2)
  | "sum"   , [a1; a2] -> T_Sum (compile_type a1, compile_type a2)

  | _, _ -> raise CompilationError

(* -------------------------------------------------------------------- *)
let rec compile (s : stack option) (c : pcode) : stack option * code =
  List.fold_left_map compile1 s c

(* -------------------------------------------------------------------- *)
and compile1 (s : stack option) (i : pinstr) : stack option * instr =
  match i.pi_name, i.pi_args, s with
  | "DROP", [PA_Value (PV_Int i)], Some s ->
    let i =
      try  BigInt.int_of_big_int i
      with Failure _ -> raise CompilationError in

    if List.length s < i then
      raise CompilationError;
    (Some (List.drop i s), DROP i)

  | "DUP", [], Some (ty :: s) ->
    (Some (ty :: ty :: s), DUP)

  | "SWAP", [], Some (ty1 :: ty2 :: s) ->
    (Some (ty2 :: ty1 :: s), SWAP)

  | "DIG", [PA_Value (PV_Int i)], Some s ->
    let i =
      try  BigInt.int_of_big_int i
      with Failure _ -> raise CompilationError in

    if List.length s < i + 1 then
      raise CompilationError;

    let hd, tl = List.split_at i s in
    let x, tl = List.hd tl, List.tl tl in

    (Some (x :: (hd @ tl)), DIG i)

  | "DUG", [PA_Value (PV_Int i)], Some (ty :: s) ->
    let i =
      try  BigInt.int_of_big_int i
      with Failure _ -> raise CompilationError in

    if List.length s < i then
      raise CompilationError;
    let s1, s2 = List.split_at i s in
    (Some (s1 @ ty :: s2), DUG i)

  | "PUSH", [PA_Value (PV_Int i)], Some s ->
    (Some (T_Int :: s), PUSH (V_Int i))

  | "PUSH", [PA_Value (PV_Str x)], Some s ->
    (Some (T_String :: s), PUSH (V_Str x))

  | "EQ" , [], Some (T_Int :: s) -> (Some (T_Bool :: s), EQ )
  | "NEQ", [], Some (T_Int :: s) -> (Some (T_Bool :: s), NEQ)
  | "LT" , [], Some (T_Int :: s) -> (Some (T_Bool :: s), LT )
  | "GT" , [], Some (T_Int :: s) -> (Some (T_Bool :: s), GT )
  | "LE" , [], Some (T_Int :: s) -> (Some (T_Bool :: s), LE )
  | "GE" , [], Some (T_Int :: s) -> (Some (T_Bool :: s), GE )

  | "NEG", [], Some (T_Int :: s) -> (Some (T_Int :: s), NEG)
  | "ABS", [], Some (T_Int :: s) -> (Some (T_Int :: s), ABS)

  | "ADD", [], Some (T_Int :: T_Int :: s) -> (Some (T_Int :: s), ADD)
  | "SUB", [], Some (T_Int :: T_Int :: s) -> (Some (T_Int :: s), SUB)
  | "MUL", [], Some (T_Int :: T_Int :: s) -> (Some (T_Int :: s), MUL)

  | "EDIV", [], Some (T_Int :: T_Int :: s) ->
    (Some (T_Option (T_Pair (T_Int, T_Int)) :: s), EDIV)

  | "NOT", [], Some (T_Bool :: s) -> (Some (T_Bool :: s), NOT)

  | "OR" , [], Some (T_Bool :: T_Bool :: s) -> (Some (T_Bool :: s), OR)
  | "AND", [], Some (T_Bool :: T_Bool :: s) -> (Some (T_Bool :: s), AND)

  | "UNIT", [], Some s ->
    (Some (T_Unit :: s), UNIT)

  | "NONE", [PA_Type ty], Some s ->
    let ty = compile_type ty in
    (Some (T_Option ty :: s), NONE ty)

  | "SOME", [], Some (ty :: s) ->
    (Some (T_Option ty :: s), SOME ty)

  | "IF_NONE", [PA_Code c1; PA_Code c2], Some (T_Option ty :: s) ->
    let s1, c1 = compile (Some s) c1 in
    let s2, c2 = compile (Some (ty :: s)) c2 in
    let s      = unify_stack s1 s2 in
    (s, IF_NONE (c1, c2))

  | "PAIR", [], Some (ty1 :: ty2 :: s) ->
    (Some (T_Pair (ty1, ty2) :: s), PAIR)

  | "UNPAIR", [], Some (T_Pair (ty1, ty2) :: s) ->
    (Some (ty1 :: ty2 :: s), UNPAIR)

  | "CAR", [], Some (T_Pair (ty1, _) :: s) ->
    (Some (ty1 :: s), CAR)

  | "CDR", [], Some (T_Pair (_, ty2) :: s) ->
    (Some (ty2 :: s), CDR)

  | "LEFT", [PA_Type tyr], Some (tyl :: s) ->
    let tyr = compile_type tyr in
    (Some (T_Sum (tyl, tyr) :: s), LEFT tyr)

  | "RIGHT", [PA_Type tyl], Some (tyr :: s) ->
    let tyl = compile_type tyl in
    (Some (T_Sum (tyl, tyr) :: s), RIGHT tyl)

  | "IF_LEFT", [PA_Code c1; PA_Code c2], Some (T_Sum (tyl, tyr) :: s) ->
    let s1, c1 = compile (Some (tyl :: s)) c1 in
    let s2, c2 = compile (Some (tyr :: s)) c2 in
    let s      = unify_stack s1 s2 in
    (s, IF_LEFT (c1, c2))

  | "NIL", [PA_Type ty], Some s ->
    let ty = compile_type ty in (Some (T_List ty :: s), NIL ty)

  | "CONS", [], Some (ty1 :: (T_List ty2 :: _) as s) when ty_eq ty1 ty2 ->
    (Some s, CONS)

  | "IF_CONS", [PA_Code c1; PA_Code c2], Some (T_List ty :: s) ->
    let s1, c1 = compile (Some (ty :: T_List ty :: s)) c1 in
    let s2, c2 = compile (Some s) c2 in
    let s      = unify_stack s1 s2 in
    (s, IF_CONS (c1, c2))

  | "SIZE", [], Some (T_List _ :: s) ->
    (Some (T_Int :: s), SIZE)

  | "MAP", [PA_Code c], Some (T_List ty :: s) -> begin
      match compile (Some (ty :: s)) c with
      | Some (rty :: s'), c when stack_eq s s' ->
        (Some (T_List rty :: s), MAP c)
      | _ -> raise CompilationError
    end

  | "ITER", [PA_Code c], Some (T_List ty :: s) ->
    let s', c = compile (Some (ty :: s)) c in
    if not (Option.eq ~eq:stack_eq (Some s) s') then
      raise CompilationError;
    (s', ITER c)

  | "IF", [PA_Code c1; PA_Code c2], Some (T_Bool :: s) ->
    let s1, c1 = compile (Some s) c1 in
    let s2, c2 = compile (Some s) c2 in
    let s      = unify_stack s1 s2 in
    (s, IF (c1, c2))

  | "DIP", [PA_Value (PV_Int i); PA_Code c], Some s ->
    let i =
      try  BigInt.int_of_big_int i
      with Failure _ -> raise CompilationError in

    if i < 0 || List.length s < i then
      raise CompilationError;
    let hd, tl = List.split_at i s in
    let tl, c = compile (Some tl) c in
    (Option.map (fun tl -> hd @ tl) tl, DIP (i, c))

  | "COMPARE", [], Some (ty1 :: ty2 :: s) ->
    if not (ty_eq ty1 ty2) then
      raise CompilationError;
    (Some (T_Int :: s), COMPARE)

  | "FAILWITH", [], Some ((_ :: _) as s) ->
    (None, FAILWITH s)

  | _, _, _ ->
    raise CompilationError

(* -------------------------------------------------------------------- *)
and unify_stack (r1 : stack option) (r2 : stack option) =
  match r1, r2 with
  | None, None ->
    None

  | Some r1, Some r2 ->
    if not (stack_eq r1 r2) then
      raise CompilationError;
    Some r1

  | None, Some s | Some s, None ->
    Some s

(* -------------------------------------------------------------------- *)
module SoftCode : sig
  val ct_UNIT : ptype
  val ct_BOOL : ptype
  val ct_INT  : ptype
  val ct_SUM  : ptype -> ptype -> ptype
  val ct_PAIR : ptype -> ptype -> ptype

  val c_DROP     : int -> pinstr
  val c_DUP      : pinstr
  val c_SWAP     : pinstr
  val c_DIG      : int -> pinstr
  val c_DUG      : int -> pinstr
  val c_PUSH     : [`Int of int | `Str of string ] -> pinstr
  val c_EQ       : pinstr
  val c_NEQ      : pinstr
  val c_LT       : pinstr
  val c_GT       : pinstr
  val c_LE       : pinstr
  val c_GE       : pinstr
  val c_NEG      : pinstr
  val c_ABS      : pinstr
  val c_ADD      : pinstr
  val c_SUB      : pinstr
  val c_MUL      : pinstr
  val c_EDIV     : pinstr
  val c_AND      : pinstr
  val c_OR       : pinstr
  val c_NOT      : pinstr
  val c_COMPARE  : pinstr
  val c_UNIT     : pinstr
  val c_NONE     : ptype -> pinstr
  val c_SOME     : pinstr
  val c_IF_NONE  : pcode -> pcode -> pinstr
  val c_PAIR     : pinstr
  val c_UNPAIR   : pinstr
  val c_CAR      : pinstr
  val c_CDR      : pinstr
  val c_LEFT     : ptype -> pinstr
  val c_RIGHT    : ptype -> pinstr
  val c_IF_LEFT  : pcode -> pcode -> pinstr
  val c_NIL      : ptype -> pinstr
  val c_CONS     : pinstr
  val c_IF_CONS  : pcode -> pcode -> pinstr
  val c_SIZE     : pinstr
  val c_MAP      : pcode -> pinstr
  val c_ITER     : pcode -> pinstr
  val c_IF       : pcode -> pcode -> pinstr
  val c_DIP      : int -> pcode -> pinstr
  val c_FAILWITH : pinstr

  val c_desugared_UNPAIR : pcode
end = struct
  let ct_UNIT = { pt_name = "unit"; pt_args = []; }
  let ct_BOOL = { pt_name = "bool"; pt_args = []; }
  let ct_INT  = { pt_name = "int" ; pt_args = []; }

  let ct_SUM ty1 ty2 =
    { pt_name = "sum"; pt_args = [ty1; ty2]; }

  let ct_PAIR ty1 ty2 =
    { pt_name = "pair"; pt_args = [ty1; ty2]; }

  let mkpinstr ?(args = []) (name : symbol) =
    { pi_name = name; pi_args = args; }

  let pa_int (i : int) =
    PA_Value (PV_Int (BigInt.of_int i))

  let pa_str (s : string) =
    PA_Value (PV_Str s)

  let pa_value = function
    | `Int i -> pa_int i
    | `Str s -> pa_str s

  let c_DROP     = fun i     -> mkpinstr "DROP"    ~args:[pa_int i]
  let c_DUP      =              mkpinstr "DUP"
  let c_SWAP     =              mkpinstr "SWAP"
  let c_DIG      = fun i     -> mkpinstr "DIG"     ~args:[pa_int i]
  let c_DUG      = fun i     -> mkpinstr "DUG"     ~args:[pa_int i]
  let c_PUSH     = fun v     -> mkpinstr "PUSH"    ~args:[pa_value v]
  let c_EQ       =              mkpinstr "EQ"
  let c_NEQ      =              mkpinstr "NEQ"
  let c_LT       =              mkpinstr "LT"
  let c_GT       =              mkpinstr "GT"
  let c_LE       =              mkpinstr "LE"
  let c_GE       =              mkpinstr "GE"
  let c_NEG      =              mkpinstr "NEG"
  let c_ABS      =              mkpinstr "ABS"
  let c_ADD      =              mkpinstr "ADD"
  let c_SUB      =              mkpinstr "SUB"
  let c_MUL      =              mkpinstr "MUL"
  let c_EDIV     =              mkpinstr "EDIV"
  let c_AND      =              mkpinstr "AND"
  let c_OR       =              mkpinstr "OR"
  let c_NOT      =              mkpinstr "NOT"
  let c_COMPARE  =              mkpinstr "COMPARE"
  let c_UNIT     =              mkpinstr "UNIT"
  let c_NONE     = fun t     -> mkpinstr "NONE"    ~args:[PA_Type t]
  let c_SOME     =              mkpinstr "SOME"
  let c_IF_NONE  = fun c1 c2 -> mkpinstr "IF_NONE" ~args:[PA_Code c1; PA_Code c2]
  let c_PAIR     =              mkpinstr "PAIR"
  let c_UNPAIR   =              mkpinstr "UNPAIR"
  let c_CAR      =              mkpinstr "CAR"
  let c_CDR      =              mkpinstr "CDR"
  let c_LEFT     = fun t     -> mkpinstr "LEFT"    ~args:[PA_Type t]
  let c_RIGHT    = fun t     -> mkpinstr "RIGHT"   ~args:[PA_Type t]
  let c_IF_LEFT  = fun c1 c2 -> mkpinstr "IF_LEFT" ~args:[PA_Code c1; PA_Code c2]
  let c_NIL      = fun t     -> mkpinstr "NIL"     ~args:[PA_Type t]
  let c_CONS     =              mkpinstr "CONS"
  let c_IF_CONS  = fun c1 c2 -> mkpinstr "IF_CONS" ~args:[PA_Code c1; PA_Code c2]
  let c_SIZE     =              mkpinstr "SIZE"
  let c_MAP      = fun c     -> mkpinstr "MAP"     ~args:[PA_Code c]
  let c_ITER     = fun c     -> mkpinstr "ITER"    ~args:[PA_Code c]
  let c_IF       = fun c1 c2 -> mkpinstr "IF"      ~args:[PA_Code c1; PA_Code c2]
  let c_DIP      = fun i  c  -> mkpinstr "DIP"     ~args:[pa_int i; PA_Code c]
  let c_FAILWITH =              mkpinstr "FAILWITH"

  let c_desugared_UNPAIR =
    [c_DUP; c_CAR; c_DIP 1 [c_CDR]]
end

(* -------------------------------------------------------------------- *)
type var   = [`VLocal of local | `VDup of vdup | `VGlobal of symbol]
and  local = expr option ref
and  vdup  = [`Direct of int * symbol option | `Redirect of vdup] ref

and expr  =
  | Fun of symbol * expr list
  | Var of var
  | Int of BigInt.t
  | Str of string
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type rstack1 = [var | `Paired of rstack1 * rstack1]
[@@deriving show {with_path = false}]

type rstack  = rstack1 list
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type dinstr =
  | DIAssign   of var * [`Expr of expr | `Dup of vdup]
  | DIIf       of expr * (dcmd * dcmd)
  | DIMatch    of expr * (symbol * dpattern list * dcmd) list
  | DIFailwith of expr

and dpattern =
  | DVar  of local
  | DPair of dpattern * dpattern

and dcmd = dinstr list

(* -------------------------------------------------------------------- *)
let pp_simple_instr fmt i =
  let str =
    match i with
    (* Stack manipulation *)
  | DROP _ -> "DROP"
  | DUP    -> "DUP"
  | SWAP   -> "SWAP"
  | DIG  _ -> "DIG"
  | DUG  _ -> "DUG"
  | PUSH _ -> "PUSH"

  (* Comparisons *)
  | EQ  -> "EQ"
  | NEQ -> "NEQ"
  | LT  -> "LT"
  | GT  -> "GT"
  | LE  -> "LE"
  | GE  -> "GE"

  (* Operations on integers *)
  | NEG -> "NEG"
  | ABS -> "ABS"
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | EDIV -> "EDIV"

  (* Operations on booleans *)
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"

  (* Generic comparison operator *)
  | COMPARE -> "COMPARE"

  (* Operations on unit value(s) *)
  | UNIT -> "UNIT"

  (* Operations on optional values *)
  | NONE    _ -> "NONE"
  | SOME    _ -> "SOME"
  | IF_NONE _ -> "IF_NONE"

  (* Operations on pairs *)
  | PAIR -> "PAIR"
  | UNPAIR -> "UNPAIR"
  | CAR -> "CAR"
  | CDR -> "CDR"

  (* Operations on sums *)
  | LEFT    _ -> "LEFT"
  | RIGHT   _ -> "RIGHT"
  | IF_LEFT _ -> "IF_LEFT"

  (* Operations on lists *)
  | NIL _ -> "NIL"
  | CONS -> "CONS"
  | IF_CONS _ -> "IF_CONS"
  | SIZE -> "SIZE"
  | MAP _ -> "MAP"
  | ITER _ -> "ITER"

  (* Control structures *)
  | FAILWITH _ -> "FAILWITH"
  | IF       _ -> "IF"
  | DIP      _ -> "DIP"
  in
  Format.fprintf fmt "%s" str

let rec pp_dcmd (fmt : Format.formatter) (c : dcmd) =
  match c with
  | [] ->
    Format.fprintf fmt "%s" "pass"

  | _ ->
    Format.fprintf fmt "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_dinstr)
      c

and pp_dinstr (fmt : Format.formatter) (i : dinstr) =
  match i with
  | DIAssign (x, `Expr e) ->
    Format.fprintf fmt "%a <- %a" pp_var x pp_expr e

  | DIAssign (x, `Dup v) ->
    Format.fprintf fmt "%a <- %a" pp_var x pp_var (`VDup v)

  | DIIf (c, (b1, b2)) ->
    Format.fprintf fmt "@[<v 2>if (%a):@\n%a@]@\n@[<v 2>else:@\n%a@]"
      pp_expr c pp_dcmd b1 pp_dcmd b2

  | DIMatch (c, bs) ->
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
        pp_dcmd body
    in

    Format.fprintf fmt "match (%a) with@\n%a@\nend" pp_expr c
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         pp_branch) bs

  | DIFailwith e ->
    Format.fprintf fmt "failwith (%a)" pp_expr e

and pp_expr (fmt : Format.formatter) (e : expr) =
  match e with
  | Int i ->
    Format.fprintf fmt "%s" (BigInt.string_of_big_int i)

  | Str s ->
    Format.fprintf fmt "\"%s\"" s

  | Fun (f, args) ->
    Format.fprintf fmt "%s(%a)" f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         pp_expr)
      args

  | Var v ->
    Format.fprintf fmt "%a" pp_var v

and pp_var (fmt : Format.formatter) (v : var) =
  match v with
  | `VGlobal n ->
    Format.fprintf fmt "%s" n

  | `VLocal x ->
    Format.fprintf fmt "#%d" (Obj.magic x : int)

  | `VDup { contents = `Direct (i, None) } ->
    Format.fprintf fmt "$%d" i

  | `VDup { contents = `Direct (_, Some n) } ->
    Format.fprintf fmt "%s" n

  | `VDup { contents = `Redirect v } ->
    Format.fprintf fmt "%a" pp_var (`VDup v)

(* -------------------------------------------------------------------- *)
let var_unset (v : local) =
  Option.is_none !v

(* -------------------------------------------------------------------- *)
let write_var (e : expr) (v : var) =
  match v with
  | `VGlobal n ->
    [DIAssign (`VGlobal n, `Expr e)]
  | `VLocal x ->
    x := Some e; []
  | `VDup { contents = `Redirect _ } ->
    assert false
  | `VDup ({ contents = `Direct (i, _) } as x) ->
    x := `Direct (i, None); [DIAssign (v, `Expr e)]

(* -------------------------------------------------------------------- *)
let rec expr_of_rstack1 (r : rstack1) =
  match r with
  | #var as v ->
    Var v
  | `Paired (r1, r2) ->
    Fun ("pair", [expr_of_rstack1 r1; expr_of_rstack1 r2])

(* -------------------------------------------------------------------- *)
let pdestruct (path : bool list) (e : expr) =
  List.fold_right (fun b e ->
      let dtor = if b then "cdr" else "car" in
      Fun (dtor, [e])) path e

(* -------------------------------------------------------------------- *)
let rec unify (v1 : rstack1) (v2 : rstack1) =
  match v1, v2 with
  | `VGlobal n, `VGlobal m when n = m ->
    ([], []), `VGlobal n

  | `VGlobal _, `VGlobal _ ->
    assert false

  | `VLocal x, `VLocal y when x ==(*phy*) y ->
    ([], []), `VLocal x

  | `VLocal x, `VLocal y ->
    assert (var_unset x && var_unset y);
    y := Some (Var (`VLocal x));
    ([], []), `VLocal x

  | `VDup { contents = `Redirect _ }, _
  | _, `VDup { contents = `Redirect _ } -> assert false

  | `VDup ({ contents = `Direct x } as vx),
    `VDup ({ contents = `Direct y } as vy) -> begin
      let tg =
        match snd x, snd y with
        | Some r1, Some r2 when r1 = r2 -> Some r1
        | _, _ -> None in

      let r = ref (`Direct (gen (), tg)) in
      vx := `Redirect r; vy := `Redirect r;
      ([], []), `VDup r
    end

  | `VLocal x, (`VDup _ as y) ->
    assert (var_unset x);
    x := Some (Var y);
    ([], []), y

  | `Paired (x1, y1), `Paired (x2, y2) ->
    let (w1, w'1), z1 = unify x1 x2 in
    let (w2, w'2), z2 = unify y1 y2 in
    (w1 @ w2, w'1 @ w'2), `Paired (z1, z2)

  | `VLocal x, `Paired _ ->
    assert (var_unset x);
    let y1 = `VLocal (ref None) in
    let y2 = `VLocal (ref None) in
    x := Some (Fun ("pair", [Var y1; Var y2]));
    unify (`Paired (y1, y2)) v2

  | `VGlobal n, `VDup { contents = `Direct (_, Some m)} when n = m ->
    ([], []), `VGlobal n

  | `VGlobal n, (`VDup (({ contents = `Direct (vi, _) } as vy) as y)) ->
    vy := `Direct (vi, None);
    ([n, y], []), `VDup y

  | `VGlobal n, `VLocal y ->
    assert (var_unset y);
    let x = ref (`Direct (gen (), Some n)) in
    y := Some (Var (`VDup x)); ([n, x], []), `VDup x

  | `VGlobal _, `Paired _
  | `VDup    _, `Paired _ ->
    assert false

  | `VDup    _, `VGlobal _
  | `VDup    _, `VLocal  _
  | `VLocal  _, `VGlobal _
  | `Paired  _, `VLocal  _
  | `Paired  _, `VGlobal _
  | `Paired  _, `VDup    _ ->
    let (w1, w2), s = unify v2 v1 in (w2, w1), s

(* -------------------------------------------------------------------- *)
let unify_rstack (s1 : rstack) (s2 : rstack) =
  assert (List.length s1 = List.length s2);
  let pr , s   = List.split (List.map2 unify s1 s2) in
  let pr1, pr2 = List.split pr in
  (List.flatten pr1, List.flatten pr2), s

(* -------------------------------------------------------------------- *)
let merge_rstack ((b1, s1) : bool * rstack) ((b2, s2) : bool * rstack) =
  match b1, b2 with
  |  true,  true -> unify_rstack s1 s2
  |  true, false -> ([], []), s1
  | false,  true -> ([], []), s2
  | false, false -> assert false

(* -------------------------------------------------------------------- *)
let rec dptn_of_rstack1 (r : rstack1) =
  match r with
  | `Paired (r1, r2) ->
    let p1, c1 = dptn_of_rstack1 r1 in
    let p2, c2 = dptn_of_rstack1 r2 in
    (DPair (p1, p2), c1 @ c2)

  | `VLocal x ->
    assert (var_unset x); (DVar x, [])

  | (`VGlobal _) as n ->
    let x : local = ref None in
    (DVar x, [DIAssign (n, `Expr (Var (`VLocal x)))])

  | (`VDup _) as y ->           (* FIXME *)
    let x : local = ref None in
    (DVar x, [DIAssign (y, `Expr (Var (`VLocal x)))])

(* -------------------------------------------------------------------- *)
let rec decompile_i (s : rstack) (i : instr) : rstack * dinstr list =
  let pp_list sep pp =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
      pp
  in

  if (Trace.get_trace ())
  then Format.eprintf "%a\t[%a]@\n" pp_simple_instr i (pp_list "; " pp_rstack1) s;


  match i with
  | DUP ->
    let x, s = List.pop s in
    let y, s = List.pop s in
    let _, z = unify x y in z :: s, [] (* FIXME? *)

  | DIG i ->
    assert (List.length s >= i + 1);
    let x, s1 = List.hd s, List.tl s in
    let s1, s2 = List.split_at i s1 in
    s1 @ (x :: s2), []

  | DUG i ->
    assert (List.length s >= i + 1);
    let s1, s2 = List.split_at i s in
    let x, s2 = List.hd s2, List.tl s2 in
    x :: (s1 @ s2), []

  | DIP (i, c) ->
    assert (List.length s >= i);
    let s1, s2 = List.split_at i s in
    let s2, ops = decompile s2 c in
    s1 @ s2, ops

  | DROP i ->
    (List.init i (fun _ -> `VLocal (ref None)) @ s), []

  | PUSH (V_Int i) -> begin
      let x, s = List.pop s in

      match x with
      | #var as v ->
        s, (write_var (Int i) v)
      | _ ->
        assert false
    end

  | PUSH (V_Str c) -> begin
      let x, s = List.pop s in

      match x with
      | #var as v ->
        s, (write_var (Str c) v)
      | _ ->
        assert false
    end

  | PAIR -> begin
      let x, s = List.pop s in

      match x with
      | `Paired (x1, x2) ->
        x1 :: x2 :: s, []

      | #var as v ->
        let x1 = `VLocal (ref None) in
        let x2 = `VLocal (ref None) in
        let op = DIAssign (v, `Expr (Fun ("pair", [Var x1; Var x2]))) in
        x1 :: x2 :: s, [op]
    end

  | UNPAIR ->
    let x, s = List.pop s in
    let y, s = List.pop s in
    `Paired (x, y) :: s, []

  | CAR ->
    let x, s = List.pop s in
    (`Paired (x, `VLocal (ref None)) :: s), []

  | CDR ->
    let y, s = List.pop s in
    (`Paired (`VLocal (ref None), y) :: s), []

  | SWAP -> begin
      let x, s = List.pop s in
      let y, s = List.pop s in
      (y :: x :: s, [])
    end

  | IF (c1, c2) -> begin
      let s1, b1 = decompile s c1 in
      let s2, b2 = decompile s c2 in
      let (pr1, pr2), s = unify_rstack s1 s2 in
      let pr1 = List.map (fun (x, e) -> DIAssign (`VGlobal x, `Dup e)) pr1 in
      let pr2 = List.map (fun (x, e) -> DIAssign (`VGlobal x, `Dup e)) pr2 in
      let x = `VLocal (ref None) in
      x :: s, [DIIf (Var x, (pr1 @ b1, pr2 @ b2))]
    end

  | IF_LEFT (c1, c2) ->
    compile_match s [("left", 1), c1; ("right", 1), c2]

  | IF_NONE (c1, c2) ->
    compile_match s [("none", 0), c1; ("some", 1), c2]

  | IF_CONS (c1, c2) ->
    compile_match s [("cons", 1), c1; ("nil", 0), c2]

  | UNIT    -> decompile_op s ("unit"   , 0)
  | NONE _  -> decompile_op s ("none"   , 0)
  | SOME _  -> decompile_op s ("some"   , 1)
  | NIL _   -> decompile_op s ("nil"    , 0)
  | CONS    -> decompile_op s ("cons"   , 2)
  | SIZE    -> decompile_op s ("size"   , 1)
  | LEFT _  -> decompile_op s ("left"   , 1)
  | RIGHT _ -> decompile_op s ("right"  , 1)

  | NOT     -> decompile_op s ("not"    , 1)
  | AND     -> decompile_op s ("and"    , 2)
  | OR      -> decompile_op s ("or"     , 2)
  | ABS     -> decompile_op s ("abs"    , 1)
  | NEG     -> decompile_op s ("neg"    , 1)
  | ADD     -> decompile_op s ("add"    , 2)
  | SUB     -> decompile_op s ("sub"    , 2)
  | MUL     -> decompile_op s ("mul"    , 2)
  | EDIV    -> decompile_op s ("ediv"   , 2)
  | EQ      -> decompile_op s ("eq"     , 1)
  | NEQ     -> decompile_op s ("neq"    , 1)
  | GT      -> decompile_op s ("gt"     , 1)
  | GE      -> decompile_op s ("ge"     , 1)
  | LE      -> decompile_op s ("le"     , 1)
  | LT      -> decompile_op s ("lt"     , 1)
  | COMPARE -> decompile_op s ("compare", 2)

  | FAILWITH s ->
    let s     = List.map (fun _ -> `VLocal (ref None)) s in
    let x, _  = List.pop s in
    (s, [DIFailwith (expr_of_rstack1 x)])

  | MAP  _ -> assert false
  | ITER _ -> assert false

and decompile_op (s : rstack) ((name, n) : symbol * int) =
  let x, s = List.pop s in
  match x with
  | #var as x ->
    let args = List.init n (fun _ -> `VLocal (ref None)) in
    args @ s, write_var (Fun (name, List.map (fun v -> Var v) args)) x
  | _ -> assert false

and compile_match (s : rstack) (bs : ((symbol * int) * code) list) =
  let sc, subs = List.split (List.map (fun ((name, n), b) ->
      let sc, bc = decompile s b in
      assert (List.length sc >= n);
      let p, sc = List.split_at n sc in
      let p, dp = List.split (List.map dptn_of_rstack1 p) in
      (sc, (name, p, List.flatten dp @ bc))) bs) in

  let x  = `VLocal (ref None) in (* FIXME *)
  let sc = List.fold_left (fun x y -> snd (unify_rstack x y)) (List.hd sc) (List.tl sc) in

  x :: sc, [DIMatch (Var x, subs)]

and decompile (s : rstack) (c : code) : rstack * dinstr list =
  let s, c = List.fold_left_map decompile_i s (List.rev c) in
  s, List.flatten (List.rev c)

(* -------------------------------------------------------------------- *)
let rec compress_c (c : dcmd) =
  List.flatten (List.map compress_i c)

and compress_i (i : dinstr) =
  match i with
  | DIAssign (x, e) -> begin
      match compress_assign_e e with
      | None   -> []
      | Some e -> [DIAssign (x, e)]
    end

  | DIIf (c, (b1, b2)) ->
    [DIIf (compress_e c, (compress_c b1, compress_c b2))]

  | DIMatch (c, bs) ->
    [DIMatch (compress_e c, List.map (fun (n, x, b) -> (n, x, compress_c b)) bs)]

  | DIFailwith e ->
    [DIFailwith (compress_e e)]

and compress_e (e : expr) =
  match e with
  | Int _ | Str _ ->
    e

  | Var (`VGlobal _ | `VLocal { contents = None }) ->
    e

  | Fun (f, args) ->
    fun_simpl f (List.map compress_e args)

  | Var (`VLocal { contents = Some e }) ->
    compress_e e

  | Var (`VDup _) ->
    e

and compress_assign_e = function
  | `Expr e -> Some (`Expr (compress_e e))
  | `Dup  d -> Option.map (fun x -> `Dup x) (compress_vdup d)

and compress_vdup (v : vdup) : vdup option =
  match !v with
  | `Redirect v ->
    compress_vdup v

  | `Direct (_, None) ->
    Some v

  | `Direct (_, Some _) ->
    None

and fun_simpl f args =
  match f, args with
  | "car", [Fun ("pair", [e; _])]
  | "cdr", [Fun ("pair", [_; e])] ->
    e

  | "pair", [Fun ("car", [e1]); Fun ("cdr", [e2])] when e1 = e2 ->
    e1

  | _, _ -> Fun (f, args)

(* -------------------------------------------------------------------- *)
module type Example = sig
  val arguments : ptype
  val storage   : ptype
  val code      : pcode
end

(* -------------------------------------------------------------------- *)
module Simple : Example = struct
  let arguments = SoftCode.ct_BOOL
  let storage   = SoftCode.ct_INT

  let code =
    let open SoftCode in [
      c_UNPAIR;
      c_DROP 1;
      c_PUSH (`Int 2);
      c_SWAP;
      c_DROP 1;
      c_NIL ct_UNIT;
      c_PAIR;
    ]
end

(* -------------------------------------------------------------------- *)
module Simple2 : Example = struct
  let arguments = SoftCode.ct_UNIT
  let storage   = SoftCode.ct_INT

  let code =
    let open SoftCode in [
      c_UNPAIR;
      c_DROP 1;
      c_PUSH (`Int 3);
      c_SWAP;
      c_DROP 1;
      c_DUP;
      c_DIG 2;
      c_DROP 1;
      c_PAIR;
      c_NIL ct_UNIT;
      c_PAIR
    ]
end

(* -------------------------------------------------------------------- *)
module DecompIf : Example = struct
  let arguments = SoftCode.ct_BOOL
  let storage   = SoftCode.ct_INT

  let code =
    let open SoftCode in
    c_desugared_UNPAIR @ [
      c_DUP;
      c_IF
        [
          c_PUSH (`Int 2);
          c_DIP 1 [
            c_DIG 1; c_DROP 1;
          ];
          c_DUG 1;
        ] (* else *) [
        c_PUSH (`Int 3);
        c_DIP 1 [
          c_DIG 1; c_DROP 1;
        ];
        c_DUG 1;
      ];
      c_DROP 1;
      c_NIL ct_UNIT;
      c_PAIR;
    ]
end

(* -------------------------------------------------------------------- *)
module DeferredWrite : Example = struct
  let arguments = SoftCode.ct_BOOL
  let storage   = SoftCode.ct_INT

  let code =
    let open SoftCode in [
      c_UNPAIR;
      c_DUP;
      c_IF [
        c_PUSH (`Int 2);
      ] (* else *) [
        c_PUSH (`Int 3);
      ];
      c_SWAP;
      c_IF [
        c_SWAP;
        c_DROP 1;
      ] (* else *) [
        c_DROP 1;
      ];
      c_NIL ct_UNIT;
      c_PAIR;
    ]
end

(*
[(ops, n)]
PAIR: [ops; n]
NIL : ops <- nil; [n]

IF-THEN:
  [n; a1]
IF-ELSE:
  [a2; n]

MERGE + IF:
  [c1;
   conflit pour n via d1 ==> n <- d1;
   conflit pour n via d2 ==> n <- d2]

SWAP: [d1; c1; d2]

IF-THEN:
  d1 est un conflit reel:
    d1 <- 2 + marque d1 en conflit
  [c1; d2]

IF-ELSE:
  d1 est un conflit reel:
    d1 <- 3 + marque d1 en conflit
  [c1; d2]

MERGE: [c1; d2]

IF: [c2; c1; d2]

DUP: c1 = c2 => c1 & c2 sont locales
  - substitution de c1 par c2
  - [c2; d2]

UNPAIR: [c2, d2]

PARAMS:
  c2 est locale -> c2 est substituee par le premier parametre
  d2 est un conflit non resolu pour n
    or, on ecrit n dans d2 -> donc d2 est resolu : c'est n
*)

(* -------------------------------------------------------------------- *)
module AVV : Example = struct
  let arguments =
    let open SoftCode in
    ct_SUM ct_INT (ct_SUM ct_UNIT ct_INT)

  let storage =
    let open SoftCode in
    ct_PAIR ct_INT ct_INT

  let code : pcode =
    let open SoftCode in [
      c_DUP;
      c_CDR;
      c_SWAP;
      c_CAR;
      c_IF_LEFT [
        c_DUP;
        c_PUSH (`Int 5);
        c_COMPARE;
        c_LT;
        c_IF [
        ] (* else *) [
          c_PUSH (`Str "WrongCondition: params.divisor > 5");
          c_FAILWITH;
        ];
        c_SWAP;
        c_DUP;
        c_CAR;
        c_SWAP;
        c_CDR;
        c_DIG 2;
        c_SWAP;
        c_EDIV;
        c_IF_NONE [
          c_PUSH (`Int 26);
          c_FAILWITH;
        ] (* else-some *) [
          c_CAR;
        ];
        c_SWAP;
        c_PAIR;
      ] (* else-right *) [
        c_IF_LEFT [
          c_DROP 1;
          c_DUP;
          c_CAR;
          c_SWAP;
          c_CDR;
          c_PUSH (`Int 2);
          c_MUL;
          c_SWAP;
          c_PAIR;
        ] (* else-right *) [
          c_SWAP;
          c_DUP;
          c_CDR;
          c_SWAP;
          c_CAR;
          c_PUSH (`Int 1);
          c_ADD;
          c_PAIR;
          c_CAR;
          c_PAIR;
        ]
      ];
      c_NIL ct_UNIT;
      c_PAIR;
    ]
end

(* -------------------------------------------------------------------- *)
let main () =
  (* let module T = Trace in *)
  (* T.set_trace true; *)

  let module E = Simple2 in

  let pty = compile_type E.arguments in
  let aty = compile_type E.storage in

  let st, code = compile (Some [T_Pair (pty, aty)]) E.code in

  if not (Option.eq ~eq:stack_eq st (Some [T_Pair (T_List T_Unit, aty)])) then
    raise CompilationError;

  let args prefix =
    let mkvar i : rstack1 =
      `VGlobal (Printf.sprintf "%s%d" prefix i) in

    let rec create i : _ -> rstack1 = function
      | T_Pair (_, ty) ->
        `Paired (mkvar i, create (i+1) ty)
      | _ ->
        mkvar i
    in create 1 in

  let pst = args "args_" pty in
  let ast = args "sto_"  aty in

  let ost, dc = decompile [`Paired (`VGlobal "ops", ast)] code in

  let dc =
    match ost with
    | [`Paired (px, ax)] ->
      let (pr1 , pr2 ), _ = unify pst px in
      let (pr'1, pr'2), _ = unify ast ax in
      let pr = List.map (fun pr ->
          List.map (fun (x, e) -> DIAssign (`VGlobal x, `Dup e)) pr
        ) [pr1; pr'1; pr2; pr'2] in
      List.flatten pr @ dc
    | _ -> assert false in

  Format.eprintf "%a@." pp_dcmd (compress_c dc)

(* -------------------------------------------------------------------- *)
let () = main ()
