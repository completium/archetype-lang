(* -------------------------------------------------------------------- *)
open Ident
open Tools
open Location

module L  = Location
module PT = ParseTree
module M  = Ast

(* -------------------------------------------------------------------- *)
module Type : sig
  val as_container        : M.ptyp -> (M.ptyp * M.container) option
  val as_asset            : M.ptyp -> M.lident option
  val as_asset_collection : M.ptyp -> (M.lident * M.container) option
  val as_tuple            : M.ptyp -> (M.ptyp list) option

  val is_numeric   : M.ptyp -> bool
  val is_currency  : M.ptyp -> bool
  val is_primitive : M.ptyp -> bool
  val is_option    : M.ptyp -> bool

  val equal     : M.ptyp -> M.ptyp -> bool
  val sig_equal : M.ptyp list -> M.ptyp list -> bool

  val compatible     : from_:M.ptyp -> to_:M.ptyp -> bool
  val sig_compatible : from_:M.ptyp list -> to_:M.ptyp list -> bool
end = struct
  let as_container = function M.Tcontainer (ty, c) -> Some (ty, c) | _ -> None
  let as_asset     = function M.Tasset     x       -> Some x       | _ -> None
  let as_tuple     = function M.Ttuple     ts      -> Some ts      | _ -> None

  let as_asset_collection = function
    | M.Tcontainer (M.Tasset asset, c) -> Some (asset, c)
    | _ -> None

  let is_numeric = function
    | M.Tbuiltin (M.VTint | M.VTrational) -> true |  _ -> false

  let is_currency = function
    | M.Tbuiltin (M.VTcurrency) -> true | _ -> false

  let is_primitive = function
    | M.Tbuiltin _ -> true | _ -> false

  let is_option = function
    | M.Toption _ -> true | _ -> false

  let equal = ((=) : M.ptyp -> M.ptyp -> bool)

  let compatible ~(from_ : M.ptyp) ~(to_ : M.ptyp) =
    match from_, to_ with
    | _, _ when from_ = to_ ->
      true

    | M.Tbuiltin bfrom, M.Tbuiltin bto -> begin
        match bfrom, bto with
        | M.VTaddress, M.VTrole
        | M.VTrole   , M.VTaddress
        | M.VTint    , M.VTrational -> true

        | _, _ -> false
      end

    | M.Tcontract _, M.Tbuiltin (M.VTaddress | M.VTrole) (* FIXME *)
    | M.Tbuiltin (M.VTaddress | M.VTrole), M.Tcontract _ ->
      true

    | _, _ ->
      false

  let sig_compatible ~(from_ : M.ptyp list) ~(to_ : M.ptyp list) =
    List.length from_ = List.length to_
    && List.for_all2 (fun from_ to_ -> compatible ~from_ ~to_) from_ to_

  let sig_equal tys1 tys2 =
    List.length tys1 = List.length tys2
    && List.for_all2 equal tys1 tys2
end

(* -------------------------------------------------------------------- *)
type opsig = {
  osl_sig : M.ptyp list;
  osl_ret : M.ptyp;
} [@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type error_desc =
  | AlienPattern
  | AssetExpected                      of M.ptyp
  | AssetWithoutFields
  | BeforeOrLabelInExpr
  | BeforeIrrelevant                   of [`Local | `State]
  | BeforeWithLabel
  | BindingInExpr
  | CannotInferAnonRecord
  | CannotInferCollectionType
  | CollectionExpected
  | DivergentExpr
  | DuplicatedContractEntryName        of ident
  | DuplicatedCtorName                 of ident
  | DuplicatedFieldInAssetDecl         of ident
  | DuplicatedFieldInRecordLiteral     of ident
  | DuplicatedInitMarkForCtor
  | DuplicatedPKey
  | DuplicatedVarDecl                  of ident
  | AnonymousFieldInEffect
  | EmptyEnumDecl
  | ExpressionExpected
  | ForeignState                       of ident option * ident option
  | FormulaExpected
  | IncompatibleTypes                  of M.ptyp * M.ptyp
  | InvalidActionDescription
  | InvalidActionExpression
  | InvalidArcheTypeDecl
  | InvalidAssetCollectionExpr         of M.ptyp
  | InvalidAssetExpression
  | InvalidCallByExpression
  | InvalidExpressionForEffect
  | InvalidExpression
  | InvalidFieldsCountInRecordLiteral
  | InvalidLValue
  | InvalidFormula
  | InvalidInstruction
  | InvalidNumberOfArguments           of int * int
  | InvalidRoleExpression
  | InvalidSecurityAction
  | InvalidSecurityRole
  | InvalidSortingExpression
  | InvalidStateExpression
  | InvalidTypeForVarWithFromTo
  | LetInElseInInstruction
  | LetInElseOnNonOption
  | MissingFieldInRecordLiteral        of ident
  | MixedAnonInRecordLiteral
  | MixedFieldNamesInRecordLiteral     of ident list
  | MoreThanOneInitState               of ident list
  | MultipleAssetStateDeclaration
  | MultipleInitialMarker
  | MultipleMatchingOperator           of PT.operator * M.ptyp list * opsig list
  | MultipleFromToInVarDecl
  | MultipleStateDeclaration
  | NameIsAlreadyBound                 of ident * Location.t option
  | NoMatchingOperator                 of PT.operator * M.ptyp list
  | NoSuchMethod                       of ident
  | NoSuchSecurityPredicate            of ident
  | NonCodeLabel                       of ident
  | NonLoopLabel                       of ident
  | NotAKeyOfType
  | NotAnAssetType
  | NotAnEnumType
  | NotAPrimitiveType
  | NotARole                           of ident
  | NumericExpressionExpected
  | OpInRecordLiteral
  | OrphanedLabel                      of ident
  | PartialMatch                       of ident list
  | ReadOnlyGlobal                     of ident
  | SecurityInExpr
  | SpecOperatorInExpr
  | TransferWithoutDest
  | UninitializedVar
  | UnknownAction                      of ident
  | UnknownAsset                       of ident
  | UnknowContractEntryPoint           of ident * ident
  | UnknownEnum                        of ident
  | UnknownField                       of ident * ident
  | UnknownFieldName                   of ident
  | UnknownLabel                       of ident
  | UnknownLocalOrVariable             of ident
  | UnknownProcedure                   of ident
  | UnknownState                       of ident
  | UnknownTypeName                    of ident
  | UnpureInFormula
  | UselessPattern
  | VoidMethodInExpr
[@@deriving show {with_path = false}]

type error = L.t * error_desc

(* -------------------------------------------------------------------- *)
let pp_operator fmt (op : PT.operator) : unit =
  let pp = Printer_tools.pp_str fmt in
  match op with
  | Logical And   -> pp "and"
  | Logical Or    -> pp "or"
  | Logical Imply -> pp "->"
  | Logical Equiv -> pp "<->"
  | Cmp Equal     -> pp "="
  | Cmp Nequal    -> pp "<>"
  | Cmp Gt        -> pp ">"
  | Cmp Ge        -> pp ">="
  | Cmp Lt        -> pp "<"
  | Cmp Le        -> pp "<="
  | Arith Plus    -> pp "+"
  | Arith Minus   -> pp "-"
  | Arith Mult    -> pp "*"
  | Arith Div     -> pp "/"
  | Arith Modulo  -> pp "%"
  | Arith DivRat  -> pp "div"
  | Unary Uplus   -> pp "unary +"
  | Unary Uminus  -> pp "unary -"
  | Unary Not     -> pp "not"


(* -------------------------------------------------------------------- *)
let pp_error_desc fmt e =
  let pp s = Format.fprintf fmt s in

  match e with
  | AlienPattern                       -> pp "This pattern does not belong to the enumeration"
  | AssetExpected ty                   -> pp "Asset expected (found a %a)" Printer_ast.pp_ptyp ty
  | AssetWithoutFields                 -> pp "Asset without fields"
  | BeforeIrrelevant `Local            -> pp "The `before' modifier cannot be used on local variables"
  | BeforeIrrelevant `State            -> pp "The `before' modifier cannot be used on state constructors"
  | BeforeOrLabelInExpr                -> pp "The `before' or label modifiers can only be used in formulas"
  | BeforeWithLabel                    -> pp "Cannot use `before' labels at the same time"
  | BindingInExpr                      -> pp "Binding in expression"
  | CannotInferAnonRecord              -> pp "Cannot infer a non record"
  | CannotInferCollectionType          -> pp "Cannot infer collection type"
  | CollectionExpected                 -> pp "Collection expected"
  | DivergentExpr                      -> pp "Divergent expression"
  | DuplicatedContractEntryName i      -> pp "Duplicated contract entry name: %a" pp_ident i
  | DuplicatedCtorName i               -> pp "Duplicated constructor name: %a" pp_ident i
  | DuplicatedFieldInAssetDecl i       -> pp "Duplicated field in asset declaration: %a" pp_ident i
  | DuplicatedFieldInRecordLiteral i   -> pp "Duplicated field in record literal: %a" pp_ident i
  | DuplicatedInitMarkForCtor          -> pp "Duplicated 'initialized by' section for asset"
  | DuplicatedPKey                     -> pp "Duplicated key"
  | DuplicatedVarDecl i                -> pp "Duplicated variable declaration: %a" pp_ident i
  | AnonymousFieldInEffect             -> pp "Anonymous field in effect"
  | EmptyEnumDecl                      -> pp "Empty state/enum declaration"
  | ExpressionExpected                 -> pp "Expression expected"
  | ForeignState (i1, i2)              -> pp "Expecting a state of %a, not %a" pp_ident (Option.get_dfl "<global>" i1) pp_ident (Option.get_dfl "<global>" i2)
  | FormulaExpected                    -> pp "Formula expected"
  | IncompatibleTypes (t1, t2)         -> pp "Incompatible types: found '%a' but expected '%a'" Printer_ast.pp_ptyp t1 Printer_ast.pp_ptyp t2
  | InvalidActionDescription           -> pp "Invalid action description"
  | InvalidActionExpression            -> pp "Invalid action expression"
  | InvalidArcheTypeDecl               -> pp "Invalid Archetype declaration"
  | InvalidAssetCollectionExpr ty      -> pp "Invalid asset collection expression: %a" M.pp_ptyp ty
  | InvalidAssetExpression             -> pp "Invalid asset expression"
  | InvalidCallByExpression            -> pp "Invalid 'Calledby' expression"
  | InvalidExpressionForEffect         -> pp "Invalid expression for effect"
  | InvalidExpression                  -> pp "Invalid expression"
  | InvalidFieldsCountInRecordLiteral  -> pp "Invalid fields count in record literal"
  | InvalidLValue                      -> pp "Invalid left-value"
  | InvalidFormula                     -> pp "Invalid formula"
  | InvalidInstruction                 -> pp "Invalid instruction"
  | InvalidNumberOfArguments (n1, n2)  -> pp "Invalid number of arguments: found '%i', but expected '%i'" n1 n2
  | InvalidRoleExpression              -> pp "Invalid role expression"
  | InvalidSecurityAction              -> pp "Invalid security action"
  | InvalidSecurityRole                -> pp "Invalid security role"
  | InvalidSortingExpression           -> pp "Invalid sorting expression"
  | InvalidStateExpression             -> pp "Invalid state expression"
  | InvalidTypeForVarWithFromTo        -> pp "A variable with a from/to declaration must be of type currency"
  | LetInElseInInstruction             -> pp "Let In else in instruction"
  | LetInElseOnNonOption               -> pp "Let in else on non-option type"
  | MissingFieldInRecordLiteral i      -> pp "Missing field in record literal: %a" pp_ident i
  | MixedAnonInRecordLiteral           -> pp "Mixed anonymous in record literal"
  | MixedFieldNamesInRecordLiteral l   -> pp "Mixed field names in record literal: %a" (Printer_tools.pp_list "," pp_ident) l
  | MoreThanOneInitState l             -> pp "More than one initial state: %a" (Printer_tools.pp_list ", " pp_ident) l
  | MultipleAssetStateDeclaration      -> pp "Multiple asset states declaration"
  | MultipleInitialMarker              -> pp "Multiple 'initial' marker"
  | MultipleFromToInVarDecl            -> pp "Variable declaration must have at most one from/to specification"
  | MultipleStateDeclaration           -> pp "Multiple state declaration"
  | NameIsAlreadyBound (i, None)       -> pp "Name is already bound: %a" pp_ident i
  | NameIsAlreadyBound (i, Some l)     -> pp "Name is already bound: %a (previous definition: %s)" pp_ident i (Location.tostring l)
  | NoSuchMethod i                     -> pp "No such method: %a" pp_ident i
  | NoSuchSecurityPredicate i          -> pp "No such security predicate: %a" pp_ident i
  | NonCodeLabel i                     -> pp "Not a code label: %a" pp_ident i
  | NonLoopLabel i                     -> pp "Not a loop label: %a" pp_ident i
  | NotAKeyOfType                      -> pp "pkey-of type expected"
  | NotAnAssetType                     -> pp "Asset type expected"
  | NotAnEnumType                      -> pp "Enumeration type expected"
  | NotAPrimitiveType                  -> pp "Primitive type expected"
  | NotARole i                         -> pp "Not a role: %a" pp_ident i
  | NumericExpressionExpected          -> pp "Expecting numerical expression"
  | OpInRecordLiteral                  -> pp "Operation in record literal"
  | OrphanedLabel i                    -> pp "Label not used: %a" pp_ident i
  | PartialMatch ps                    -> pp "Partial match (%a)" (Printer_tools.pp_list ", " pp_ident) ps
  | ReadOnlyGlobal i                   -> pp "Global is read only: %a" pp_ident i
  | SecurityInExpr                     -> pp "Found securtiy predicate in expression"
  | SpecOperatorInExpr                 -> pp "Specification operator in expression"
  | TransferWithoutDest                -> pp "Transfer without destination"
  | UninitializedVar                   -> pp "This variable declaration is missing an initializer"
  | UnknownAction i                    -> pp "Unknown action: %a" pp_ident i
  | UnknownAsset i                     -> pp "Unknown asset: %a" pp_ident i
  | UnknowContractEntryPoint (c, m)    -> pp "Unknown contract entry point: %s.%s" c m
  | UnknownEnum i                      -> pp "Unknown enum: %a" pp_ident i
  | UnknownField (i1, i2)              -> pp "Unknown field: asset %a does not have a field %a" pp_ident i1 pp_ident i2
  | UnknownFieldName i                 -> pp "Unknown field name: %a" pp_ident i
  | UnknownLabel i                     -> pp "Unknown label: %a" pp_ident i
  | UnknownLocalOrVariable i           -> pp "Unknown local or variable: %a" pp_ident i
  | UnknownProcedure i                 -> pp "Unknown procedure: %a" pp_ident i
  | UnknownState i                     -> pp "Unknown state: %a" pp_ident i
  | UnknownTypeName i                  -> pp "Unknown type: %a" pp_ident i
  | UnpureInFormula                    -> pp "Cannot use expression with side effect"
  | UselessPattern                     -> pp "Useless match branch"
  | VoidMethodInExpr                   -> pp "Expecting arguments"

  | NoMatchingOperator (op, sig_) ->
    pp "No matches for operator %a(%a)"
      pp_operator op
      (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) sig_

  | MultipleMatchingOperator (op, sig_, sigs) ->
    pp "Multiple matches for operator %a(%a): %a"
      pp_operator op
      (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) sig_
      (Printer_tools.pp_list ", " (fun fmt sig_ ->
           Format.fprintf fmt "(%a) -> %a"
             (Printer_tools.pp_list " * " Printer_ast.pp_ptyp) sig_.osl_sig
             Printer_ast.pp_ptyp sig_.osl_ret)) sigs

(* -------------------------------------------------------------------- *)
type argtype = [`Type of M.type_ | `Effect of ident]

(* -------------------------------------------------------------------- *)
let eqtypes =
  [ M.VTbool           ;
    M.VTint            ;
    M.VTrational       ;
    M.VTdate           ;
    M.VTduration       ;
    M.VTstring         ;
    M.VTaddress        ;
    M.VTrole           ;
    M.VTcurrency       ;
    M.VTkey            ;
    M.VTbytes          ]

let cmptypes =
  [ M.VTint            ;
    M.VTrational       ;
    M.VTdate           ;
    M.VTduration       ;
    M.VTstring         ;
    M.VTcurrency       ;
    M.VTbytes          ]

let grptypes =
  [ M.VTdate           ;
    M.VTduration       ;
    M.VTcurrency       ]

let rgtypes =
  [ M.VTint      ;
    M.VTrational ]

(* -------------------------------------------------------------------- *)
let cmpsigs : (PT.operator * (M.vtyp list * M.vtyp)) list =
  let ops  = [PT.Gt; PT.Ge; PT.Lt; PT.Le] in
  let sigs = List.map (fun ty -> ([ty; ty], M.VTbool)) cmptypes in
  List.mappdt (fun op sig_ -> (PT.Cmp op, sig_)) ops sigs

let opsigs =
  let _eqsigs : (PT.operator * (M.vtyp list * M.vtyp)) list =
    let ops  = [PT.Equal; PT.Nequal] in
    let sigs = List.map (fun ty -> ([ty; ty], M.VTbool)) eqtypes in
    List.mappdt (fun op sig_ -> (PT.Cmp op, sig_)) ops sigs in

  let grptypes : (PT.operator * (M.vtyp list * M.vtyp)) list =
    let ops  =
      (List.map (fun x -> PT.Arith x) [PT.Plus ; PT.Minus])
      @ (List.map (fun x -> PT.Unary x) [PT.Uplus; PT.Uminus]) in
    let sigs = List.map (fun ty -> ([ty; ty], ty)) grptypes in
    List.mappdt (fun op sig_ -> (op, sig_)) ops sigs in

  let rgtypes : (PT.operator * (M.vtyp list * M.vtyp)) list =
    let ops  =
      (List.map (fun x -> PT.Arith x) [PT.Plus; PT.Minus; PT.Mult; PT.Div])
      @ (List.map (fun x -> PT.Unary x) [PT.Uplus; PT.Uminus]) in
    let sigs = List.map (fun ty -> ([ty; ty], ty)) rgtypes in
    List.mappdt (fun op sig_ -> (op, sig_)) ops sigs in

  let ariths : (PT.operator * (M.vtyp list * M.vtyp)) list =
    [ PT.Arith PT.Modulo, ([M.VTint; M.VTint], M.VTint)] in

  let bools : (PT.operator * (M.vtyp list * M.vtyp)) list =
    let unas = List.map (fun x -> PT.Unary   x) [PT.Not] in
    let bins = List.map (fun x -> PT.Logical x) [PT.And; PT.Or; PT.Imply; PT.Equiv] in

    List.map (fun op -> (op, ([M.VTbool], M.VTbool))) unas
    @ List.map (fun op -> (op, ([M.VTbool; M.VTbool], M.VTbool))) bins in

  let others : (PT.operator * (M.vtyp list * M.vtyp)) list =
    [ PT.Arith PT.Plus, ([M.VTdate    ; M.VTduration      ], M.VTdate)      ;
      PT.Arith PT.Plus, ([M.VTint     ; M.VTduration      ], M.VTduration)  ;
      PT.Arith PT.Mult, ([M.VTrational; M.VTcurrency      ], M.VTcurrency)  ;
      PT.Arith PT.Mult, ([M.VTcurrency; M.VTrational      ], M.VTcurrency)
        ] in

  cmpsigs @ grptypes @ rgtypes @ ariths @ bools @ others

let opsigs =
  let doit (args, ret) =
    { osl_sig = List.map (fun x -> M.Tbuiltin x) args;
      osl_ret = M.Tbuiltin ret; } in
  List.map (snd_map doit) opsigs

(* -------------------------------------------------------------------- *)
type acttx = [
  | `Action     of PT.action_decl
  | `Transition of PT.transition_decl
]

type groups = {
  gr_archetypes  : (PT.lident * PT.exts)      loced list;
  gr_states      : PT.enum_decl               loced list;
  gr_enums       : (PT.lident * PT.enum_decl) loced list;
  gr_assets      : PT.asset_decl              loced list;
  gr_vars        : PT.variable_decl           loced list;
  gr_funs        : PT.s_function              loced list;
  gr_acttxs      : acttx                      loced list;
  gr_specs       : PT.specification           loced list;
  gr_secs        : PT.security                loced list;
  gr_externals   : PT.contract_decl           loced list;
}

(* -------------------------------------------------------------------- *)
let globals = [
  ("balance"     , M.Cbalance     , M.vtcurrency);
  ("caller"      , M.Ccaller      , M.vtaddress);
  ("now"         , M.Cnow         , M.vtdate);
  ("source"      , M.Csource      , M.vtaddress);
  ("transferred" , M.Ctransferred , M.vtcurrency);
]

let statename = "state"

type method_ = {
  mth_name     : M.const;
  mth_purity   : [`Pure | `Effect];
  mth_totality : [`Total | `Partial];
  mth_sig      : mthtyp list * mthtyp option;
}

and mthtyp = [
  | `T        of M.ptyp
  | `The
  | `Pk
  | `Effect
  | `Asset
  | `SubColl
  | `Cmp
  | `Pred
  | `RExpr
  | `Ref      of int
]

let methods : (string * method_) list =
  let mk mth_name mth_purity mth_totality mth_sig =
    { mth_name; mth_purity; mth_totality; mth_sig; }
  in [
    ("isempty"     , mk M.Cisempty      `Pure   `Total   ([             ], Some (`T M.vtbool)));
    ("get"         , mk M.Cget          `Pure   `Partial ([`Pk          ], Some `The));
    ("add"         , mk M.Cadd          `Effect `Total   ([`The         ], None));
    ("remove"      , mk M.Cremove       `Effect `Total   ([`Pk          ], None));
    ("removeif"    , mk M.Cremoveif     `Effect `Total   ([`Pred        ], None));
    ("update"      , mk M.Cupdate       `Effect `Total   ([`Pk; `Effect ], None));
    ("add_update"  , mk M.Cadd_update   `Effect `Total   ([`Pk; `Effect ], None));
    ("contains"    , mk M.Ccontains     `Pure   `Total   ([`Pk          ], Some (`T M.vtbool)));
    ("nth"         , mk M.Cnth          `Pure   `Partial ([`T M.vtint   ], Some (`Asset)));
    ("select"      , mk M.Cselect       `Pure   `Total   ([`Pred        ], Some (`SubColl)));
    ("sort"        , mk M.Csort         `Pure   `Total   ([`Cmp         ], Some (`SubColl)));
    ("count"       , mk M.Ccount        `Pure   `Total   ([             ], Some (`T M.vtint)));
    ("sum"         , mk M.Csum          `Pure   `Total   ([`RExpr       ], Some (`Ref 0)));
    ("max"         , mk M.Cmax          `Pure   `Partial ([`RExpr       ], Some (`Ref 0)));
    ("min"         , mk M.Cmin          `Pure   `Partial ([`RExpr       ], Some (`Ref 0)));
    ("subsetof"    , mk M.Csubsetof     `Pure   `Total   ([`SubColl     ], Some (`T M.vtbool)));
    ("head"        , mk M.Chead         `Pure   `Total   ([`T M.vtint   ], Some (`SubColl)));
    ("tail"        , mk M.Ctail         `Pure   `Total   ([`T M.vtint   ], Some (`SubColl)));
    ("unmoved"     , mk M.Cunmoved      `Pure   `Total   ([             ], Some (`SubColl)));
    ("added"       , mk M.Cadded        `Pure   `Total   ([             ], Some (`SubColl)));
    ("removed"     , mk M.Cremoved      `Pure   `Total   ([             ], Some (`SubColl)));
    ("iterated"    , mk M.Citerated     `Pure   `Total   ([             ], Some (`SubColl)));
    ("toiterate"   , mk M.Ctoiterate    `Pure   `Total   ([             ], Some (`SubColl)));
  ]

let methods = Mid.of_list methods

(* -------------------------------------------------------------------- *)
type assetdecl = {
  as_name   : M.lident;
  as_fields : fielddecl list;
  as_pk     : M.lident;
  as_sortk  : M.lident list;
  as_invs   : (M.lident option * M.pterm) list;
  as_state  : M.lident option;
}

and fielddecl = {
  fd_name  : M.lident;
  fd_type  : M.ptyp;
  fd_dfl   : M.pterm option;
  fd_ghost : bool;
}

let get_field (x : ident) (decl : assetdecl) =
  List.Exn.find (fun fd -> x = L.unloc fd.fd_name) decl.as_fields

(* -------------------------------------------------------------------- *)
type vardecl = {
  vr_name   : M.lident;
  vr_type   : M.ptyp;
  vr_kind   : [`Constant | `Variable | `Ghost | `Enum];
  vr_invs   : M.lident M.label_term list;
  vr_def    : (M.pterm * [`Inline | `Std]) option;
  vr_tgt    : M.lident option * M.lident option;
  vr_core   : M.const option;
}

(* -------------------------------------------------------------------- *)
type 'env ispecification = [
  | `Predicate     of M.lident * (M.lident * M.ptyp) list * M.pterm
  | `Definition    of M.lident * (M.lident * M.ptyp) option * M.pterm
  | `Lemma         of M.lident * M.pterm
  | `Theorem       of M.lident * M.pterm
  | `Variable      of M.lident * M.pterm option
  | `Assert        of M.lident * M.pterm * (M.lident * M.pterm list) list * M.lident list
  | `Effect        of 'env * M.instruction
  | `Postcondition of M.lident * M.pterm * (M.lident * M.pterm list) list * M.lident list
]

(* -------------------------------------------------------------------- *)
type 'env fundecl = {
  fs_name  : M.lident;
  fs_args  : (M.lident * M.ptyp) list;
  fs_retty : M.ptyp;
  fs_body  : M.instruction;
  fs_spec  : 'env ispecification list option;
}

(* -------------------------------------------------------------------- *)
type txeffect = {
  tx_state  : M.lident;
  tx_when   : M.pterm option;
  tx_effect : M.instruction option;
}

type 'env tactiondecl = {
  ad_name   : M.lident;
  ad_args   : (M.lident * M.ptyp) list;
  ad_callby : M.lident list;
  ad_effect : [`Raw of M.instruction | `Tx of M.lident * txeffect list] option;
  ad_funs   : 'env fundecl option list;
  ad_reqs   : (M.lident option * M.pterm) list;
  ad_fais   : (M.lident option * M.pterm) list;
  ad_spec   : 'env ispecification list;
  ad_actfs  : bool;
}

(* -------------------------------------------------------------------- *)
type statedecl = {
  sd_name  : M.lident;
  sd_state : bool;
  sd_ctors : ctordecl list;
  sd_init  : ident;
}

and ctordecl = M.lident * (M.lident option * M.pterm) list

(* -------------------------------------------------------------------- *)
type contractdecl = {
  ct_name    : M.lident;
  ct_entries : (M.lident * (M.lident * M.ptyp) list) list;
}

(* -------------------------------------------------------------------- *)
let pterm_arg_as_pterm = function M.AExpr e -> Some e | _ -> None

(* -------------------------------------------------------------------- *)
let core_types = [
  ("string"   , M.vtstring         );
  ("int"      , M.vtint            );
  ("rational" , M.vtrational       );
  ("bool"     , M.vtbool           );
  ("role"     , M.vtrole           );
  ("address"  , M.vtaddress        );
  ("date"     , M.vtdate           );
  ("tez"      , M.vtcurrency       );
  ("duration" , M.vtduration       );
  ("bytes"    , M.vtbytes          );
]

(* -------------------------------------------------------------------- *)
module Env : sig
  type t

  type label_kind = [`Plain | `Code | `Loop of ident]

  type entry = [
    | `Label       of t * label_kind
    | `State       of statedecl
    | `StateByCtor of statedecl * M.lident
    | `Type        of M.ptyp
    | `Local       of M.ptyp
    | `Global      of vardecl
    | `Asset       of assetdecl
    | `Action      of t tactiondecl
    | `Function    of t fundecl
    | `Field       of ident
    | `Contract    of contractdecl
    | `Context     of assetdecl * ident option
  ]

  type ecallback = error -> unit

  val create       : ecallback -> t
  val emit_error   : t -> error -> unit
  val name_free    : t -> ident -> [`Free | `Clash of Location.t option]
  val lookup_entry : t -> ident -> entry option
  val open_        : t -> t
  val close        : t -> t
  val inscope      : t -> (t -> t * 'a) -> t * 'a

  module Label : sig
    val lookup : t -> ident -> (t * label_kind) option
    val get    : t -> ident -> t * label_kind
    val exists : t -> ident -> bool
    val push   : t -> M.lident * label_kind -> t
  end

  module Type : sig
    val lookup : t -> ident -> M.ptyp option
    val get    : t -> ident -> M.ptyp
    val exists : t -> ident -> bool
    val push   : t -> (M.lident * M.ptyp) -> t
  end

  module Local : sig
    val lookup : t -> ident -> (ident * M.ptyp) option
    val get    : t -> ident -> (ident * M.ptyp)
    val exists : t -> ident -> bool
    val push   : t -> M.lident * M.ptyp -> t
  end

  module Var : sig
    val lookup : t -> ident -> vardecl option
    val get    : t -> ident -> vardecl
    val exists : t -> ident -> bool
    val push   : t -> vardecl -> t
  end

  module Function : sig
    val lookup : t -> ident -> t fundecl option
    val get    : t -> ident -> t fundecl
    val exists : t -> ident -> bool
    val push   : t -> t fundecl -> t
  end

  module State : sig
    val lookup : t -> ident -> statedecl option
    val get    : t -> ident -> statedecl
    val exists : t -> ident -> bool
    val byctor : t -> ident -> statedecl option
    val push   : t -> statedecl -> t
  end

  module Asset : sig
    val lookup  : t -> ident -> assetdecl option
    val get     : t -> ident -> assetdecl
    val exists  : t -> ident -> bool
    val byfield : t -> ident -> (assetdecl * fielddecl) option
    val push    : t -> assetdecl -> t
  end

  module TAction : sig
    val lookup  : t -> ident -> t tactiondecl option
    val get     : t -> ident -> t tactiondecl
    val exists  : t -> ident -> bool
    val push    : t -> t tactiondecl -> t
  end

  module Contract : sig
    val lookup  : t -> ident -> contractdecl option
    val get     : t -> ident -> contractdecl
    val exists  : t -> ident -> bool
    val push    : t -> contractdecl -> t
  end

  module Context : sig
    val the  : ident
    val push : t -> ident -> t
  end
end = struct
  type ecallback = error -> unit

  type label_kind = [`Plain | `Code | `Loop of ident]

  type entry = [
    | `Label       of t * label_kind
    | `State       of statedecl
    | `StateByCtor of statedecl * M.lident
    | `Type        of M.ptyp
    | `Local       of M.ptyp
    | `Global      of vardecl
    | `Asset       of assetdecl
    | `Action      of t tactiondecl
    | `Function    of t fundecl
    | `Field       of ident
    | `Contract    of contractdecl
    | `Context     of assetdecl * ident option
  ]

  and t = {
    env_error    : ecallback;
    env_bindings : (Location.t option * entry) Mid.t;
    env_context  : assetdecl list;
    env_locals   : Sid.t;
    env_scopes   : Sid.t list;
  }

  let ctxtname = "the"

  let create ecallback : t =
    { env_error    = ecallback;
      env_bindings = Mid.empty;
      env_context  = [];
      env_locals   = Sid.empty;
      env_scopes   = []; }

  let emit_error (env : t) (e : error) =
    env.env_error e

  let name_free (env : t) (x : ident) =
    if x = ctxtname then `Clash None else

    Option.map fst (Mid.find_opt x env.env_bindings)
    |> Option.map_dfl (fun x -> `Clash x) `Free

  let lookup_entry (env : t) (name : ident) : entry option =
    if   name = ctxtname
    then Option.map (fun x -> `Context (x, None)) (List.ohead env.env_context)
    else Option.map snd (Mid.find_opt name env.env_bindings)

  let lookup_gen (proj : entry -> 'a option) (env : t) (name : ident) : 'a option =
    Option.bind proj (lookup_entry env name)

  let push (env : t) ?(loc : Location.t option) (name : ident) (entry : entry) =
    let env = { env with
                env_bindings = Mid.add name (loc, entry) env.env_bindings } in

    match entry with
    | `Local _ -> { env with env_locals = Sid.add name env.env_locals }
    | _        -> env

  let open_ (env : t) =
    { env with
      env_locals = Sid.empty;
      env_scopes = env.env_locals :: env.env_scopes; }

  let close (env : t) =
    let lc, sc =
      match env.env_scopes with lc :: sc -> lc, sc | _ -> assert false in

    let bds =
      Sid.fold
        (fun x bds -> Mid.remove x bds) env.env_locals env.env_bindings in

    { env with env_bindings = bds; env_locals = lc; env_scopes = sc; }

  let inscope (env : t) (f : t -> t * 'a) =
    let env, aout = f (open_ env) in (close env, aout)

  module Label = struct
    let proj (entry : entry) =
      match entry with
      | `Label x    -> Some x
      | _           -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) ((name, kind) : M.lident * label_kind) =
      push env ~loc:(loc name) (unloc name) (`Label (env, kind))
  end

  module Type = struct
    let proj (entry : entry) =
      match entry with
      | `Type  x       -> Some x
      | `Asset decl    -> Some (M.Tasset decl.as_name)
      | `State decl    -> Some (M.Tenum decl.sd_name)
      | `Contract decl -> Some (M.Tcontract decl.ct_name)
      | _              -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) ((name, ty) : M.lident * M.ptyp) =
      push env ~loc:(loc name) (unloc name) (`Type ty)
  end

  module State = struct
    let proj (entry : entry) =
      match entry with
      | `State x -> Some x
      | _        -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let byctor (env : t) (name : ident) =
      match lookup_entry env name with
      | Some (`StateByCtor (decl, _)) -> Some decl
      | _ -> None

    let push (env : t) (decl : statedecl) =
      let env =
        List.fold_left
          (fun env (name, _) ->
             (push env ~loc:(loc name) (unloc name) (`StateByCtor (decl, name))))
          env decl.sd_ctors in
      push env (unloc decl.sd_name) (`State decl)
  end

  module Local = struct
    let proj = function `Local x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      Option.map (fun ty -> (name, ty)) (lookup_gen proj env name)

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) ((x, ty) : M.lident * M.ptyp) =
      push env ~loc:(loc x) (unloc x) (`Local ty)
  end

  module Var = struct
    let proj = function
      | `Global x ->
        Some x

      | `Asset  a ->
        Some { vr_name = a.as_name;
               vr_type = M.Tcontainer (M.Tasset a.as_name, M.Collection);
               vr_kind = `Constant;
               vr_invs = [];
               vr_core = None;
               vr_tgt  = (None, None);
               vr_def  = None; }

      | `StateByCtor (enum, ctor) ->
        Some { vr_name = ctor;
               vr_type = M.Tenum enum.sd_name;
               vr_kind = `Enum;
               vr_invs = [];
               vr_core = None;
               vr_tgt  = (None, None);
               vr_def  = None; }

      | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (decl : vardecl) =
      push env ~loc:(loc decl.vr_name) (unloc decl.vr_name) (`Global decl)
  end

  module Function = struct
    let proj = function `Function x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (decl : t fundecl) =
      push env ~loc:(loc decl.fs_name) (unloc decl.fs_name) (`Function decl)
  end

  module Asset = struct
    let proj = function `Asset x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let byfield (env : t) (fname : ident) =
      Option.bind
        (function
          | `Field nm ->
            let decl  = get env nm in
            let field = get_field fname decl in
            Some (decl, Option.get field)
          | _ -> None)
        (lookup_entry env fname)

    let push (env : t) ({ as_name = nm } as decl : assetdecl) : t =
      let env = push env ~loc:(loc nm) (unloc nm) (`Asset decl) in
      List.fold_left
        (fun env fd -> push env ~loc:(loc fd.fd_name)
                         (unloc fd.fd_name) (`Field (unloc nm)))
        env decl.as_fields
  end

  module TAction = struct
    let proj = function `Action x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (act : t tactiondecl) =
      push env ~loc:(loc act.ad_name) (unloc act.ad_name) (`Action act)
  end

  module Contract = struct
    let proj = function `Contract x -> Some x | _ -> None

    let lookup (env : t) (name : ident) =
      lookup_gen proj env name

    let exists (env : t) (name : ident) =
      Option.is_some (lookup env name)

    let get (env : t) (name : ident) =
      Option.get (lookup env name)

    let push (env : t) (ctt : contractdecl) =
      push env ~loc:(loc ctt.ct_name) (unloc ctt.ct_name) (`Contract ctt)
  end

  module Context = struct
    let the : ident = ctxtname

    let push (env : t) (asset : ident) =
      let asset = Asset.get env asset in
      { env with
          env_context  = asset :: env.env_context;
          env_bindings = List.fold_left (fun bds fd ->
            Mid.add (unloc fd.fd_name)
              (None, `Context (asset, Some (unloc fd.fd_name))) bds
          ) env.env_bindings asset.as_fields; }
  end
end

type env = Env.t

let coreloc = { Location.dummy with loc_fname = "<stdlib>" }

let empty : env =
  let cb (lc, error) =
    let str : string = Format.asprintf "%a@." pp_error_desc error in
    let pos : Position.t list = [location_to_position lc] in
    Error.error_alert pos str (fun _ -> ());
  in

  let env = Env.create cb in

  let env =
    List.fold_left
      (fun env (name, ty) -> Env.Type.push env (mkloc coreloc name, ty))
      env core_types in

  let env =
    let mk vr_name vr_type vr_core =
      let def = M.Pconst vr_core in
      let def = M.mk_sp ~type_:vr_type  def in

      { vr_name; vr_type; vr_core = Some vr_core;
        vr_tgt  = (None, None);
        vr_def  = Some (def, `Inline);
        vr_kind = `Constant;
        vr_invs = [];
      } in

    List.fold_left
      (fun env (name, const, ty) ->
         Env.Var.push env (mk (mkloc L.dummy name) ty const))
      env globals in

  env

(* -------------------------------------------------------------------- *)
let check_and_emit_name_free (env : env) (x : M.lident) =
  match Env.name_free env (unloc x) with
  | `Free ->
    true

  | `Clash olc ->
    Env.emit_error env (loc x, NameIsAlreadyBound (unloc x, olc));
    false

(* --------------------------------------------------------------------- *)
let select_operator env loc (op, tys) =
  match op with
  | PT.Cmp (PT.Equal | PT.Nequal) -> begin
      match tys with
      | [t1; t2] when Type.equal t1 t2 ->
        Some ({ osl_sig = [t1; t2]; osl_ret = M.Tbuiltin M.VTbool; })

      | _ ->
        Env.emit_error env
          (loc, NoMatchingOperator (op, tys));
        None
    end

  | _ -> begin
    let filter (sig_ : opsig) =
      Type.sig_compatible ~from_:tys ~to_:sig_.osl_sig in

    match List.filter filter (List.assoc_all op opsigs) with
    | [] ->
      Env.emit_error env
        (loc, NoMatchingOperator (op, tys));
      None

    | _::_::_ as sigs -> begin
        let module E = struct exception Bailout end in

        try
          let sig_ =
            match
              List.filter
                (fun sig_ -> Type.sig_equal sig_.osl_sig tys)
                sigs
            with [sig_] -> sig_ | _ -> raise E.Bailout in

          List.iter (fun sig2 ->
              if not (Type.sig_compatible ~from_:sig_.osl_sig ~to_:sig2.osl_sig) then
                raise E.Bailout
            ) sigs;

          Some sig_

        with E.Bailout ->
          Env.emit_error env
            (loc, MultipleMatchingOperator (op, tys, sigs));
          None
      end

    | [sig_] ->
      Some sig_
  end

(* -------------------------------------------------------------------- *)
let for_container (_ : env) = function
  | PT.Collection-> M.Collection
  | PT.Partition -> M.Partition

(* -------------------------------------------------------------------- *)
let for_assignment_operator = function
  | PT.ValueAssign  -> M.ValueAssign
  | PT.PlusAssign   -> M.PlusAssign
  | PT.MinusAssign  -> M.MinusAssign
  | PT.MultAssign   -> M.MultAssign
  | PT.DivAssign    -> M.DivAssign
  | PT.AndAssign    -> M.AndAssign
  | PT.OrAssign     -> M.OrAssign

(* -------------------------------------------------------------------- *)
let tt_logical_operator (op : PT.logical_operator) =
  match op with
  | And   -> M.And
  | Or    -> M.Or
  | Imply -> M.Imply
  | Equiv -> M.Equiv

(* -------------------------------------------------------------------- *)
let tt_arith_operator (op : PT.arithmetic_operator) =
  match op with
  | Plus   -> M.Plus
  | Minus  -> M.Minus
  | Mult   -> M.Mult
  | Div    -> M.Div
  | DivRat -> M.DivRat
  | Modulo -> M.Modulo

(* -------------------------------------------------------------------- *)
let tt_cmp_operator (op : PT.comparison_operator) =
  match op with
  | Equal  -> M.Equal
  | Nequal -> M.Nequal
  | Gt     -> M.Gt
  | Ge     -> M.Ge
  | Lt     -> M.Lt
  | Le     -> M.Le

(* -------------------------------------------------------------------- *)
exception InvalidType

let rec for_type_exn (env : env) (ty : PT.type_t) : M.ptyp =
  match unloc ty with
  | Tref x -> begin
      match Env.Type.lookup env (unloc x) with
      | None ->
        Env.emit_error env (loc x, UnknownTypeName (unloc x));
        raise InvalidType
      | Some ty -> ty
    end

  | Tasset x ->
    let decl = Env.Asset.lookup env (unloc x) in
    M.Tasset (Option.get_exn InvalidType decl).as_name

  | Tcontainer (ty, ctn) ->
    M.Tcontainer (for_type_exn env ty, for_container env ctn)

  | Tlist ty ->
    M.Tcontainer (for_type_exn env ty, M.List)

  | Ttuple tys ->
    M.Ttuple (List.map (for_type_exn env) tys)

  | Toption ty ->
    M.Toption (for_type_exn env ty)

  | Tkeyof ty -> begin
      match for_type_exn env ty with
      | M.Tasset x ->
        let decl = Env.Asset.get env (unloc x) in
        let ctor = Env.Asset.byfield env (unloc decl.as_pk) in
        (snd (Option.get ctor)).fd_type

      | _ ->
        Env.emit_error env (loc ty, NotAnAssetType);
        raise InvalidType
    end

let for_type (env : env) (ty : PT.type_t) : M.ptyp option =
  try Some (for_type_exn env ty) with InvalidType -> None

(* -------------------------------------------------------------------- *)
let for_asset_type (env : env) (ty : PT.type_t) : M.lident option =
  match Option.map Type.as_asset (for_type env ty) with
  | None ->
    None
  | Some None ->
    Env.emit_error env (loc ty, NotAnAssetType);
    None
  | Some (Some x) ->
    Some x

(* -------------------------------------------------------------------- *)
let for_asset_keyof_type (env : env) (ty : PT.type_t) : M.lident option =
  match unloc ty with
  | PT.Tkeyof t ->
    for_asset_type env t
  | _ ->
    Env.emit_error env (loc ty, NotAKeyOfType);
    None

(* -------------------------------------------------------------------- *)
let for_literal (_env : env) (topv : PT.literal loced) : M.bval =
  let mk_sp type_ node = M.mk_sp ~loc:(loc topv) ~type_ node in

  match unloc topv with
  | Lbool b ->
    mk_sp M.vtbool (M.BVbool b)

  | Lnumber i ->
    mk_sp M.vtint (M.BVint i)

  | Ldecimal str ->
    begin
     let n, d = Core.decimal_string_to_rational str in
     mk_sp M.vtrational (M.BVrational (n, d))
    end

  | Lstring s ->
    mk_sp M.vtstring (M.BVstring s)

  | Ltz tz ->
    mk_sp (M.vtcurrency) (M.BVcurrency (M.Tz,  tz))

  | Lmtz tz ->
    mk_sp (M.vtcurrency) (M.BVcurrency (M.Mtz, tz))

  | Lutz tz ->
    mk_sp (M.vtcurrency) (M.BVcurrency (M.Utz, tz))

  | Laddress a ->
    mk_sp M.vtaddress (M.BVaddress a)

  | Lduration d ->
    mk_sp M.vtduration (M.BVduration (Core.string_to_duration d))

  | Ldate d ->
    mk_sp M.vtdate (M.BVdate (Core.string_to_date d))

  | Lbytes s ->
    mk_sp M.vtbytes (M.BVbytes (s))

(* -------------------------------------------------------------------- *)
type emode_t = [`Expr | `Formula]

let rec for_xexpr (mode : emode_t) (env : env) ?(ety : M.ptyp option) (tope : PT.expr) =
  let for_xexpr = for_xexpr mode in

  let module E = struct exception Bailout end in

  let bailout = fun () -> raise E.Bailout in

  let mk_sp type_ node = M.mk_sp ~loc:(loc tope) ?type_ node in
  let dummy type_ : M.pterm = mk_sp type_ (M.Pvar (VTnone, mkloc (loc tope) "<error>")) in

  let doit () =
    match unloc tope with
    | Eterm (st, x) -> begin
        let before = st.before in
        let vt, subenv =
          match st.before, st.label with
          | true, Some _ ->
              Env.emit_error env (loc tope, BeforeWithLabel);
              M.VTnone, env

          | true , None ->
              M.VTbefore, env

          | false, Some lbl -> begin
              match Env.Label.lookup env (unloc lbl) with
              | None ->
                Env.emit_error env (loc lbl, UnknownLabel (unloc lbl));
                M.VTnone, env
              | Some (subenv, `Code) ->
                M.VTat (unloc lbl), subenv
              | Some (_, _) ->
                Env.emit_error env (loc lbl, NonCodeLabel (unloc lbl));
                M.VTnone, env
            end

          | false, None ->
              M.VTnone, env
        in

        let vt =
          let hasvt = st.before || Option.is_some st.label in
          if hasvt && mode <> `Formula then begin
            Env.emit_error env (loc tope, BeforeOrLabelInExpr); M.VTnone
          end else vt in

        match Env.lookup_entry subenv (unloc x) with
        | Some (`Local xty) ->
          if before then
            Env.emit_error env (loc tope, BeforeIrrelevant `Local);
          mk_sp (Some xty) (M.Pvar (VTnone, x))

        | Some (`Global decl) -> begin
            match decl.vr_def with
            | Some (body, `Inline) ->
              body
            | _ ->
              mk_sp (Some decl.vr_type) (M.Pvar (vt, x))
          end

        | Some (`Asset decl) ->
          let typ = M.Tcontainer ((M.Tasset decl.as_name), M.Collection) in
          mk_sp (Some typ) (M.Pvar (vt, x))

        | Some (`StateByCtor (decl, _)) ->
          if before then
            Env.emit_error env (loc tope, BeforeIrrelevant `State);

          let typ = M.Tenum decl.sd_name in
          mk_sp (Some typ) (M.Pvar (VTnone, x))

        | Some (`Context (asset, ofield)) -> begin
          let atype = M.Tasset asset.as_name in
          let var   = mkloc (loc tope) Env.Context.the in
          let the   = mk_sp (Some atype) (M.Pvar (VTnone, var)) in

          if before then
            Env.emit_error env (loc tope, BeforeIrrelevant `Local);
          match ofield with
          | None ->
              the
          | Some fname ->
              let fty = (Option.get (get_field fname asset)).fd_type in
              mk_sp (Some fty) (M.Pdot (the, mkloc (loc tope) fname))
          end

        | _ ->
          Env.emit_error env (loc x, UnknownLocalOrVariable (unloc x));
          bailout ()
      end

    | Eliteral v ->
      let v = for_literal env (mkloc (loc tope) v) in
      mk_sp v.M.type_ (M.Plit v)

    | Earray [] -> begin
        match ety with
        | Some (M.Tcontainer (_, _)) ->
          mk_sp ety (M.Parray [])

        | _ ->
          Env.emit_error env (loc tope, CannotInferCollectionType);
          bailout ()
      end

    | Earray (e :: es) -> begin
        let elty = Option.bind (Option.map fst |@ Type.as_container) ety in
        let e    = for_xexpr env ?ety:elty e in
        let elty = if Option.is_some e.M.type_ then e.M.type_ else elty in
        let es   = List.map (fun e -> for_xexpr env ?ety:elty e) es in

        match ety with
        | Some (M.Tcontainer (_, _)) ->
          mk_sp ety (M.Parray (e :: es))

        | _ ->
          Env.emit_error env (loc tope, CannotInferCollectionType);
          bailout ()
      end

    | Erecord fields -> begin
        let module E = struct
          type state = {
            hasupdate : bool;
            fields    : ident list;
            anon      : bool;
          }

          let state0 = {
            hasupdate = false; fields = []; anon = false;
          }
        end in

        let is_update = function
          | (None | Some (PT.ValueAssign, _)) -> false
          |  _ -> true in

        let infos = List.fold_left (fun state (fname, _) ->
            E.{ hasupdate = state.hasupdate || is_update fname;
                fields    = Option.fold
                    (fun names (_, name)-> unloc name :: names)
                    state.fields fname;
                anon      = state.anon || Option.is_none fname; })
            E.state0 fields in

        if infos.E.hasupdate then
          Env.emit_error env (loc tope, OpInRecordLiteral);

        if infos.E.anon && not (List.is_empty (infos.E.fields)) then begin
          Env.emit_error env (loc tope, MixedAnonInRecordLiteral);
          bailout ()
        end;

        if infos.E.anon || List.is_empty fields then
          match Option.map Type.as_asset ety with
          | None | Some None ->
            Env.emit_error env (loc tope, CannotInferAnonRecord);
            bailout ()

          | Some (Some asset) ->
            let asset = Env.Asset.get env (unloc asset) in
            let ne, ng = List.length fields, List.length asset.as_fields in

            if ne <> ng then begin
              Env.emit_error env (loc tope, InvalidFieldsCountInRecordLiteral);
              bailout ()
            end;

            let fields =
              List.map2 (fun (_, fe) fd ->
                  for_xexpr env ~ety:fd.fd_type fe
                ) fields asset.as_fields;
            in mk_sp ety (M.Precord fields)

        else begin
          let fmap =
            List.fold_left (fun fmap (fname, e) ->
                let fname = unloc (snd (Option.get fname)) in

                Mid.update fname (function
                    | None -> begin
                        let asset = Env.Asset.byfield env fname in
                        if Option.is_none asset then begin
                          let err = UnknownFieldName fname in
                          Env.emit_error env (loc tope, err)
                        end; Some (asset, [e])
                      end

                    | Some (asset, es) ->
                      if List.length es = 1 then begin
                        let err = DuplicatedFieldInRecordLiteral fname in
                        Env.emit_error env (loc tope, err)
                      end; Some (asset, e :: es)) fmap
              ) Mid.empty fields
          in

          let assets =
            List.undup id (Mid.fold (fun _ (asset, _) assets ->
                Option.fold
                  (fun assets (asset, _) -> asset :: assets)
                  assets asset
              ) fmap []) in

          let assets = List.sort Stdlib.compare assets in

          let fields =
            Mid.map (fun (asset, es) ->
                let aty = Option.map (fun (_, fd) -> fd.fd_type) asset in
                List.map (fun e -> for_xexpr env ?ety:aty e) es
              ) fmap in

          let record =
            match assets with
            | [] ->
              bailout ()

            | _ :: _ :: _ ->
              let err =
                MixedFieldNamesInRecordLiteral
                  (List.map (fun x -> unloc x.as_name) assets)
              in Env.emit_error env (loc tope, err); bailout ()

            | [asset] ->
              let fields =
                List.map (fun ({ fd_name = { pldesc = fname } } as fd) ->
                  match fd.fd_dfl with
                  | None -> begin
                    match Mid.find_opt fname fields with
                    | None ->
                      let err = MissingFieldInRecordLiteral fname in
                      Env.emit_error env (loc tope, err); dummy (Some fd.fd_type)
                    | Some thisf ->
                      List.hd (List.rev thisf)
                    end
                  | Some dfl -> dfl
                ) asset.as_fields
              in mk_sp (Some (M.Tasset asset.as_name)) (M.Precord fields)

          in record
        end
      end

    | Etuple es -> begin
        let etys =
          match Option.bind Type.as_tuple ety with
          | Some etys when List.length etys = List.length es ->
            List.map Option.some etys
          | _ ->
            List.make (fun _ -> None) (List.length es) in

        let es = List.map2 (fun ety e -> for_xexpr env ?ety e) etys es in
        let ty = Option.get_all (List.map (fun x -> x.M.type_) es) in
        let ty = Option.map (fun x -> M.Ttuple x) ty in

        mk_sp ty (M.Ptuple es)
      end

    | Edot (pe, x) -> begin
        if Mid.mem (unloc x) methods then
          for_xexpr env ?ety (mkloc (loc tope) (PT.Emethod (pe, x, [])))
        else
          let e = for_xexpr env pe in

          match Option.map Type.as_asset e.M.type_ with
          | None ->
            bailout ()

          | Some None ->
            Env.emit_error env (loc pe, AssetExpected (Option.get e.M.type_));
            bailout ()

          | Some (Some asset) -> begin
              let asset = Env.Asset.get env (unloc asset) in

              match get_field (unloc x) asset with
              | None ->
                let err = UnknownField (unloc asset.as_name, unloc x) in
                Env.emit_error env (loc x, err); bailout ()

              | Some { fd_type = fty } ->
                mk_sp (Some fty) (M.Pdot (e, x))
            end
      end

    | Emulticomp (e, l) ->
      let e = for_xexpr env e in
      let l = List.map (snd_map (for_xexpr env)) l in

      let _, aout =
        List.fold_left_map (fun e ({ pldesc = op }, e') ->
            match e.M.type_, e'.M.type_ with
            | Some ty, Some ty' -> begin
              let aout =
                Option.map (fun sig_ ->
                  let term = M.Pcomp (tt_cmp_operator op, e, e') in
                  mk_sp (Some sig_.osl_ret) term
                ) (select_operator env (loc tope) (PT.Cmp op, [ty; ty']))
              in (e', aout)
            end

            | _, _ ->
              e', None)
          e l in

      begin match List.pmap (fun x -> x) aout with
        | [] ->
          let lit = M.mk_sp ~type_:M.vtbool ~loc:(loc tope) (M.BVbool true) in
          mk_sp (Some M.vtbool) (M.Plit lit)

        | e :: es ->
          List.fold_left (fun e e' ->
              (mk_sp (Some M.vtbool) (M.Plogical (tt_logical_operator And, e, e'))))
            e es
      end

    | Eapp (Foperator { pldesc = op }, args) -> begin
        let args = List.map (for_xexpr env) args in

        if List.exists (fun arg -> Option.is_none arg.M.type_) args then
          bailout ();

        let aty = List.map (fun a -> Option.get a.M.type_) args in
        let sig_ =
          Option.get_fdfl
            (fun () -> bailout ())
            (select_operator env (loc tope) (op, aty)) in

        let aout =
          match op with
          | Logical op ->
            let a1, a2 = Option.get (List.as_seq2 args) in
            M.Plogical (tt_logical_operator op, a1, a2)

          | Unary op -> begin
              let a1 = Option.get (List.as_seq1 args) in

              match
                match op with
                | PT.Not    -> `Not
                | PT.Uplus  -> `UArith (M.Uplus)
                | PT.Uminus -> `UArith (M.Uminus)
              with
              | `Not ->
                M.Pnot a1

              | `UArith op ->
                M.Puarith (op, a1)
            end

          | Arith op ->
            let a1, a2 = Option.get (List.as_seq2 args) in
            M.Parith (tt_arith_operator op, a1, a2)

          | Cmp op ->
            let a1, a2 = Option.get (List.as_seq2 args) in
            M.Pcomp (tt_cmp_operator op, a1, a2)

        in mk_sp (Some (sig_.osl_ret)) aout
      end

    | Emethod (the, m, args) -> begin
        let infos = for_gen_method_call mode env (loc tope) (`Parsed the, m, args) in
        let the, asset, method_, args, amap = Option.get_fdfl bailout infos in

        let type_of_mthtype = function
          | `T typ   -> Some typ
          | `The     -> Some (M.Tasset asset.as_name)
          | `Asset   -> Some (M.Tasset asset.as_name)
          | `SubColl -> Some (M.Tcontainer (M.Tasset asset.as_name, M.Collection))
          | `Ref i   -> Some (Mint.find i amap)
          | _        -> assert false
        in

        if Option.is_none (snd method_.mth_sig) then begin
          Env.emit_error env (loc tope, VoidMethodInExpr)
        end;

        begin match method_.mth_purity, mode with
          | `Effect, `Formula ->
            Env.emit_error env (loc tope, UnpureInFormula)
          | _, _ ->
            () end;

        let rty = Option.bind type_of_mthtype (snd method_.mth_sig) in
        let rty =
          match method_.mth_totality, mode with
          | `Partial, `Formula ->
            Option.map (fun x -> M.Toption x) rty
          | _, _ ->
            rty in

        mk_sp rty (M.Pcall (Some the, M.Cconst method_.mth_name, args))
      end

    | Eif (c, et, Some ef) ->
      let c    = for_xexpr env ~ety:M.vtbool c in
      let et   = for_xexpr env et in
      let ef   = for_xexpr env ?ety:et.type_ ef in
      let aout = mk_sp (Some M.vtbool) (M.Pif (c, et, ef)) in

      aout

    | Eletin (x, ty, e1, e2, oe) ->
      let ty  = Option.bind (for_type env) ty in
      let e   = for_xexpr env ?ety:ty e1 in
      let bty =
        if Option.is_some oe then
          Option.bind (fun bty ->
              match bty with
              | M.Toption bty -> Some bty
              | _ -> Env.emit_error env (loc tope, LetInElseOnNonOption); None
            ) e.M.type_
        else e.M.type_ in

      let env, body =
        let _ : bool = check_and_emit_name_free env x in
        Env.inscope env (fun env ->
            let env =
              Option.fold (fun env bty ->
                  Env.Local.push env (x, bty)) env bty
            in env, for_xexpr env e2) in

      let oe = Option.map (fun oe -> for_xexpr env ?ety:body.M.type_ oe) oe in

      mk_sp body.M.type_ (M.Pletin (x, e, ty, body, oe))

    | Evar (_lv, _t, _e1) ->
      assert false

    | Ematchwith (e, bs) -> begin
        match for_gen_matchwith mode env (loc tope) e bs with
        | None -> bailout () | Some (decl, me, (wd, bsm), es) ->

          let es  = List.map (for_xexpr env) es in
          let bty = List.find_opt (fun e -> Option.is_some e.M.type_) es in
          let bty = Option.bind (fun e -> e.M.type_) bty in

          Option.iter (fun bty ->
              List.iter (fun be ->
                  Option.iter (fun ety ->
                      if not (Type.equal bty ety) then
                        Env.emit_error env (loc tope, IncompatibleTypes (ety, bty))
                    ) be.M.type_)
                es
            ) bty;

          let aout = List.pmap (fun (cname, _) ->
              let ctor = M.mk_sp (M.Mconst cname) in (* FIXME: loc ? *)
              let bse  =
                match Mstr.find (unloc cname) bsm, wd with
                | Some i, _ ->
                  Some (List.nth es i)
                | None, Some _ ->
                  None
                | None, None ->
                  Some (dummy bty)
              in Option.map (fun bse -> (ctor, bse)) bse) decl.sd_ctors in

          let aout =
            Option.fold
              (fun aout extra -> aout @ [M.mk_sp M.Mwild, extra])
              aout (Option.map (List.nth es) wd) in

          mk_sp bty (M.Pmatchwith (me, aout))
      end

    | Equantifier (qt, x, xty, body) -> begin
        if mode <> `Formula then begin
          Env.emit_error env (loc tope, BindingInExpr);
          bailout ()
        end else
          match
            match xty with
            | PT.Qcollection xe ->
              let ast, xe = for_asset_collection_expr mode env (`Parsed xe) in
              Option.map (fun (ad, _) -> (Some ast, M.Tasset ad.as_name)) xe
            | PT.Qtype ty ->
              let ty = for_type env ty in
              Option.map (fun ty -> (None, ty)) ty
          with
          | None -> bailout () | Some (ast, xty) ->

            let _, body =
              Env.inscope env (fun env ->
                  let _ : bool = check_and_emit_name_free env x in
                  let env = Env.Local.push env (x, xty) in
                  env, for_formula env body) in

            let qt =
              match qt with
              | PT.Forall -> M.Forall
              | PT.Exists -> M.Exists in

            mk_sp (Some M.vtbool) (M.Pquantifer (qt, x, (ast, xty), body))
      end

    | Enothing
    | Eapp      _
    | Eassert   _
    | Elabel    _
    | Eassign   _
    | Ebreak
    | Efailif   _
    | Efor      _
    | Eiter     _
    | Eif       _
    | Erequire  _
    | Ereturn   _
    | Eoption   _
    | Eseq      _
    | Etransfer _
    | Einvalid ->
      Env.emit_error env (loc tope, InvalidExpression);
      bailout ()

  in

  try
    let aout = doit () in

    begin match aout, ety with
      | { type_ = Some from_ }, Some to_ ->
        if not (Type.compatible ~from_ ~to_) then
          Env.emit_error env (loc tope, IncompatibleTypes (from_, to_));

(*
      | _, Some _ ->
        Env.emit_error env (loc tope, ExpressionExpected)
*)

      | _, _ ->
        ()
    end;

    aout

  with E.Bailout -> dummy ety

(* -------------------------------------------------------------------- *)
and for_gen_matchwith (mode : emode_t) (env : env) theloc pe bs =
  let me = for_xexpr mode env pe in

  match me.M.type_ with
  | None ->
    None

  | Some (M.Tenum x) ->
    let decl = Env.State.get env (unloc x) in
    let bsm  = List.map (fun (ct, _) -> (unloc ct, None)) decl.sd_ctors in
    let bsm  = Mstr.of_list bsm in

    let wd, bsm = List.fold_lefti (fun bse bsm (pts, _) ->
        List.fold_left (fun (wd, bsm) pt ->
            let module E = struct exception Bailout end in

            try
              begin match unloc pt with
                | PT.Pref pid ->
                  if not (Mstr.mem (unloc pid) bsm) then begin
                  end;
                | PT.Pwild -> () end;

              match unloc pt with
              | PT.Pref pid ->
                let bsm =
                  Mstr.change (unloc pid) (function
                      | None   ->
                        Env.emit_error env (loc pt, AlienPattern);
                        raise E.Bailout

                      | Some None when Option.is_none wd ->
                        Some (bse)

                      | Some _ ->
                        Env.emit_error env (loc pt, UselessPattern);
                        raise E.Bailout
                    ) bsm
                in (wd, bsm)

              | PT.Pwild -> begin
                  match wd with
                  | None when Mstr.exists (fun _ v -> Option.is_none v) bsm ->
                    (Some bse, bsm)

                  | _ ->
                    Env.emit_error env (loc pt, UselessPattern);
                    raise E.Bailout
                end

            with E.Bailout -> (wd, bsm)) bsm pts
      ) (None, bsm) bs in

    if Option.is_none wd then begin
      let missing = Mstr.bindings bsm in
      let missing = List.filter (fun (_, v) -> Option.is_none v) missing in
      let missing = List.sort String.compare (List.map fst missing) in

      if not (List.is_empty missing) then
        Env.emit_error env (theloc, PartialMatch missing)
    end;

    Some (decl, me, (wd, bsm), (List.map snd bs))

  | Some _ ->
    Env.emit_error env (loc pe, NotAnEnumType);
    None

(* -------------------------------------------------------------------- *)
and for_asset_expr mode (env : env) (tope : PT.expr) =
  let ast = for_xexpr mode env tope in
  let typ =
    match Option.map Type.as_asset ast.M.type_ with
    | None ->
      None

    | Some None ->
      Env.emit_error env (loc tope, InvalidAssetExpression);
      None

    | Some (Some asset) ->
      Some (Env.Asset.get env (unloc asset))

  in (ast, typ)

(* -------------------------------------------------------------------- *)
and for_asset_collection_expr mode (env : env) tope =
  let ast =
    match tope with
    | `Typed   ast -> ast
    | `Parsed tope -> for_xexpr mode env tope
  in

  let typ =
    match Option.map Type.as_asset_collection ast.M.type_ with
    | None ->
      None

    | Some None ->
      Env.emit_error env
        (ast.M.loc, InvalidAssetCollectionExpr (Option.get ast.M.type_));
      None

    | Some (Some (asset, c)) ->
      Some (Env.Asset.get env (unloc asset), c)

  in (ast, typ)

(* -------------------------------------------------------------------- *)
and for_gen_method_call mode env theloc (the, m, args) =
  let module E = struct exception Bailout end in

  try
    let the, asset = for_asset_collection_expr mode env the in
    let asset, _ = Option.get_fdfl (fun () -> raise E.Bailout) asset in
    let method_ =
      match Mid.find_opt (unloc m) methods with
      | None ->
        Env.emit_error env (loc m, NoSuchMethod (unloc m));
        raise E.Bailout
      | Some method_ -> method_
    in

    let args =
      match args with
      | [ {pldesc = Etuple l; _} ] -> l
      | _ -> args
    in

    let ne = List.length (fst method_.mth_sig) in
    let ng = List.length args in

    if ne <> ng then begin
      Env.emit_error env (theloc, InvalidNumberOfArguments (ne, ng));
      raise E.Bailout
    end;

    let doarg arg (aty : mthtyp) =
      match aty with
      | `Pk ->
        let pk = Option.get (get_field (unloc asset.as_pk) asset) in
        M.AExpr (for_xexpr mode env ~ety:pk.fd_type arg)

      | `The ->
        M.AExpr (for_xexpr mode env ~ety:(Tasset asset.as_name) arg)

      | `Pred ->
        let env   = Env.Context.push env (unloc asset.as_name) in
        let theid = mkloc (loc arg) Env.Context.the in
        let thety = M.Tasset asset.as_name in
        M.AFun (theid, thety, for_xexpr mode env ~ety:M.vtbool arg)

      | `RExpr ->
        let env   = Env.Context.push env (unloc asset.as_name) in
        let theid = mkloc (loc arg) Env.Context.the in
        let thety = M.Tasset asset.as_name in
        let e     = for_xexpr mode env arg in

        e.M.type_ |> Option.iter (fun ty ->
            if not (Type.is_numeric ty) then
              Env.emit_error env (loc arg, NumericExpressionExpected));
        M.AFun (theid, thety, e)

      | `Effect ->
        M.AEffect (Option.get_dfl [] (for_arg_effect mode env asset arg))

      | `SubColl ->
        let ty = M.Tcontainer (Tasset asset.as_name, M.Collection) in
        M.AExpr (for_xexpr mode env ~ety:ty arg)

      | `T ty ->
        M.AExpr (for_xexpr mode env ~ety:ty arg)

      | `Cmp -> begin
          let asc, field =
            match unloc arg with
            | Eterm ({ before = false; label = None; }, f) ->
              (true, Some f)
            | Eapp (Fident { pldesc = ("asc" | "desc") as order },
                    [{pldesc = Eterm ({ before = false; label = None; }, f) }]) ->
              (order = "asc", Some f)
            | _ ->
              Env.emit_error env (loc arg, InvalidSortingExpression);
              (true, None) in

          let field = Option.bind (fun f ->
              match get_field (unloc f) asset with
              | None ->
                Env.emit_error env (loc f, UnknownFieldName (unloc f));
                None
              | Some _ -> Some f) field in

          let field = Option.get_fdfl (fun () -> mkloc (loc arg) "<error>") field in
          M.ASorting (asc, field)
        end

      | _ ->
        assert false

    in

    let args = List.map2 doarg args (fst method_.mth_sig) in
    let amap =
      let aout = ref Mint.empty in
      List.iteri (fun i arg ->
          match arg with
          | M.AExpr { M.type_ = Some ty } ->
            aout := Mint.add i ty !aout
          | M.AFun (_, _, { M.type_ = Some ty }) ->
            aout := Mint.add i ty !aout
          | _ -> ()) args; !aout in

    Some (the, asset, method_, args, amap)

  with E.Bailout -> None

(* -------------------------------------------------------------------- *)
and for_arg_effect mode (env : env) (asset : assetdecl) (tope : PT.expr) =
  match unloc tope with
  | Erecord fields ->
    let do1 map ((x, e) : PT.record_item) =
      match x with
      | None ->
        Env.emit_error env (loc tope, AnonymousFieldInEffect);
        map

      | Some (op, x) -> begin
          match get_field (unloc x) asset with
          | Some { fd_type = fty } ->
            let op  = for_assignment_operator op in
            let e   = for_assign_expr mode env (loc x) (op, fty) e in

            if Mid.mem (unloc x) map then begin
              Env.emit_error env (loc x, DuplicatedFieldInRecordLiteral (unloc x));
              map
            end else
              Mid.add (unloc x) (x, `Assign op, e) map

          | None ->
            Env.emit_error env (loc x, UnknownField (unloc asset.as_name, unloc x));
            map
        end
    in

    let effects = List.fold_left do1 Mid.empty fields in

    Some (List.map snd (Mid.bindings effects))

  | _ ->
    Env.emit_error env (loc tope, InvalidExpressionForEffect);
    None

(* -------------------------------------------------------------------- *)
and for_assign_expr mode env orloc (op, fty) e =
  let ety =
    match op with
    | ValueAssign ->
      Some fty

    | PlusAssign
    | MinusAssign
    | MultAssign
    | DivAssign ->
      if not (Type.is_numeric fty) then begin
        Env.emit_error env (orloc, NumericExpressionExpected);
        None
      end else
        Some fty

    | AndAssign
    | OrAssign ->
      if not (Type.compatible ~from_:fty ~to_:M.vtbool) then
        Env.emit_error env (orloc, IncompatibleTypes (fty, M.vtbool));
      Some M.vtbool

  in for_xexpr mode env ?ety e

(* -------------------------------------------------------------------- *)
and for_formula (env : env) (topf : PT.expr) : M.pterm =
  let e = for_xexpr `Formula ~ety:(M.Tbuiltin M.VTbool) env topf in
  Option.iter (fun ety ->
      if ety <> M.vtbool then
        Env.emit_error env (loc topf, FormulaExpected))
    e.type_; e

(* -------------------------------------------------------------------- *)
and for_action_description (env : env) (sa : PT.security_arg) : M.action_description =
  match unloc sa with
  | Sident { pldesc = "anyaction" } ->
    M.ADAny

  | Sapp (act, [{ pldesc = PT.Sident asset }]) -> begin
      let st : PT.s_term = PT.mk_s_term () in
      let asset = mkloc (loc asset) (PT.Eterm (st, asset)) in
      let asset = for_asset_collection_expr `Formula env (`Parsed asset) in

      match snd asset with
      | None ->
        M.ADAny

      | Some (decl, _) ->
        M.ADOp (unloc act, decl.as_name)
    end

  | _ ->
    Env.emit_error env (loc sa, InvalidActionDescription);
    M.ADAny

(* -------------------------------------------------------------------- *)
and for_security_action (env : env) (sa : PT.security_arg) : M.security_action =
  match unloc sa with
  | Sident id ->
    begin
      match unloc id with
      | "anyaction" -> Sany
      | _           ->
        let ad = Env.TAction.lookup env (unloc id) in

        if Option.is_none ad then
          Env.emit_error env (loc id, UnknownAction (unloc id));

        Sentry [id]
    end

  | Slist sas ->
    M.Sentry (List.flatten (List.map (
        fun x ->
          let a = for_security_action env x in
          match a with
          | Sentry ids -> ids
          | _ -> assert false) sas))

  | _ ->
    Env.emit_error env (loc sa, InvalidSecurityAction);
    Sentry []

(* -------------------------------------------------------------------- *)
and for_security_role (env : env) (sa : PT.security_arg) : M.security_role list =
  match unloc sa with
  | Sident id ->
    Option.get_as_list (for_role env id)

  | _ ->
    Env.emit_error env (loc sa, InvalidSecurityRole);
    []

(* -------------------------------------------------------------------- *)
and for_role (env : env) (name : PT.lident) =
  match Env.Var.lookup env (unloc name) with
  | None ->
    Env.emit_error env (loc name, UnknownLocalOrVariable (unloc name));
    None

  | Some nty ->
    if not (Type.compatible ~from_:nty.vr_type ~to_:M.vtrole) then
      (Env.emit_error env (loc name, NotARole (unloc name)); None)
    else Some name

(* -------------------------------------------------------------------- *)
let for_expr (env : env) ?(ety : M.type_ option) (tope : PT.expr) : M.pterm =
  for_xexpr `Expr env ?ety tope

(* -------------------------------------------------------------------- *)
let for_lbl_expr (env : env) ?ety (topf : PT.label_expr) : env * (M.lident option * M.pterm) =
  if check_and_emit_name_free env (fst (unloc topf)) then
    let env = Env.Label.push env (fst (unloc topf), `Plain) in
    env, (Some (fst (unloc topf)), for_expr ?ety env (snd (unloc topf)))
  else
    env, (None, for_expr env ?ety (snd (unloc topf)))

(* -------------------------------------------------------------------- *)
let for_lbls_expr (env : env) ?ety (topf : PT.label_exprs) : env * (M.lident option * M.pterm) list =
  List.fold_left_map (for_lbl_expr ?ety) env topf

(* -------------------------------------------------------------------- *)
let for_lbl_bexpr = for_lbl_expr ~ety:(M.Tbuiltin M.VTbool)

(* -------------------------------------------------------------------- *)
let for_lbls_bexpr = for_lbls_expr ~ety:(M.Tbuiltin M.VTbool)

(* -------------------------------------------------------------------- *)
let for_lbl_formula (env : env) (topf : PT.label_expr) : env * (M.lident option * M.pterm) =
  if check_and_emit_name_free env (fst (unloc topf)) then
    let env = Env.Label.push env (fst (unloc topf), `Plain) in
    env, (Some (fst (unloc topf)), for_formula env (snd (unloc topf)))
  else
    env, (None, for_formula env (snd (unloc topf)))

(* -------------------------------------------------------------------- *)
let for_xlbls_formula (env : env) (topf : PT.label_exprs) : env * (M.lident option * M.pterm) list =
  List.fold_left_map for_lbl_formula env topf

(* -------------------------------------------------------------------- *)
let for_lbls_formula (env : env) (topf : PT.label_exprs) : env * (M.lident option * M.pterm) list =
  List.fold_left_map for_lbl_formula env topf

(* -------------------------------------------------------------------- *)
let for_arg_decl (env : env) ((x, ty, _) : PT.lident_typ) =
  let ty = for_type env ty in
  let b  = check_and_emit_name_free env x in

  match b, ty with
  | true, Some ty ->
    (Env.Local.push env (x, ty), Some (x, ty))

  | _, _ ->
    (env, None)

(* -------------------------------------------------------------------- *)
let for_args_decl (env : env) (xs : PT.args) =
  List.fold_left_map for_arg_decl env xs

(* -------------------------------------------------------------------- *)
let for_lvalue (env : env) (e : PT.expr) : (M.lvalue * M.ptyp) option =
  match unloc e with
  | Eterm ({ before = false; label = None; }, x) -> begin
      match Env.lookup_entry env (unloc x) with
      | Some (`Local xty) ->
        Some (`Var x, xty)

      | Some (`Global vd) ->
        if vd.vr_kind <> `Variable then
          Env.emit_error env (loc e, ReadOnlyGlobal (unloc x));
        Some (`Var x, vd.vr_type)

      | _ ->
        Env.emit_error env (loc e, UnknownLocalOrVariable (unloc x));
        None
    end

  | Edot (pnm, x) -> begin
      let nm = for_xexpr `Expr env pnm in

      match Option.map Type.as_asset nm.M.type_ with
      | None ->
        None

      | Some None ->
        Env.emit_error env (loc pnm, AssetExpected (Option.get nm.M.type_));
        None

      | Some (Some asset) -> begin
          let asset = Env.Asset.get env (unloc asset) in

          match get_field (unloc x) asset with
          | None ->
            let err = UnknownField (unloc asset.as_name, unloc x) in
            Env.emit_error env (loc x, err); None

          | Some { fd_type = fty } ->
            Some (`Field (nm, x), fty)
        end
    end

  | _ ->
    Env.emit_error env (loc e, InvalidLValue); None

(* -------------------------------------------------------------------- *)
let rec for_instruction (env : env) (i : PT.expr) : env * M.instruction =
  let module E = struct exception Failure end in

  let bailout () = raise E.Failure in

  let mki ?label node : M.instruction =
    M.{ node; label; loc = loc i; } in

  let mkseq i1 i2 =
    let asblock = function M.{ node = Iseq is } -> is | _ as i -> [i] in
    match asblock i1 @ asblock i2 with
    | [i] -> i
    | is  -> mki (Iseq is) in

  try
    match unloc i with
    | Emethod (pthe, m, args) -> begin
        let the = for_xexpr `Expr env pthe in

        match the.M.type_ with
        | Some (M.Tcontract ctt) -> begin
            let ctt = Env.Contract.get env (unloc ctt) in
            match List.Exn.assoc_map unloc (unloc m) ctt.ct_entries with
            | None ->
              let exn = UnknowContractEntryPoint (unloc ctt.ct_name, unloc m) in
              Env.emit_error env (loc m, exn); bailout ()

            | Some entry ->
              let args = match args with
                | [ {pldesc = Etuple l; _} ] -> l
                | _ -> args
              in

              let na, ne = List.length args, List.length entry in
              if na <> ne then begin
                let loc = Location.mergeall (loc pthe :: List.map loc args) in
                Env.emit_error env (loc, InvalidNumberOfArguments (na, ne));
                bailout ()
              end;

              let args =
                List.map2
                  (fun arg (_arg_id, ety) -> for_xexpr `Expr env ~ety arg)
                  args entry in

              let args = List.map (fun x -> M.AExpr x) args in

              env, mki (M.Icall (Some the, M.Cid m, args))
          end

        | _ ->
          let infos = for_gen_method_call `Expr env (loc i) (`Typed the, m, args) in
          let the, _asset, method_, args, _ = Option.get_fdfl bailout infos in
          env, mki (M.Icall (Some the, M.Cconst method_.mth_name, args))
      end

    | Eseq (i1, i2) ->
      let env, i1 = for_instruction env i1 in
      let env, i2 = for_instruction env i2 in
      env, mkseq i1 i2

    | Eassign (op, plv, pe) -> begin
        let lv = for_lvalue env plv in
        let x  = Option.get_dfl
            (`Var (mkloc (loc plv) "<error>"))
            (Option.map fst lv) in
        let op = for_assignment_operator op in

        let e  =
          match lv with
          | None ->
            for_expr env pe

          | Some (_, fty) ->
            for_assign_expr `Expr env (loc plv) (op, fty) pe
        in

        let type_assigned = M.Tbuiltin (VTint) in (* TODO: replace by the var/field assigned type *)
        env, mki (M.Iassign (type_assigned, op, x, e))
      end

    | Etransfer (e, d, _c) ->
      let e   = for_expr env ~ety:M.vtcurrency e in
      let to_ = for_expr env ~ety:M.vtrole d in

      env, mki (Itransfer (e, to_))

    | Eif (c, bit, bif) ->
      let c        = for_expr env ~ety:M.vtbool c in
      let env, cit = for_instruction env bit in
      let cif      = Option.map (for_instruction env) bif in
      let env, cif = Option.get_dfl (env, mki (Iseq [])) cif in
      env, mki (M.Iif (c, cit, cif))

    | Eletin (x, ty, e1, e2, eo) ->
      if Option.is_some eo then
        Env.emit_error env (loc i, LetInElseInInstruction);
      let ty = Option.bind (for_type env) ty in
      let e  = for_expr env ?ety:ty e1 in
      let env, body =
        Env.inscope env (fun env ->
            let _ : bool = check_and_emit_name_free env x in
            let env =
              Option.fold (fun env ty ->
                  Env.Local.push env (x, ty)
                ) env e.M.type_
            in for_instruction env e2) in

      env, mki (M.Iletin (x, e, body))

    | Evar (x, ty, v) ->
      let ty = Option.bind (for_type env) ty in
      let v  = for_expr env ?ety:ty v in
      let env =
        let _ : bool = check_and_emit_name_free env x in
        if Option.is_some v.M.type_ then
          Env.Local.push env (x, Option.get v.M.type_)
        else env

      in

      env, mki (M.Ideclvar (x, v))

    | Efor (lbl, x, e, i) ->
      let e, asset = for_asset_collection_expr `Expr env (`Parsed e) in
      let asset = Option.map fst asset in
      let env, i = Env.inscope env (fun env ->
          let _ : bool = check_and_emit_name_free env x in
          let env, aname =
            if Option.is_some asset then
              let nm = (Option.get asset).as_name in
              let ty = M.Tasset nm in
              Env.Local.push env (x, ty), Some (unloc nm)
            else env, None in

          let env =
            match aname with
            | None ->
              env
            | Some aname ->
              Option.fold (fun env lbl ->
                  if (check_and_emit_name_free env lbl) then
                    Env.Label.push env (lbl, `Loop aname)
                  else env) env lbl
          in for_instruction env i) in

      env, mki (M.Ifor (x, e, i)) ?label:(Option.map unloc lbl)

    | Eiter (lbl, x, a, b, i) ->
      let zero_b = M.mk_sp (M.BVint Big_int.zero_big_int) ~type_:M.vtint in
      let zero : M.pterm = M.mk_sp (M.Plit zero_b) ~type_:M.vtint in
      let a = Option.map_dfl (fun x -> for_expr env ~ety:M.vtint x) zero a in
      let b = for_expr env ~ety:M.vtint b in
      let env, i = Env.inscope env (fun env ->
          let _ : bool = check_and_emit_name_free env x in
          let env = Env.Local.push env (x, M.vtint) in
          for_instruction env i) in
      env, mki (M.Iiter (x, a, b, i)) ?label:(Option.map unloc lbl)

    | Erequire e ->
      let e = for_formula env e in
      env, mki (M.Irequire (true, e))

    | Efailif e ->
      let e = for_formula env e in
      env, mki (M.Irequire (false, e))

    | Eassert lbl ->
      let env =
        if (check_and_emit_name_free env lbl) then
          Env.Label.push env (lbl, `Plain)
        else env in
      env, mki (Ilabel lbl)

    | Ematchwith (e, bs) -> begin
        match for_gen_matchwith `Expr env (loc i) e bs with
        | None -> bailout () | Some (decl, me, (wd, bsm), is) ->

          let env, is = List.fold_left_map for_instruction env is in

          let aout = List.pmap (fun (cname, _) ->
              let ctor = M.mk_sp (M.Mconst cname) in (* FIXME: loc ? *)
              let bse  =
                match Mstr.find (unloc cname) bsm, wd with
                | Some k, _ ->
                  Some (List.nth is k)
                | None, Some _ ->
                  None
                | None, None ->
                  Some (mki (Iseq []))
              in Option.map (fun bse -> (ctor, bse)) bse) decl.sd_ctors in

          let aout =
            Option.fold
              (fun aout extra -> aout @ [M.mk_sp M.Mwild, extra])
              aout (Option.map (List.nth is) wd) in

          env, mki (M.Imatchwith (me, aout))
      end

    | Elabel lbl ->
        let env =
          if   check_and_emit_name_free env lbl
          then Env.Label.push env (lbl, `Code)
          else env
        in env, mki (Iseq [])

    | _ ->
      Env.emit_error env (loc i, InvalidInstruction);
      bailout ()

  with E.Failure ->
    env, mki (Iseq [])

(* -------------------------------------------------------------------- *)
let for_effect (env : env) (effect : PT.expr) =
  Env.inscope env (fun env ->
    let env, i = for_instruction env effect in (env, (env, i)))

(* -------------------------------------------------------------------- *)
let for_specification_item
    (env, poenv : env * env) (v : PT.specification_item)
  : (env * env) * env ispecification
=
  match unloc v with
  | PT.Vpredicate (x, args, f) ->
    let env, (args, f) =
      Env.inscope env (fun env ->
          let env, args = for_args_decl env args in
          let args = List.pmap id args in
          let f = for_formula env f in
          (env, (args, f)))
    in (env, poenv), `Predicate (x, args, f)

  | PT.Vdefinition (x, ty, y, f) ->
    let env, (arg, f) =
      Env.inscope env (fun env ->
          let env, arg = for_arg_decl env (y, ty, None) in
          let f = for_formula env f in
          (env, (arg, f)))
    in ((env, poenv), `Definition (x, arg, f))

  | PT.Vvariable (x, ty, e) ->
    let ty = for_type env ty in
    let e  = Option.map (for_expr env ?ety:ty) e in
    let poenv =
      if not (check_and_emit_name_free env x) then poenv else
        Option.fold (fun poenv ty -> Env.Local.push poenv (x, ty)) poenv ty

    in ((env, poenv), `Variable (x, e))

  | PT.Vassert (x, f, invs, uses) -> begin
      let env0 =
        match Env.Label.lookup env (unloc x) with
        | None ->
          Env.emit_error env (loc x, UnknownLabel (unloc x));
          env
        | Some (env, _) ->
          env
      in

      let for_inv (lbl, linvs) =
        (lbl, List.map (for_formula env0) linvs) in

      let f    = for_formula env0 f in
      let invs = List.map for_inv invs in

      ((env, poenv), `Assert (x, f, invs, uses))
    end

  | PT.Veffect i ->
    (* FIXME: we are not properly tracking labels here *)
    let _, ((poenv, _) as i) = for_effect poenv i in
    ((env, poenv), `Effect i)

  | PT.Vpostcondition (x, f, invs, uses)
  | PT.Vcontractinvariant (x, f, invs, uses) ->
    let for_inv (lbl, linvs) =
      let env0 =
        match Env.Label.lookup env (unloc lbl) with
        | None ->
          Env.emit_error env (loc lbl, UnknownLabel (unloc lbl));
          env
        | Some (env, `Loop aname) ->
          let ty = M.Tasset (mkloc (loc lbl) aname) in
          let ty = M.Tcontainer (ty, M.Subset) in
          Env.Local.push env (mkloc coreloc "toiterate", ty)
        | Some (_, _) ->
          Env.emit_error env (loc lbl, NonLoopLabel (unloc lbl));
          env
      in (lbl, List.map (for_formula env0) linvs) in
    let f    = for_formula poenv f in
    let invs = List.map for_inv invs in
    ((env, poenv), `Postcondition (x, f, invs, uses))

(* -------------------------------------------------------------------- *)
let for_specification ((env, poenv) : env * env) (v : PT.specification) =
  let (env, _), items =
    List.fold_left_map for_specification_item (env, poenv) (fst (unloc v))
  in (env, items)

(* -------------------------------------------------------------------- *)
module SecurityPred = struct
  type _ mode =
    | ActionDesc : M.action_description mode
    | Role       : M.lident list        mode
    | Action     : M.security_action    mode

  let validate1 (type a) (env : env) (mode : a mode) (v : PT.security_arg) : a =
    match mode with
    | ActionDesc -> for_action_description env v
    | Role       -> for_security_role      env v
    | Action     -> for_security_action    env v

  type _ validator =
    | V0 : unit validator
    | VC : 'a mode * 'b validator -> ('a * 'b) validator

  let (^:) m v = VC (m, v)

  exception ArgCountError

  let rec vdlen : type a . a validator -> int =
    function V0 -> 0 | VC (_, vd) -> 1 + vdlen vd

  let rec validate
    : type a . env -> a validator * PT.security_arg list -> a
    = fun env -> function
      | V0, [] ->
        ()

      | VC (m, vd), v :: args ->
        let v    = validate1 env m    v     in
        let args = validate  env (vd, args) in
        (v, args)

      | _, _ ->
        raise ArgCountError

  type predc =
    | PredC : ('a -> M.security_node) * 'a validator -> predc

  let pclen (PredC (_, vd)) = vdlen vd

  let vd1 f m =
    PredC ((fun (x, ()) -> f x), m ^: V0)

  let vd2 f m1 m2 =
    PredC
      ((fun (x, (y, ())) -> f x y),
       m1 ^: m2 ^: V0)

  let vd3 f m1 m2 m3 =
    PredC
      ((fun (x, (y, (z, ()))) -> f x y z),
       m1 ^: m2 ^: m3 ^: V0)

  let validate_and_build env (PredC (f, vd)) args =
    f (validate env (vd, args))

  let preds = [
    "only_by_role",           vd2 (fun x y   -> M.SonlyByRole         (x, y)   ) ActionDesc Role;
    "only_in_action",         vd2 (fun x y   -> M.SonlyInAction       (x, y)   ) ActionDesc Action;
    "only_by_role_in_action", vd3 (fun x y z -> M.SonlyByRoleInAction (x, y, z)) ActionDesc Role Action;
    "not_by_role",            vd2 (fun x y   -> M.SnotByRole          (x, y)   ) ActionDesc Role;
    "not_in_action",          vd2 (fun x y   -> M.SnotInAction        (x, y)   ) ActionDesc Action;
    "not_by_role_in_action",  vd3 (fun x y z -> M.SnotByRoleInAction  (x, y, z)) ActionDesc Role Action;
    "transferred_by",         vd1 (fun x     -> M.StransferredBy      (x)      ) ActionDesc;
    "transferred_to",         vd1 (fun x     -> M.StransferredTo      (x)      ) ActionDesc;
    "no_storage_fail",        vd1 (fun x     -> M.SnoStorageFail      (x)      ) Action;
  ]

  let preds = Mid.of_list preds
end

(* -------------------------------------------------------------------- *)
let for_security_item (env : env) (v : PT.security_item) : (env * M.security_item) option =
  let module E = struct exception Bailout end in

  try
    let loc, (label, name, args) = Location.deloc v in

    (* FIXME: check and add label in env *)

    let sp =
      match Mid.find_opt (unloc name) SecurityPred.preds with
      | None ->
        Env.emit_error env (L.loc name, NoSuchSecurityPredicate (unloc name));
        raise E.Bailout
      | Some method_ -> method_
    in

    let ne = SecurityPred.pclen sp in
    let ng = List.length args in

    if ne <> ng then begin
      Env.emit_error env (loc, InvalidNumberOfArguments (ne, ng));
      raise E.Bailout
    end;

    let security_node : M.security_node =
      SecurityPred.validate_and_build env sp args
    in

    let security_item : M.security_item =
      M.{ loc; label; predicate = M.{ loc; s_node = security_node; }; }
    in

    Some (env, security_item)

  with E.Bailout -> None

(* -------------------------------------------------------------------- *)
let for_security (env : env) (v : PT.security) : env * M.security =
  let env, items = List.fold_left (fun (env, items) x ->
      match for_security_item env x with
      | Some (e, v) -> (e, v::items)
      | None -> (env, items)
    ) (env, []) (fst (unloc v)) in
  env, M.{ items = List.rev items; loc = loc v; }

(* -------------------------------------------------------------------- *)
let for_named_state ?enum (env : env) (x : PT.lident) =
  match Env.State.byctor env (unloc x) with
  | None ->
    Env.emit_error env (loc x, UnknownState (unloc x));
    mkloc (loc x) "<error>"

  | Some state ->
    let sname = unloc state.sd_name in

    if Option.get_dfl ("$" ^ statename) enum <> sname then begin
      Env.emit_error env (loc x, ForeignState (enum, Some sname));
      mkloc (loc x) "<error>"
    end else
      x

(* -------------------------------------------------------------------- *)
let for_state ?enum (env : env) (st : PT.expr) : M.lident =
  match unloc st with
  | Eterm ({ before = false; label = None; }, x) ->
    for_named_state ?enum env x

  | _ ->
    Env.emit_error env (loc st, InvalidStateExpression);
    mkloc (loc st) "<error>"

(* -------------------------------------------------------------------- *)
let for_function (env : env) (fdecl : PT.s_function loced) =
  let fdecl = unloc fdecl in

  Env.inscope env (fun env ->
      let env, args = for_args_decl env fdecl.args in
      let rty       = Option.bind (for_type env) fdecl.ret_t in
      let body      = for_expr env ?ety:rty fdecl.body in
      let env, spec =
        Option.foldmap (fun env -> for_specification (env, env)) env fdecl.spec in

      if Option.is_some rty && not (List.exists Option.is_none args) then
        if check_and_emit_name_free env fdecl.name then
          let body =
            M.{ node    = M.Ireturn body;
                loc     = loc fdecl.body;
                label   = None; } in

          (env, Some {
              fs_name  = fdecl.name;
              fs_args  = List.pmap id args;
              fs_retty = Option.get rty;
              fs_body  = body;
              fs_spec  = spec; })
        else (env, None)
      else (env, None))

(* -------------------------------------------------------------------- *)
let rec for_callby (env : env) (cb : PT.expr) =
  match unloc cb with
  | Eterm ({ before = false; label = None; }, name)
    when String.equal (unloc name) "any"
    -> [name]

  | Eterm ({ before = false; label = None; }, name) ->
    Option.get_as_list (for_role env name)

  | Eapp (Foperator { pldesc = Logical Or }, [e1; e2]) ->
    (for_callby env e1) @ (for_callby env e2)

  | _ ->
    Env.emit_error env (loc cb, InvalidCallByExpression);
    []

(* -------------------------------------------------------------------- *)
let for_action_properties (env, poenv : env * env) (act : PT.action_properties) =
  let calledby  = Option.map (fun (x, _) -> for_callby env x) act.calledby in
  let env, req  = Option.foldmap for_lbls_bexpr env (Option.fst act.require) in
  let env, fai  = Option.foldmap for_lbls_bexpr env (Option.fst act.failif) in
  let env, spec = Option.foldmap
                    (fun env x -> for_specification (env, poenv) x) env act.spec_fun in
  let env, funs = List.fold_left_map for_function env act.functions in

  (env, (calledby, req, fai, spec, funs))

(* -------------------------------------------------------------------- *)
let for_transition ?enum (env : env) (state, when_, effect) =
  let tx_state  = for_named_state ?enum env state in
  let tx_when   =
    Option.map (for_formula env) (Option.fst when_) in
  let env, tx_effect = snd_map (Option.map snd)
    (Option.foldmap for_effect env (Option.fst effect)) in

  env, { tx_state; tx_when; tx_effect; }

(* -------------------------------------------------------------------- *)
type enum_core = ((PT.lident * PT.enum_option list) list)

let for_core_enum_decl (env : env) (enum : enum_core loced) =
  (* FIXME: check that ctor names are available *)

  let ctors = unloc enum in

  match ctors with
  | [] ->
    Env.emit_error env (loc enum, EmptyEnumDecl);
    env, None

  | _ ->
    Option.iter
      (fun (_, x) ->
         Env.emit_error env (loc x, DuplicatedCtorName (unloc x)))
      (List.find_dup unloc (List.map fst ctors));

    let ctors = Mid.collect (unloc : M.lident -> ident) ctors in

    let for1 (cname, options) =
      let init, inv =
        List.fold_left (fun (init, inv) option ->
            match option with
            | PT.EOinitial ->
              (init+1, inv)
            | PT.EOspecification spec ->
              (init, List.rev_append spec inv)
          ) (0, []) options in

      if init > 1 then
        Env.emit_error env (loc cname, DuplicatedInitMarkForCtor);
      (init <> 0, List.rev inv) in

    let for1 env ((cname : PT.lident), options) =
      let init, inv = for1 (cname, options) in
      let env , inv = for_lbls_formula env inv in

      (env, (cname, init, inv)) in

    let env, ctors = List.fold_left_map for1 env ctors in

    let ictor =
      let ictors =
        List.pmap
          (fun (x, b, _) -> if b then Some x else None)
          ctors in

      match ictors with
      | [] ->
        proj3_1 (List.hd ctors)
      | init :: ictors ->
        if not (List.is_empty ictors) then
          Env.emit_error env (loc enum, MultipleInitialMarker);
        init in

    env, Some (unloc ictor, List.map (fun (x, _, inv) -> (x, inv)) ctors)

(* -------------------------------------------------------------------- *)
let for_enum_decl (env : env) (decl : (PT.lident * PT.enum_decl) loced) =
  let (name, (ctors, _)) = unloc decl in
  let env, ctors = for_core_enum_decl env (mkloc (loc decl) ctors) in
  let env, decl =
    Option.foldbind (fun env (sd_init, sd_ctors) ->
        let enum = { sd_name = name; sd_ctors; sd_init; sd_state = false; } in
        if   check_and_emit_name_free env name
        then Env.State.push env enum, None
        else env, Some enum) env ctors in

  env, decl

(* -------------------------------------------------------------------- *)
let for_enums_decl (env : env) (decls : (PT.lident * PT.enum_decl) loced list) =
  List.fold_left_map for_enum_decl env decls

(* -------------------------------------------------------------------- *)
let for_var_decl (env : env) (decl : PT.variable_decl loced) =
  let (x, ty, pe, tgt, ctt, invs, _) = unloc decl in

  let ty   = for_type env ty in
  let e    = Option.map (for_expr env ?ety:ty) pe in
  let dty  =
    if   Option.is_some ty
    then ty
    else Option.bind (fun e -> e.M.type_) e in
  let ctt  = match ctt with
    | VKconstant -> `Constant
    | VKvariable -> `Variable in

  if Option.is_none pe then
    Env.emit_error env (loc decl, UninitializedVar);

  let tgt =
    let for1 = function
      | PT.VOfrom x -> (`From, for_role env x)
      | PT.VOto   x -> (`To  , for_role env x)
    in List.map for1 (Option.get_dfl [] tgt) in

  let (tf, tt) =
    let for1 (f, t) =
      function (`From, x) -> (x :: f, t) | (`To, x) -> (f, x :: t)
    in List.fold_left for1 ([], []) tgt in

  let tgtc = (List.length tf, List.length tt) in

  if tgtc <> (0, 0) && (fst tgtc > 1 || snd tgtc > 1) then
    Env.emit_error env (loc decl, MultipleFromToInVarDecl);

  match dty with
  | None ->
    (env, None)

  | Some dty ->
    if tgtc <> (0, 0) then begin
      if not (Type.is_currency dty) then
        Env.emit_error env (loc decl, InvalidTypeForVarWithFromTo);
    end;

    let vtgt_tf = match tf with [Some tf] -> Some tf | _ -> None in
    let vtgt_tt = match tt with [Some tt] -> Some tt | _ -> None in

    let decl = {
      vr_name = x;
      vr_type = dty;
      vr_kind = ctt;
      vr_core = None;
      vr_invs = [];
      vr_tgt  = (vtgt_tf, vtgt_tt);
      vr_def  = Option.map (fun e -> (e, `Std)) e; } in

    (* FIXME: check in which env. checking invariants *)
    let env, invs =
      Env.inscope env (fun env ->
        let env = Env.Local.push env (x, dty) in
        for_lbls_formula env invs
      ) in
    let invs =
      let for1 (label, term) =
        M.{ label; term; loc = term.M.loc }
      in List.map for1 invs in
    let decl = { decl with vr_invs = invs; } in

    if   (check_and_emit_name_free env x)
    then (Env.Var.push env decl, Some decl)
    else (env, None)

(* -------------------------------------------------------------------- *)
let for_vars_decl (env : env) (decls : PT.variable_decl loced list) =
  List.fold_left_map for_var_decl env decls

(* -------------------------------------------------------------------- *)
let for_fun_decl (env : env) (fdecl : PT.s_function loced) =
  let env, decl = for_function env fdecl in

  (Option.fold (fun env decl -> Env.Function.push env decl) env decl, decl)

(* -------------------------------------------------------------------- *)
let for_funs_decl (env : env) (decls : PT.s_function loced list) =
  List.fold_left_map for_fun_decl env decls

(* -------------------------------------------------------------------- *)
let for_asset_decl ?(force = false) (env : env) (decl : PT.asset_decl loced) =
  let (x, fields, shadow_fields, opts, postopts, _ (* FIXME *), _) = unloc decl in

  let for_field field =
    let PT.Ffield (f, fty, init, _) = unloc field in
    let fty  = for_type env fty in
    let init = Option.map (for_expr env ?ety:fty) init in

    if   check_and_emit_name_free env f
    then Some (mkloc (loc f) (unloc f, fty, init))
    else None in

  let fields = List.pmap for_field (fields @ shadow_fields) in

  Option.iter
    (fun (_, { plloc = lc; pldesc = (name, _, _) }) ->
       Env.emit_error env (lc, DuplicatedFieldInAssetDecl name))
    (List.find_dup (fun x -> proj3_1 (unloc x)) fields);

  let get_field name =
    List.Exn.find
      (fun { pldesc = (x, _, _) } -> x = name)
      fields
  in

  (* FIXME: check for duplicated type name? *)

  let pk, sortk =
    let dokey key =
      if Option.is_none (get_field (unloc key)) then begin
        Env.emit_error env (loc key, UnknownFieldName (unloc key));
        None
      end else Some key in

    let do1 (pk, sortk) = function
      | PT.AOidentifiedby newpk ->
        if Option.is_some pk then
          Env.emit_error env (loc newpk, DuplicatedPKey);
        let newpk = dokey newpk in
        ((if Option.is_some pk then pk else newpk), sortk)

      | PT.AOsortedby newsortk ->
        let newsortk = dokey newsortk in
        (pk, Option.fold (fun sortk newsortk -> newsortk :: sortk) sortk newsortk)

    in List.fold_left do1 (None, []) opts in

  let sortk = List.rev sortk in

  let env, invs =
    let for1 env = function
      | PT.APOconstraints invs ->
        Env.inscope env (fun env ->
            let env =
              List.fold_left (fun env { pldesc = (f, fty, _); plloc = loc; } ->
                  Option.fold (fun env fty ->
                      Env.Local.push env (mkloc loc f, fty)) env fty)
                env fields
            in for_xlbls_formula env invs)

      | _ ->
        env, []

    in List.fold_left_map for1 env postopts in

  let state =
    let for1 = function
      | PT.APOstates x ->
          let aout =
            match Env.State.lookup env (unloc x) with
            | None ->
              Env.emit_error env (loc x, UnknownEnum (unloc x));
              None
            | Some _ ->
              Some x
          in Some aout

      | _ ->
        None in

    match List.pmap for1 postopts with
    | _ :: _ :: _ ->
      Env.emit_error env (loc decl, MultipleAssetStateDeclaration);
      None
    | [] | [None] ->
      None
    | [Some st] ->
      Some st
  in

  if not force && not (check_and_emit_name_free env x) then begin
    (env, None)
  end else
    let module E = struct exception Bailout end in

    try
      if List.is_empty fields then begin
        Env.emit_error env (loc decl, AssetWithoutFields);
        raise E.Bailout
      end;

      let get_field_type { plloc = loc; pldesc = (x, ty, e) } =
        let ty =
          if   Option.is_some ty
          then ty
          else Option.bind (fun e -> e.M.type_) e
        in { fd_name = mkloc loc x; fd_type = Option.get ty;
             fd_dfl = e; fd_ghost = false; }
      in

      let decl = {
        as_name   = x;
        as_fields = List.map get_field_type fields;
        as_pk     = Option.get_fdfl
            (fun () -> L.lmap proj3_1 (List.hd fields))
            pk;
        as_sortk  = sortk;
        as_invs   = List.flatten invs;
        as_state  = state;
      } in (Env.Asset.push env decl, Some decl)
    with E.Bailout -> (env, None)

(* -------------------------------------------------------------------- *)
let for_assets_decl (env as env0 : env) (decls : PT.asset_decl loced list) =
  let b, env = List.fold_left (fun (b, env) decl ->
      let (name, _, _, _, _, _, _) = unloc decl in
      let b = b && check_and_emit_name_free env name in
      let d = { as_name   = name;
                as_fields = [];
                as_pk     = mkloc Location.dummy "";
                as_sortk  = [];
                as_invs   = [];
                as_state  = None; } in
      (b, Env.Asset.push env d)) (true, env) decls in

  if b then
    List.fold_left_map (for_asset_decl ~force:true) env decls
  else (env0, List.map (fun _ -> None)  decls)

(* -------------------------------------------------------------------- *)
let for_contract_decl (env : env) (decl : PT.contract_decl loced) =
  let name, sigs, _ = unloc decl in
  let entries =
    List.pmap (fun (PT.Ssignature (ename, psig)) ->
        let for1 (arg_id, pty) =
          let ty = for_type env pty in
          Option.bind (fun ty ->
              if not (Type.is_primitive ty) then begin
                Env.emit_error env (loc pty, NotAPrimitiveType);
                None
              end else Some (arg_id, ty)) ty in

        let sig_ = List.map for1 psig in

        if List.length sig_ = List.length psig then
          Some (ename, List.pmap id sig_)
        else None
      ) sigs in

  let cdecl = { ct_name = name; ct_entries = entries; } in

  if check_and_emit_name_free env name then
    (Env.Contract.push env cdecl, Some cdecl)
  else (env, None)

(* -------------------------------------------------------------------- *)
let for_contracts_decl (env : env) (decls : PT.contract_decl loced list) =
  List.fold_left_map for_contract_decl env decls

(* -------------------------------------------------------------------- *)
let for_acttx_decl (env : env) (decl : acttx loced) =
  match unloc decl with
  | `Action (x, args, pt, i_exts, _exts) -> begin
      let env, decl =
        Env.inscope env (fun env ->
            let env, args = for_args_decl env args in
            let env, poeffect =
              Option.foldmap for_effect env (Option.fst i_exts) in
            let effect = Option.map snd poeffect in
            let poenv  = Option.get_dfl env (Option.map fst poeffect) in
            let env, (callby, reqs, fais, spec, funs) =
              for_action_properties (env, poenv) pt in

            let decl =
              { ad_name   = x;
                ad_args   = List.pmap (fun x -> x) args;
                ad_callby = Option.get_dfl [] callby;
                ad_effect = Option.map (fun x -> `Raw x) effect;
                ad_funs   = funs;
                ad_reqs   = Option.get_dfl [] reqs;
                ad_fais   = Option.get_dfl [] fais;
                ad_spec   = Option.get_dfl [] spec;
                ad_actfs  = pt.accept_transfer; } in

            (env, decl))

      in (Env.TAction.push env decl, decl)
    end

  | `Transition (x, args, tgt, from_, actions, tx, _exts) ->
    let env, decl =
      Env.inscope env (fun env ->
          let env, args = for_args_decl env args in
          let env, enum =
            Option.foldbind (fun env (vtg, ttg) ->
                Option.foldbind (fun env aname ->
                    let asset = Env.Asset.get env (unloc aname) in
                    let env =
                      if check_and_emit_name_free env vtg then
                        let field = Env.Asset.byfield env (unloc asset.as_pk) in
                        let field = Option.get field in
                        Env.Local.push env (vtg, (snd field).fd_type)
                      else env in
                    (env, Option.map unloc asset.as_state))
                  env (for_asset_keyof_type env ttg))
              env tgt in

          let from_ = for_state ?enum env from_ in
          let env, (callby, reqs, fais, spec, funs) =
            for_action_properties (env, env) actions in
          let env, tx =
            List.fold_left_map (for_transition ?enum) env tx in

          let decl =
            { ad_name   = x;
              ad_args   = List.pmap (fun x -> x) args;
              ad_callby = Option.get_dfl [] callby;
              ad_effect = Some (`Tx (from_, tx));
              ad_funs   = funs;
              ad_reqs   = Option.get_dfl [] reqs;
              ad_fais   = Option.get_dfl [] fais;
              ad_spec   = Option.get_dfl [] spec;
              ad_actfs  = actions.accept_transfer; }

          in (env, decl))

    in (Env.TAction.push env decl, decl)

(* -------------------------------------------------------------------- *)
let for_acttxs_decl (env : env) (decls : acttx loced list) =
  List.fold_left_map for_acttx_decl env decls

(* -------------------------------------------------------------------- *)
let for_specs_decl (env as poenv : env) (decls : PT.specification loced list) =
  List.fold_left_map
    (fun env { pldesc = x } -> for_specification (env, poenv) x)
    env decls

(* -------------------------------------------------------------------- *)
let for_secs_decl (env : env) (decls : PT.security loced list) =
  List.fold_left_map
    (fun env { pldesc = x } -> for_security env x)
    env decls

(* -------------------------------------------------------------------- *)
let group_declarations (decls : (PT.declaration list)) =
  let empty = {
    gr_archetypes = [];
    gr_states     = [];
    gr_enums      = [];
    gr_assets     = [];
    gr_vars       = [];
    gr_funs       = [];
    gr_acttxs     = [];
    gr_specs      = [];
    gr_secs       = [];
    gr_externals  = [];
  } in

  let for1 { plloc = loc; pldesc = decl } (g : groups) =
    let mk x = Location.mkloc loc x in

    match decl with
    | PT.Darchetype (x, exts) ->
      { g with gr_archetypes = mk (x, exts) :: g.gr_archetypes }

    | PT.Dvariable infos ->
      { g with gr_vars = mk infos :: g.gr_vars }

    | PT.Denum (PT.EKstate, infos) ->
      { g with gr_states = mk infos :: g.gr_states }

    | PT.Denum (PT.EKenum x, infos) ->
      { g with gr_enums = mk (x, infos) :: g.gr_enums }

    | PT.Dasset infos ->
      { g with gr_assets = mk infos :: g.gr_assets }

    | PT.Daction infos ->
      { g with gr_acttxs = mk (`Action infos) :: g.gr_acttxs }

    | PT.Dtransition infos ->
      { g with gr_acttxs = mk (`Transition infos) :: g.gr_acttxs }

    | PT.Dfunction infos ->
      { g with gr_funs = mk infos :: g.gr_funs }

    | PT.Dspecification infos ->
      { g with gr_specs = mk infos :: g.gr_specs }

    | PT.Dsecurity infos ->
      { g with gr_secs = mk infos :: g.gr_secs }

    | PT.Dcontract infos ->
      { g with gr_externals = mk infos :: g.gr_externals }

    | Dnamespace _  -> assert false
    | Dextension _  -> assert false
    | Dinvalid      -> assert false

  in List.fold_right for1 decls empty

(* -------------------------------------------------------------------- *)
type decls = {
  state     : statedecl option;
  contracts : contractdecl option list;
  variables : vardecl option list;
  enums     : statedecl option list;
  assets    : assetdecl option list;
  functions : env fundecl option list;
  acttxs    : env tactiondecl list;
  specs     : env ispecification list list;
  secspecs  : M.security list;
}

let for_grouped_declarations (env : env) (toploc, g) =
  if not (List.is_empty g.gr_archetypes) then
    Env.emit_error env (toploc, InvalidArcheTypeDecl);

  if List.length g.gr_states > 1 then
    Env.emit_error env (toploc, MultipleStateDeclaration);

  let state, env =
    let for1 { plloc = loc; pldesc = state } =
      match for_core_enum_decl env (mkloc loc (fst state)) with
      | env, Some state -> Some (env, loc, state)
      | _  , None       -> None in

    match List.pmap for1 g.gr_states with
    | (env, loc, (init, ctors)) :: _ ->
      let decl = { sd_name  = mkloc loc ("$" ^ statename);
                   sd_state = true;
                   sd_ctors = ctors;
                   sd_init  = init; } in
      let vdecl = { vr_name = (mkloc loc statename);
                    vr_type = M.Tenum (mkloc loc ("$" ^ statename));
                    vr_kind = `Constant;
                    vr_invs = [];
                    vr_def  = None;
                    vr_tgt  = (None, None);
                    vr_core = Some Cstate; } in
      let env = Env.State.push env decl in
      let env = Env.Var.push env vdecl in
      (Some decl, env)
    | _ ->
      (None, env) in

  let env, contracts = for_contracts_decl env g.gr_externals in
  let env, enums     = for_enums_decl     env g.gr_enums     in
  let env, variables = for_vars_decl      env g.gr_vars      in
  let env, assets    = for_assets_decl    env g.gr_assets    in
  let env, functions = for_funs_decl      env g.gr_funs      in
  let env, acttxs    = for_acttxs_decl    env g.gr_acttxs    in
  let env, specs     = for_specs_decl     env g.gr_specs     in
  let env, secspecs  = for_secs_decl      env g.gr_secs      in

  let output =
    { state    ; contracts; variables; enums   ; assets;
      functions; acttxs   ; specs    ; secspecs;       }

  in (env, output)

(* -------------------------------------------------------------------- *)
let enums_of_statedecl (enums : statedecl list) : M.enum list =
  let for1 tg =
    let for_ctor1 ((id, invs) : ctordecl) =
      let invs = List.map (fun (label, inv) -> M.mk_label_term ?label inv) invs in

      M.{ name       = id;
          initial    = String.equal (unloc id) tg.sd_init;
          invariants = invs;
          loc        = Location.dummy; } in

    let items = List.map for_ctor1 tg.sd_ctors in
    let kind  =
      if tg.sd_state then M.EKstate else M.EKenum tg.sd_name in

    M.{ kind; items; loc = Location.dummy; }

  in List.map for1 enums

(* -------------------------------------------------------------------- *)
let assets_of_adecls adecls =
  let for1 (decl : assetdecl) =
    let for_field fd =
      M.{ name    = fd.fd_name;
          typ     = Some fd.fd_type;
          default = fd.fd_dfl;
          shadow  = fd.fd_ghost;
          loc     = loc fd.fd_name; } in

    let spec (l, f) =
      M.{ label = l; term = f; loc = f.loc } in

    M.{ name   = decl.as_name;
        fields = List.map for_field decl.as_fields;
        key    = Some decl.as_pk;
        sort   = decl.as_sortk;
        state  = decl.as_state;
        init   = None;           (* FIXME *)
        specs  = List.map spec decl.as_invs;
        loc    = loc decl.as_name; }

  in List.map for1 (List.pmap (fun x -> x) adecls)

(* -------------------------------------------------------------------- *)
let variables_of_vdecls fdecls =
  let mktgt x =
    M.mk_sp
      ~loc:(loc x) ~type_:(M.Tbuiltin (M.VTrole))
      (M.Qident x) in (* FIXME: type? *)

  let for1 (decl : vardecl) =
    M.{ decl =
          M.{ name    = decl.vr_name;
              typ     = Some decl.vr_type;
              default = Option.fst decl.vr_def;
              shadow  = false;
              loc     = loc decl.vr_name; };
        constant = decl.vr_kind = `Constant;
        from     = Option.map mktgt (fst decl.vr_tgt);
        to_      = Option.map mktgt (snd decl.vr_tgt);
        invs     = decl.vr_invs;
        loc      = loc decl.vr_name; }

  in List.map for1 (List.pmap (fun x -> x) fdecls)

(* -------------------------------------------------------------------- *)
let contracts_of_cdecls (decls : contractdecl option list) =
  let for1 (decl : contractdecl) =
    let for_sig ((name, args) : M.lident * (M.lident * M.ptyp) list) =
      M.{ name; args; loc = loc name; } in

    M.{ name       = decl.ct_name;
        signatures = List.map for_sig decl.ct_entries;
        loc        = loc decl.ct_name;
        init       = None; }

  in List.map for1 (List.pmap id decls)

(* -------------------------------------------------------------------- *)
let specifications_of_ispecifications =
  let env0 : M.lident M.specification = M.{
      predicates  = [];
      definitions = [];
      lemmas      = [];
      theorems    = [];
      variables   = [];
      invariants  = [];
      effect      = None;
      specs       = [];
      asserts     = [];
      loc         = L.dummy;      (* FIXME *) } in

  let do1 (env : M.lident M.specification) (ispec : env ispecification) =
    match ispec with
    | `Postcondition (x, e, invs, uses) ->
      let spec =
        let for_inv (lbl, inv) =
          M.{ label = lbl; formulas = inv }
        in
        M.{ name       = x;
            formula    = e;
            invariants = List.map for_inv invs;
            uses       = uses; }
      in { env with M.specs = env.specs @ [spec] }

    | `Assert (x, form, invs, uses) ->
      let asst =
        let for_inv (lbl, inv) =
          M.{ label = lbl; formulas = inv }
        in
        M.{ name       = x;
            label      = x;
            formula    = form;
            invariants = List.map for_inv invs;
            uses       = uses; }
      in { env with M.asserts = env.asserts @ [asst] }

    | `Variable (x, e) ->
      let var =
        M.mk_variable ~loc:(loc x)
          (M.mk_decl
             ~loc:(loc x) ?default:e
             ?typ:(Option.bind (fun e -> e.M.type_) e)
             x)
      in { env with M.variables = env.variables @ [var] }

    | `Effect (_, i) ->
      assert (Option.is_none env.M.effect);
      { env with M.effect = Some i; }

    | _ ->
      assert false

  in fun ispecs -> List.fold_left do1 env0 ispecs

(* -------------------------------------------------------------------- *)
let functions_of_fdecls fdecls =
  let for1 (decl : env fundecl) =
    let args = List.map (fun (x, ty) -> M.{
        name = x; typ = Some ty; default = None; shadow  = false; loc = loc x;
      }) decl.fs_args in

    let specs = Option.map specifications_of_ispecifications decl.fs_spec in

    M.{ name          = decl.fs_name;
        args          = args;
        body          = decl.fs_body;
        specification = specs;
        return        = decl.fs_retty;
        loc           = loc decl.fs_name; }

  in List.map for1 (List.pmap (fun x -> x) fdecls)

(* -------------------------------------------------------------------- *)
let transactions_of_tdecls tdecls =
  let for_calledby cb : M.rexpr option =
    match cb with [] -> None | c :: cb ->

      let for1 = fun x ->
        let node =
          match unloc x with
          | "any" -> M.Rany
          | _ -> M.Rqualid (M.mk_sp ~loc:(loc x) (M.Qident x))
        in M.mk_sp ~loc:(loc x) node in

      Some (List.fold_left (fun acc c' ->
          M.mk_sp (M.Ror (acc, for1 c')))
          (for1 c) cb)
  in

  let for1 tdecl =
    let mkl (x, c) =  M.{ label = x; term = c; loc = L.dummy; } in

    let transition =
      match tdecl.ad_effect with
      | Some (`Tx (from_, x)) ->
        let from_ = M.mk_sp ~loc:(loc from_) (M.Sref from_) in

        Some (M.{ from = from_; on = None; trs =
                                             List.map
                                               (fun tx ->(tx.tx_state, tx.tx_when, tx.tx_effect)) x })

      | _ -> None in

    let effect =
      match tdecl.ad_effect with
      | Some (`Raw x) -> Some x | _ -> None in

    M.{ name = tdecl.ad_name;
        args =
          List.map (fun (x, xty) ->
              M.{ name = x; typ = Some xty; default = None; shadow  = false; loc = loc x; })
            tdecl.ad_args;
        calledby        = for_calledby tdecl.ad_callby;
        accept_transfer = tdecl.ad_actfs;
        require         = Some (List.map mkl tdecl.ad_reqs);
        failif          = Some (List.map mkl tdecl.ad_fais);
        transition      = transition;
        specification   = Some (specifications_of_ispecifications tdecl.ad_spec);
        functions       = functions_of_fdecls tdecl.ad_funs;
        effect          = effect;
        loc             = loc tdecl.ad_name; }

  in List.map for1 tdecls

(* -------------------------------------------------------------------- *)
let for_declarations (env : env) (decls : (PT.declaration list) loced) : M.model =
  let toploc = loc decls in

  match unloc decls with
  | { pldesc = Darchetype (x, _exts) } :: decls ->
    let groups = group_declarations decls in
    let _env, decls = for_grouped_declarations env (toploc, groups) in

    M.mk_model
      ~enums:(enums_of_statedecl (List.pmap id (decls.state :: decls.enums)))
      ~assets:(assets_of_adecls decls.assets)
      ~variables:(variables_of_vdecls decls.variables)
      ~transactions:(transactions_of_tdecls decls.acttxs)
      ~functions:(functions_of_fdecls decls.functions)
      ~specifications:(List.map specifications_of_ispecifications decls.specs)
      ~securities:(decls.secspecs)
      ~contracts:(contracts_of_cdecls decls.contracts)
      x

  | _ ->
    Env.emit_error env (loc decls, InvalidArcheTypeDecl);
    { (M.mk_model (mkloc (loc decls) "<unknown>")) with loc = loc decls }

(* -------------------------------------------------------------------- *)
let typing (env : env) (cmd : PT.archetype) =
  match unloc cmd with
  | Marchetype decls ->
    for_declarations env (mkloc (loc cmd) decls)

  | Mextension _ ->
    assert false
