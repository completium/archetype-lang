/* parser */
%{
  open ParseTree
  open Location

  let emit_error loc =
    let str : string = "syntax error" in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

  let emit_error_msg loc msg =
    let str : string = "syntax error: " ^ msg in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

  let dummy_entry_properties = mk_entry_properties ()

  let rec split_seq e =
    match unloc e with
    | Eseq (a, b) -> (split_seq a) @ (split_seq b)
    | _ -> [e]

%}

%token ACCEPT_TRANSFER
%token ADDED
%token ADDRESS_TO_CONTRACT
%token AGGREGATE
%token AMPEQUAL
%token AND
%token ANY
%token ARCHETYPE
%token ASSERT
%token AS
%token ASSET
%token ASSET_CONTAINER
%token ASSET_KEY
%token ASSET_VALUE
%token ASSET_VIEW
%token AT
%token BEFORE
%token BEGIN
%token BIG_MAP
%token BUT
%token BY
%token CALL
%token CALLED
%token CALL_VIEW
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COMMA
%token CONST
%token CONSTANT
%token CONTRACT
%token DEFINITION
%token DIV
%token DIVEQUAL
%token DO
%token DO_FAIL_IF
%token DONE
%token DO_REQUIRE
%token DOT
%token EFFECT
%token ELSE
%token EMIT
%token END
%token ENTRY
%token GET_ENTRYPOINT
%token ENUM
%token EOF
%token EQUAL
%token EQUIV
%token EVENT
%token EXISTS
%token EXTENSION
%token FAIL
%token FAIL_IF
%token FAILSOME
%token FAILS
%token FALSE
%token FOLD
%token FOR
%token FORALL
%token FROM
%token FUNCTION
%token GETTER
%token GREATER
%token GREATEREQUAL
%token IDENTIFIED
%token IF
%token IMPLY
%token IMPORT
%token IN
%token INITIAL
%token INITIALIZED
%token INVARIANT
%token IS
%token ITER
%token ITERABLE_BIG_MAP
%token LABEL
%token LAMBDA
%token LBRACE
%token LBRACKET
%token LBRACKETPERCENT
%token LEFT
%token LESS
%token LESS_EQUAL_GREATER
%token LESS_LESS_PIPE
%token LESSEQUAL
%token LET
%token LIST
%token LPAREN
%token MAKE_ASSET
%token MAKE_BIG_MAP
%token MAKE_EVENT
%token MAKE_LIST
%token MAKE_MAP
%token MAKE_SET
%token MAP
%token MATCH
%token MINUS
%token MINUSEQUAL
%token MULT
%token MULTEQUAL
%token NAMESPACE
%token NEQUAL
%token NO_TRANSFER
%token NONE
%token NOT
%token OFFCHAIN
%token ON
%token ONCHAIN
%token OPTION
%token OR
%token OTHERWISE
%token PARTITION
%token PERCENT
%token PERCENTRBRACKET
%token PIPE
%token PIPE_GREATER_GREATER
%token PIPEEQUAL
%token PLUS
%token PLUSEQUAL
%token POSTCONDITION
%token PREDICATE
%token QUESTION
%token QUESTIONCOLONEQUAL
%token QUESTIONDOT
%token QUESTIONEQUAL
%token QUESTIONIS
%token RBRACE
%token RBRACKET
%token RECORD
%token REQUIRE_ENTRYPOINT
%token REMOVED
%token REQUIRE
%token RETURN
%token RIGHT
%token RPAREN
%token SAPLING_STATE
%token SAPLING_TRANSACTION
%token SECURITY
%token SELF
%token SEMI_COLON
%token SET
%token SHADOW
%token SLASH
%token SLASHPERCENT
%token SOME
%token SORTED
%token SOURCED
%token SPECIFICATION
%token STATE_IS
%token STATES
%token THEN
%token TICKET
%token TO
%token TRANSFER
%token TRANSITION
%token TRUE
%token TYPE
%token UNDERSCORE
%token UNIT
%token UNMOVED
%token UNPACK
%token USE
%token VAR
%token VARIABLE
%token VIEW
%token WHEN
%token WHILE
%token WITH
%token WITH_METADATA
%token XOR

%token INVALID_EXPR
%token INVALID_DECL
%token INVALID_EFFECT

%token <string> IDENT
%token <string> PIDENT
%token <string> STRING
%token <string> STRING_EXT
%token <Big_int.big_int> NUMBERINT
%token <Big_int.big_int> NUMBERNAT
%token <string> DECIMAL
%token <string> TZ
%token <string> MTZ
%token <string> UTZ
%token <string> ADDRESS
%token <string> TX_ROLLUP_L2_ADDRESS
%token <string> DURATION
%token <string> DATE
%token <string> BYTES
%token <string> PERCENT_LIT

%nonassoc IN

%left COMMA SEMI_COLON

%right OTHERWISE
%right THEN ELSE

%nonassoc prec_var
%nonassoc QUESTIONCOLONEQUAL COLONEQUAL PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL AMPEQUAL PIPEEQUAL
%right COLON

%right IMPLY
%nonassoc EQUIV

%nonassoc QUESTION

%left OR XOR
%left AND

%nonassoc EQUAL NEQUAL
%nonassoc prec_order
%nonassoc prec_labelexpr
%nonassoc prec_tern
%left LESS_LESS_PIPE PIPE_GREATER_GREATER
%left LESS_EQUAL_GREATER
%left GREATER GREATEREQUAL LESS LESSEQUAL

%left PLUS MINUS
%left MULT SLASH PERCENT DIV SLASHPERCENT

%right NOT

%type <ParseTree.archetype> main
%type <ParseTree.expr> start_expr

%start main start_expr

%%

%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos; }
      }

sl(separator, X):
/* empty */             { [] }
| l = snl(separator, X) { l }

snl(separator, X):
  x = X { [ x ] }
| x = X; separator { [ x ] }
| x = X; separator; xs = snl(separator, X) { x :: xs }

snl2(separator, X):
  x = X; separator; y = X { [ x; y ] }
| x = X; separator; xs = snl2(separator, X) { x :: xs }

%inline paren(X):
| LPAREN x=X RPAREN { x }

%inline braced(X):
| LBRACE x=X RBRACE { x }

%inline bracket(X):
| LBRACKET x=X RBRACKET { x }

start_expr:
| x=expr EOF { x }

main:
 | x=loc(archetype_r) { x }

archetype_r:
 | x=implementation_archetype EOF { x }
 | x=archetype_extension      EOF { x }

archetype_extension:
 | ARCHETYPE EXTENSION id=ident xs=paren(declarations) ys=braced(declarations)
     { Mextension (id, xs, ys) }

implementation_archetype:
 | x=declarations { Marchetype x }

%inline declarations:
| xs=declaration+ { xs }

%inline declaration:
| e=loc(declaration_r) { e }

declaration_r:
 | x=archetype          { x }
 | x=import             { x }
 | x=constant           { x }
 | x=variable           { x }
 | x=enum               { x }
 | x=asset              { x }
 | x=record             { x }
 | x=event              { x }
 | x=entry              { x }
 | x=entry_simple       { x }
 | x=transition         { x }
 | x=dextension         { x }
 | x=namespace          { x }
 | x=function_decl      { x }
 | x=getter_decl        { x }
 | x=view_decl          { x }
 | x=specification_decl { x }
 | x=specasset          { x }
 | x=specfun            { x }
 | x=specentry          { x }
 | x=specgetter         { x }
 | x=specvariable       { x }
 | x=security_decl      { x }
 | x=type_decl          { x }
 | INVALID_DECL         { Dinvalid }

archetype:
| ARCHETYPE exts=option(extensions) x=ident ps=parameters m=metadata { Darchetype (x, ps, m, exts) }

import:
| IMPORT x=ident FROM y=loc(STRING) {Dimport (x, y)}

%inline metadata:
| WITH_METADATA v=loc(STRING)     { Some (Muri v) }
| WITH_METADATA v=loc(STRING_EXT) { Some (Mjson v) }
| /* empty */                     { None }

%inline parameters:
 | /* empty */                             { None }
 | xs=loc(parameters_unloc)                { Some xs }

%inline parameters_unloc:
 | LPAREN xs=snl(COMMA, parameter) RPAREN  { xs }

%inline parameter:
 | x=loc(parameter_unloc)                  { x }

%inline parameter_unloc:
 | c=boption(CONST) id=ident COLON ty=type_t dv=parameter_init?  { (id, ty, dv, c) }

%inline parameter_init:
 | EQUAL x=simple_expr { x }

%inline invariants:
| /* empty */                 { [] }
| WITH xs=braced(label_exprs) { xs }

vc_decl(X):
| X exts=extensions? x=ident COLON t=type_t dv=default_value? invs=invariants
    { (x, t, dv, invs, exts) }

constant:
  | x=vc_decl(CONSTANT) { let x, t, dv, invs, exts = x in
                          Dvariable (x, t, dv, VKconstant, invs, exts) }

variable:
  | x=vc_decl(VARIABLE) { let x, t, dv, invs, exts = x in
                          Dvariable (x, t, dv, VKvariable, invs, exts) }

%inline default_value:
| EQUAL x=expr { x }

%inline ext_args:
 |                                           { [] }
 | LPAREN xs=snl(COMMA, simple_expr) RPAREN  { xs }

dextension:
| PERCENT x=ident args=ext_args { Dextension (x, args) }

%inline extensions:
| xs=extension+ { xs }

%inline extension:
| e=loc(extension_r) { e }

extension_r:
| LBRACKETPERCENT x=ident args=ext_args PERCENTRBRACKET { Eextension (x, args) }

namespace:
| NAMESPACE x=ident xs=braced(declarations) { Dnamespace (x, xs) }

%inline fun_body:
| e=expr { (None, e) }
| s=specification_fun
      EFFECT e=braced(expr)
        { (Some s, e) }

%inline function_gen:
 | FUNCTION id=ident xs=function_args
     r=function_return? LBRACE b=fun_body RBRACE {
  let (s, e) = b in
  {
    name   = id;
    args   = xs;
    ret_t  = r;
    spec   = s;
    body   = e;
    getter = false;
    view   = false;
    view_visibility = VVnone;
  }
}

function_item:
| f=loc(function_gen)
    { f }

function_decl:
| f=function_gen
    { Dfunction f }

%inline getter_gen:
 | GETTER id=ident xs=function_args
     r=function_return? LBRACE b=fun_body RBRACE {
  let (s, e) = b in
  {
    name   = id;
    args   = xs;
    ret_t  = r;
    spec   = s;
    body   = e;
    getter = true;
    view   = false;
    view_visibility = VVnone;
  }
}

%inline view_visibility:
| ONCHAIN OFFCHAIN { VVonoffchain }
| ONCHAIN          { VVonchain }
| OFFCHAIN         { VVoffchain }
|                  { VVnone}

%inline view_gen:
 | vv=view_visibility VIEW id=ident xs=function_args
     r=function_return? LBRACE b=fun_body RBRACE {
  let (s, e) = b in
  {
    name   = id;
    args   = xs;
    ret_t  = r;
    spec   = s;
    body   = e;
    getter = false;
    view   = true;
    view_visibility = vv;
  }
}

getter_decl:
| f=getter_gen
    { Dfunction f }

view_decl:
| f=view_gen
    { Dfunction f }

%inline spec_predicate:
| PREDICATE id=ident xs=function_args e=braced(expr) { Vpredicate (id, xs, e) }

%inline spec_fail_item:
| lbl=ident WITH fid=ident? LPAREN arg=ident COLON t=type_t RPAREN COLON f=expr SEMI_COLON
{ (lbl, fid, arg, t, f) }

%inline spec_fail_items:
| xs=spec_fail_item+ { xs }

%inline spec_fails:
| FAILS xs=braced(spec_fail_items) { Vfails xs }

%inline spec_definition:
| DEFINITION id=ident LBRACE a=ident COLON t=type_t PIPE e=expr RBRACE { Vdefinition (id, t, a, e) }

%inline spec_variable:
| VARIABLE id=ident COLON t=type_t dv=default_value? { Vvariable (id, t, dv) }

%inline spec_effect:
| SHADOW EFFECT e=braced(block) { Veffect e }

%inline invars:
| INVARIANT FOR id=ident xs=braced(expr) { (id, split_seq xs) }

%inline uses:
| /* empty */ { [] }
| USE COLON ids=ident+ SEMI_COLON { ids }

%inline spec_body:
| e=expr xs=invars* u=uses { (e, xs, u) }

%inline spec_assert:
| ASSERT id=ident sp=braced(spec_body)
    { let e, xs, u = sp in Vassert (id, e, xs, u) }

%inline spec_postcondition:
| POSTCONDITION id=ident sp=braced(spec_body)
    { let e, xs, u = sp in Vpostcondition (id, e, xs, u, Some PKPost) }

%inline spec_contract_invariant:
| CONTRACT INVARIANT id=ident sp=braced(spec_body)
    { let e, xs, u = sp in Vpostcondition (id, e, xs, u, Some PKInv) }

spec_items:
| ds=loc(spec_definition)*
  ps=loc(spec_predicate)*
  fs=loc(spec_fails)*
  vs=loc(spec_variable)*
  es=loc(spec_effect)*
  bs=loc(spec_assert)*
  ss=loc(spec_postcondition)*
  cs=loc(spec_contract_invariant)*
   { ds @ ps @ fs @ vs @ es @ bs @ ss @ cs }

%inline specification_unloc_c:
| xs=spec_items { xs }

| xs=label_exprs_non_empty
        { let ll = List.map (fun x ->
            let loc, (lbl, e) = Location.deloc x in
            mkloc loc (Vpostcondition (lbl, e, [], [], None))) xs in
            ll }

%inline specification_with_exts_unloc_c:
| x=specification_unloc_c { (x, None) }

%inline specification_c:
| x=loc(specification_with_exts_unloc_c) { x }

%inline specification:
| SPECIFICATION exts=option(extensions) LBRACE xs=specification_unloc_c RBRACE
    { (xs, exts) }

specification_fun:
| x=loc(specification) { x }

specification_decl:
| x=loc(specification)      { Dspecification x }

specasset:
| SPECIFICATION ASSET id=ident LBRACE xs=label_exprs_non_empty RBRACE
{ Dspecasset (id, xs) }

specfun_gen(X):
| SPECIFICATION X id=ident args=function_args LBRACE s=specification_c RBRACE
{ (id, args, s) }

specfun:
| x=specfun_gen(FUNCTION) { let id, args, s = x in Dspecfun (SKfunction, id, args, s) }

specentry:
| x=specfun_gen(ENTRY)    { let id, args, s = x in Dspecfun (SKentry, id, args, s) }

specgetter:
| x=specfun_gen(GETTER)    { let id, args, s = x in Dspecfun (SKgetter, id, args, s) }

specvariable:
| SPECIFICATION VARIABLE id=ident LBRACE xs=label_exprs_non_empty RBRACE
{ Dspecvariable (id, xs) }

%inline security_item_unloc:
| lbl=ident COLON id=ident args=security_args
    { (lbl, id, args) }

%inline security_item:
| x=loc(security_item_unloc) { x }

security_decl_unloc:
| SECURITY exts=option(extensions) LBRACE
    xs=sl(SEMI_COLON, security_item) RBRACE
        { (xs, exts) }

security_decl:
| x=loc(security_decl_unloc)      { Dsecurity x }

type_decl:
| TYPE id=ident EQUAL t=type_t    { Dtype (id, t) }

enum:
| STATES exts=extensions? body=prefix(EQUAL, enum_body)?
    {Denum (EKstate, (Tools.Option.get_dfl [] body, exts))}

| ENUM exts=extensions? x=ident body=prefix(EQUAL, enum_body)?
    {Denum (EKenum x, (Tools.Option.get_dfl [] body, exts))}

enum_body:
| xs=enum_cdecl* { xs }

enum_cdecl:
| PIPE x=ident tys=enum_types opts=enum_option*
    { (x, Tools.Option.get_dfl [] tys, opts) }

%inline enum_types:
| LESS tys=separated_nonempty_list(MULT, type_s) GREATER { Some tys }
| /* empty */                                            { None }

enum_option:
| INITIAL                     { EOinitial }
| WITH xs=braced(label_exprs) { EOspecification (xs) }

%inline type_t:
| t=loc(type_r)                             { (t, None) }
| LPAREN t=loc(type_r) a=loc(PIDENT) RPAREN { (t, Some a) }

%inline type_s:
| x=loc(type_s_unloc)     { (x, None) }

type_r:
| x=type_s xs=type_tuples { Ttuple (x::xs) }
| x=type_s_unloc          { x }

type_s_unloc:
| s=scope x=ident                                          { Tref (s, x)              }
| c=container         LESS x=type_t GREATER                { Tcontainer (x, c)        }
| OPTION              LESS x=type_t GREATER                { Toption x                }
| LIST                LESS x=type_t GREATER                { Tlist x                  }
| SET                 LESS x=type_t GREATER                { Tset x                   }
| MAP                 LESS k=type_t COMMA v=type_s GREATER { Tmap (k, v)              }
| BIG_MAP             LESS k=type_t COMMA v=type_s GREATER { Tbig_map (k, v)          }
| ITERABLE_BIG_MAP    LESS k=type_t COMMA v=type_s GREATER { Titerable_big_map (k, v) }
| OR                  LESS k=type_t COMMA v=type_s GREATER { Tor (k, v)               }
| LAMBDA              LESS a=type_t COMMA r=type_s GREATER { Tlambda (a, r)           }
| CONTRACT            LESS x=type_t GREATER                { Tcontract x              }
| TICKET              LESS x=type_t GREATER                { Tticket x                }
| SAPLING_STATE       n=paren(NUMBERNAT)                   { Tsapling_state       n   }
| SAPLING_TRANSACTION n=paren(NUMBERNAT)                   { Tsapling_transaction n   }
| x=paren(type_r)                                          { x                        }

%inline type_tuples:
| xs=type_tuple+ { xs }

%inline type_tuple:
| MULT x=type_s { x }

%inline container:
| AGGREGATE        { Aggregate }
| PARTITION        { Partition }
| ASSET_CONTAINER  { AssetContainer }
| ASSET_KEY        { AssetKey }
| ASSET_VALUE      { AssetValue }
| ASSET_VIEW       { AssetView }

%inline shadow_asset_fields:
| /* empty */ { [] }
| SHADOW x=asset_fields { x }

%inline record_position:
| AS x=paren(expr) { x }

record:
| RECORD exts=extensions? x=ident fields=asset_fields? pos=record_position?
{ let fs = match fields with | None -> [] | Some x -> x in
  Drecord (x, fs, pos, exts) }

event:
| EVENT exts=extensions? x=ident fields=asset_fields? pos=record_position?
{ let fs = match fields with | None -> [] | Some x -> x in
  Devent (x, fs, pos, exts) }

asset:
| ASSET exts=extensions? x=ident opts=asset_options?
        fields=asset_fields?
        sfields=shadow_asset_fields
                 apo=asset_post_options
                       {
                         let fs = match fields with | None -> [] | Some x -> x in
                         let os = match opts with | None -> [] | Some x -> x in
                         Dasset (x, fs, sfields, os, apo, None, exts) }

%inline by_or_with:
| BY {}
| WITH {}

asset_post_option:
| WITH STATES x=ident                                               { APOstates x }
| WITH xs=braced(label_exprs)                                       { APOconstraints (xs) }
| INITIALIZED by_or_with LBRACE l=separated_nonempty_list(SEMI_COLON, record_expr) RBRACE { APOinit l }

%inline record_expr:
 | x=loc(record_expr_unloc) { x }

%inline record_expr_unloc:
 | s=scope LBRACE xs=separated_nonempty_list(SEMI_COLON, record_item) RBRACE
     { Erecord (s, xs) }

%inline asset_post_options:
 | xs=asset_post_option* { xs }

%inline asset_fields:
| fields=braced(fields) { fields }

%inline asset_options:
| xs=asset_option+ { xs }

%inline map_kind:
| MAP              { MKMap }
| BIG_MAP          { MKBigMap }
| ITERABLE_BIG_MAP { MKIterableBigMap }

asset_option:
| IDENTIFIED BY xs=ident+ { AOidentifiedby xs }
| SORTED BY x=ident       { AOsortedby x }
| TO x=map_kind           { AOtoMapKind x }

%inline fields:
| xs=sl(SEMI_COLON, field) { xs }

field_r:
| x=ident exts=option(extensions)
      COLON y=type_t
          dv=default_value?
    { Ffield (x, y, dv, exts) }

%inline field:
| f=loc(field_r) { f }

%inline ident:
| x=loc(IDENT) { x }
| x=loc(PIDENT) { x }

%inline scope:
 | x=ident COLONCOLON { SIId x }
 |         COLONCOLON { SIParent }
 |                    { SINone }

entry:
  ENTRY exts=option(extensions) x=ident
    args=function_args xs=transitems_eq
      { let a, b = xs in Dentry (x, args, a, b, exts) }

entry_simple:
  ENTRY exts=option(extensions) x=ident
    args=function_args e=braced(block)
      { Dentry (x, args, dummy_entry_properties, Some (e, None), exts) }

transition_to_item:
| TO x=ident y=require_value? z=with_effect? { (x, y, z) }

%inline transitions:
 | xs=transition_to_item+ { xs }

on_value:
 | ON LPAREN x=ident COLON y=type_t RPAREN { x, y }

transition:
  TRANSITION exts=option(extensions) x=ident
    args=function_args on=on_value? LBRACE xs=entry_properties FROM f=simple_expr trs=transitions RBRACE
      { Dtransition (x, args, on, f, xs, trs, exts) }

%inline transitems_eq:
| { (dummy_entry_properties, None) }
| LBRACE xs=entry_properties e=effect? RBRACE { (xs, e) }

%inline otherwise_section:
| OTHERWISE x=simple_expr { x }

%inline accept_transfer:
| /* empty */     { (true, None) }
| NO_TRANSFER o=otherwise_section? { (false, o) }
| ACCEPT_TRANSFER o=otherwise_section? { (true, o) }

entry_properties:
  sp=specification_fun? at=accept_transfer sb=sourcedby? cb=calledby? si=state_is? cst=constants? cs=require? fi=failif? fs=function_item*
  {
    {
      accept_transfer = at;
      sourcedby       = sb;
      calledby        = cb;
      state_is        = si;
      constants       = cst;
      require         = cs;
      failif          = fi;
      functions       = fs;
      spec_fun        = sp;
    }
  }

calledby:
| CALLED BY exts=option(extensions) x=expr o=otherwise_section? { (x, o, exts) }

sourcedby:
| SOURCED BY exts=option(extensions) x=expr o=otherwise_section? { (x, o, exts) }

%inline state_is:
| STATE_IS id=ident o=otherwise_section? { (id, o) }

%inline rfs(X):
| /* empty */   { [] }
| l=rfs_non_empty(X) { l }

%inline rfs_non_empty(X):
| l=snl(SEMI_COLON, rf(X)) { l }

rf(X):
| id=ident COLON e=expr %prec prec_labelexpr { (id, e, None) }
| id=ident COLON e=expr f=rfi(X) %prec prec_labelexpr { (id, e, Some f) }

%inline rfi(X):
| X e=expr { e }

cfs:
| /* empty */   { [] }
| l=cfs_non_empty { l }

%inline cfs_non_empty:
| l=snl(SEMI_COLON, cf) { l }

cf:
| id=ident IS e=expr %prec prec_labelexpr                          { (id, e, None) }
| id=ident QUESTIONIS e=expr OTHERWISE f=expr %prec prec_labelexpr { (id, e, Some f) }

%inline constants:
 | CONSTANT exts=option(extensions) xs=braced(cfs)
       { (xs, exts) }

%inline require:
 | REQUIRE exts=option(extensions) xs=braced(rfs(OTHERWISE))
       { (xs, exts) }

%inline failif:
 | FAIL_IF exts=option(extensions) xs=braced(rfs(WITH))
       { (xs, exts) }

%inline require_value:
| WHEN exts=option(extensions) e=braced(expr) { (e, exts) }

%inline with_effect:
| WITH e=effect { e }

effect:
 | EFFECT exts=option(extensions) e=braced(block) { (e, exts) }
 | INVALID_EFFECT                                 { (mkloc Location.dummy Einvalid, None) }

%inline function_return:
 | COLON ty=type_t { ty }

%inline function_args:
 | LPAREN xs=sl(COMMA, function_arg) RPAREN { xs }

%inline function_arg:
 | id=ident exts=option(extensions) COLON ty=type_t
     { (id, ty, exts) }

%inline assignment_operator_record:
 | EQUAL                    { ValueAssign }
 | COLONEQUAL               { ValueAssign }
 | op=assignment_operator_extra { op }

%inline assignment_operator_expr:
 | COLONEQUAL               { ValueAssign }
 | op=assignment_operator_extra { op }

%inline assignment_operator_extra:
 | PLUSEQUAL   { PlusAssign }
 | MINUSEQUAL  { MinusAssign }
 | MULTEQUAL   { MultAssign }
 | DIVEQUAL    { DivAssign }
 | AMPEQUAL    { AndAssign }
 | PIPEEQUAL   { OrAssign }

%inline branchs:
 | xs=branch+ { xs }

branch:
 | xs=patterns IMPLY x=expr { (xs, x) }

%inline patterns:
 | xs=loc(pattern)+ { xs }

%inline pattern:
 | PIPE p=pattern_r { p }

%inline pattern_r:
 | UNDERSCORE
     { Pwild }

 | i=loc(pname) x=ident
     { Pref (i, [x]) }

 | i=loc(pname) xs=paren(separated_nonempty_list(COMMA, ident))?
     { Pref (i, Tools.Option.get_dfl [] xs) }

| LBRACKET RBRACKET
     { let lc = Location.make $startpos $endpos in
       Pref (mkloc lc PNil, []) }

| x1=ident lc=loc(COLONCOLON) x2=ident
     { Pref (mkloc (loc lc) PCons, [x1; x2]) }

pname:
 | x=ident { PIdent (unloc x) }
 | SOME    { PSome  }
 | NONE    { PNone  }
 | LEFT    { PLeft  }
 | RIGHT   { PRight }

%inline expr:
 | e=loc(expr_r) { e }

%inline ident_typ_q_item:
 | LPAREN ids=ident+ t=quant_kind RPAREN { List.map (fun x -> (x, t)) ids }

ident_typ_q:
 | xs=ident_typ_q_item+ { List.flatten xs }

%inline colon_type_opt:
|                { None }
| COLON t=type_s { Some t }

%inline colon_ident:
|                { None }
| COLON i=ident  { Some i }

%inline for_ident_unloc:
| i=ident                             { FIsimple i }
| LPAREN x=ident COMMA y=ident RPAREN { FIdouble (x, y) }

%inline for_ident:
 | e=loc(for_ident_unloc) { e }

%inline from_expr:
|                { None }
| FROM e=simple_expr    { Some e }

%inline get_const:
 | VAR   { false }
 | CONST { true  }

%inline get_typed_id:
 | ADDRESS_TO_CONTRACT { Location.dumloc "address_to_contract" }
 | MAKE_SET            { Location.dumloc "make_set"            }
 | MAKE_LIST           { Location.dumloc "make_list"           }
 | MAKE_MAP            { Location.dumloc "make_map"            }
 | MAKE_BIG_MAP        { Location.dumloc "make_big_map"        }
 | MAKE_ASSET          { Location.dumloc "make_asset"          }
 | MAKE_EVENT          { Location.dumloc "make_event"          }

%inline tentry_postfix:
| DOT ida=ident arga=paren(expr) { (ida, arga) }

expr_r:
 | LPAREN RPAREN
     { Enothing }

 | c=expr QUESTION x=expr COLON y=expr %prec prec_tern
     { Eternary (c, x, y) }

 | q=quantifier id=ident t=quant_kind COMMA y=expr
     { Equantifier (q, id, t, y) }

 | q=quantifier xs=ident_typ_q COMMA y=expr
    {
      (List.fold_right (fun (i, t) acc ->
           let l = loc i in
           mkloc l (Equantifier (q, i, t, acc))) xs y) |> unloc
    }

 | LET SOME i=ident t=colon_type_opt EQUAL e=expr IN y=expr OTHERWISE o=expr
     { Eletin (i, t, e, y, Some o) }

 | LET i=ident t=colon_type_opt EQUAL e=expr IN y=expr
     { Eletin (i, t, e, y, None) }

 | c=get_const i=ident t=colon_type_opt EQUAL e=expr %prec prec_var
     { Evar (i, t, e, c) }

 | c=get_const i=ident t=colon_type_opt QUESTIONEQUAL e=expr COLON f=expr %prec prec_var
     { Evaropt (i, t, e, Some f, c) }

 | c=get_const i=ident t=colon_type_opt QUESTIONEQUAL e=expr %prec prec_var
     { Evaropt (i, t, e, None, c) }

 | e1=expr SEMI_COLON e2=expr
     { Eseq (e1, e2) }

 | ASSERT id=ident
     { Eassert id }

 | LABEL id=ident
     { Elabel id }

 | FOR lbl=colon_ident x=for_ident IN y=expr DO body=block DONE
     { Efor (lbl, x, y, body) }

 | ITER lbl=colon_ident x=ident a=from_expr TO b=expr DO body=block DONE
     { Eiter (lbl, x, a, b, body) }

 | WHILE lbl=colon_ident c=expr DO body=block DONE
     { Ewhile (lbl, c, body) }

 | IF c=expr THEN t=expr
     { Eif (c, t, None) }

 | IF c=expr THEN t=expr ELSE e=expr
     { Eif (c, t, Some e) }

 | xs=paren(snl2(COMMA, expr))
     { Etuple xs }

 | x=expr op=assignment_operator_expr y=expr
     { Eassign (op, x, y) }

 | x=expr QUESTIONCOLONEQUAL y=expr COLON z=simple_expr
     { Eassignopt (x, y, z) }

 | TRANSFER x=simple_expr
     { Etransfer (TToperation (x)) }

 | TRANSFER x=simple_expr TO y=simple_expr
     { Etransfer (TTsimple (x, y)) }

 | TRANSFER x=simple_expr TO y=simple_expr CALL id=ident LESS t=type_t GREATER args=paren(expr)
     { Etransfer (TTcontract (x, y, id, t, args)) }

 | TRANSFER x=simple_expr TO ENTRY id=ident arg=paren(expr) p=tentry_postfix?
     { match p with | Some (ida, arga) -> Etransfer (TTentry2 (x, id, arg, ida, arga)) | None -> Etransfer (TTentry (x, id, arg)) }

 | TRANSFER x=simple_expr TO ENTRY SELF DOT id=ident args=paren(sl(COMMA, simple_expr))
     { Etransfer (TTself (x, id, args)) }

 | DO_REQUIRE LPAREN x=expr COMMA y=expr RPAREN
     { Edorequire (x, y) }

 | DO_FAIL_IF LPAREN x=expr COMMA y=expr RPAREN
     { Edofailif (x, y) }

 | FAIL e=paren(expr)
     { Efail e }

 | FAILSOME e=paren(expr)
     { Efailsome e }

 | RETURN x=simple_expr
     { Ereturn x }

 | x=order_operations %prec prec_order { x }

 | e1=expr op=loc(bin_operator) e2=expr
     { Eapp ( Foperator op, [e1; e2]) }

 | op=loc(un_operator) x=expr
     { Eapp ( Foperator op, [x]) }

 | x=simple_expr_r
     { x }

%inline block:
 | x=loc(block_r) { x }

%inline block_r:
| e=expr_r SEMI_COLON? { e }

order_operation:
 | e1=expr op=loc(ord_operator) e2=expr
     { Eapp ( Foperator op, [e1; e2]) }

order_operations:
  | e=order_operation { e }
  | ops=loc(order_operations) op=loc(ordering_operator) e=expr
    {
      match unloc ops with
      | Eapp (Foperator ({pldesc = Cmp opa; plloc = lo}), [lhs; rhs]) ->
	 Emulticomp (lhs, [mkloc lo opa, rhs; op, e])
      | Emulticomp (a, l) ->
	 Emulticomp (a, l @ [op, e])
      | _ -> assert false
    }

%inline app_args:
 | LPAREN RPAREN         { [] }
 | LPAREN xs=snl(COMMA, expr) RPAREN  { xs }

%inline simple_expr:
 | x=loc(simple_expr_r) { x }

simple_expr_r:
 | MATCH x=expr WITH xs=branchs END { Ematchwith (x, xs) }

 | FOLD LPAREN x=expr COMMA id=ident IMPLY e=expr RPAREN { Efold (x, id, e) }

 | MAP LPAREN x=expr COMMA id=ident IMPLY e=expr RPAREN { Emap (x, id, e) }

 | id=ident a=app_args
     { Eapp ( Fident id, a) }

 | id=get_typed_id LESS ts=snl(COMMA, type_t) GREATER a=app_args
     { Eappt ( Fident id, ts, a) }

 | x=simple_expr DOT s=scope y=ident
     { Edot (x, (s, y)) }

 | i=simple_expr LBRACKET e=expr RBRACKET
     { Esqapp (i, e) }

 | x=simple_expr DOT id=ident a=app_args
     { Emethod (x, id, a) }

 | x=simple_expr QUESTIONDOT s=scope y=ident
     { Equestiondot (x, (s, y)) }

 | s=scope LBRACKET RBRACKET
     { Earray (s, []) }

 | s=scope LBRACKET e=expr RBRACKET
     { Earray (s, split_seq e) }

 | LBRACE e=simple_expr WITH xs=separated_list(SEMI_COLON, recupdate_item) RBRACE
     { Erecupdate (e, xs) }

 | s=scope LBRACE xs=separated_list(SEMI_COLON, record_item) RBRACE
     { Erecord (s, xs) }

 | x=literal
     { Eliteral x }

 | s=scope vt=vt x=ident
     { Eterm (vt, (s, x)) }

 | SOME x=paren(simple_expr)
     { Eoption (OSome x) }

 | NONE %prec prec_order
     { Eoption (ONone None) }

 | NONE LESS t=type_t GREATER
     { Eoption (ONone (Some t)) }

 | LEFT LESS t=type_t GREATER x=paren(expr)
     { Eor (Oleft (None, t, x)) }

 | LEFT LESS UNDERSCORE COMMA t=type_t GREATER x=paren(expr)
     { Eor (Oleft (None, t, x)) }

 | LEFT LESS ot=type_t COMMA t=type_t GREATER x=paren(expr)
     { Eor (Oleft (Some ot, t, x)) }

 | RIGHT LESS t=type_t GREATER x=paren(expr)
     { Eor (Oright (t, None, x)) }

 | RIGHT LESS t=type_t COMMA UNDERSCORE GREATER x=paren(expr)
     { Eor (Oright (t, None, x)) }

 | RIGHT LESS t=type_t COMMA ot=type_t GREATER x=paren(expr)
     { Eor (Oright (t, Some ot, x)) }

 | LAMBDA LESS rt=type_t GREATER LPAREN LPAREN id=ident COLON at=type_t RPAREN IMPLY e=expr RPAREN
     { Elambda (Some rt, id, Some at, e) }

 | LAMBDA LESS rt=type_t GREATER LPAREN id=ident IMPLY e=expr RPAREN
     { Elambda (Some rt, id, None, e) }

 | LAMBDA LPAREN LPAREN id=ident COLON at=type_t RPAREN IMPLY e=expr RPAREN
     { Elambda (None, id, Some at, e) }

 | LAMBDA LPAREN id=ident IMPLY e=expr RPAREN
     { Elambda (None, id, None, e) }

 | UNPACK LESS t=type_t GREATER x=paren(expr)
     { Eunpack (t, x) }

 | EMIT LESS t=type_t GREATER x=paren(expr)
     { Eemit (t, x) }

 | SELF DOT x=ident
     { Eself x }

 | GET_ENTRYPOINT LESS t=type_t GREATER LPAREN a=expr COMMA b=expr RPAREN
     { Eentrypoint (t, a, b, None) }

 | REQUIRE_ENTRYPOINT LESS t=type_t GREATER LPAREN a=expr COMMA b=expr COMMA c=expr RPAREN
     { Eentrypoint (t, a, b, Some c) }

 | CALL_VIEW LESS t=type_t GREATER LPAREN a=expr COMMA b=expr COMMA c=expr RPAREN
     { Ecallview (t, a, b, c) }

 | ANY
     { Eany }

 | UNIT
     { Eunit }

 | INVALID_EXPR
     { Einvalid }

 | x=paren(block_r)
     { x }

 | BEGIN x=block_r END
     { x }

%inline vt_vset:
| ADDED   { (VSAdded   : var_vset) }
| UNMOVED { (VSUnmoved : var_vset) }
| REMOVED { (VSRemoved : var_vset) }

%inline vt_lbl:
| BEFORE
   { VLBefore }

| AT LPAREN l=ident RPAREN
   { VLIdent l }

%inline vt:
| vset=ioption(postfix(vt_vset, DOT))
   lbl=ioption(postfix(vt_lbl , DOT))
   { (vset, lbl) }

%inline label_exprs:
| /* empty */   { [] }
| l=label_exprs_non_empty { l }

%inline label_exprs_non_empty:
| l=snl(SEMI_COLON, label_expr) { l }

%inline label_expr:
| le=loc(label_expr_unloc) { le }

label_expr_unloc:
| id=ident COLON e=expr %prec prec_labelexpr { (id, e) }

%inline quant_kind:
| COLON t=type_s      { Qtype t }
| IN    e=simple_expr { Qcollection e }

literal:
 | x=NUMBERINT      { Lint            x }
 | x=NUMBERNAT      { Lnat            x }
 | x=DECIMAL        { Ldecimal        x }
 | x=TZ             { Ltz             x }
 | x=MTZ            { Lmtz            x }
 | x=UTZ            { Lutz            x }
 | x=STRING         { Lstring         x }
 | x=ADDRESS        { Laddress        x }
 | x=TX_ROLLUP_L2_ADDRESS { Ltx_rollup_l2_address x }
 | x=bool_value     { Lbool           x }
 | x=DURATION       { Lduration       x }
 | x=DATE           { Ldate           x }
 | x=BYTES          { Lbytes          x }
 | x=PERCENT_LIT    { Lpercent        x }

%inline bool_value:
 | TRUE  { true }
 | FALSE { false }

record_item:
 | e=simple_expr { (None, e) }
 | id=ident op=assignment_operator_record e=simple_expr
   { (Some (op, id), e) }

recupdate_item:
 | id=ident EQUAL e=simple_expr { (id, e) }

%inline quantifier:
 | FORALL { Forall }
 | EXISTS { Exists }

%inline logical_operator:
 | AND   { And }
 | OR    { Or }
 | XOR   { Xor }
 | IMPLY { Imply }
 | EQUIV { Equiv }

%inline comparison_operator:
 | EQUAL        { Equal }
 | NEQUAL       { Nequal }

%inline ordering_operator:
 | LESS         { Lt }
 | LESSEQUAL    { Le }
 | GREATER      { Gt }
 | GREATEREQUAL { Ge }

%inline arithmetic_operator:
 | PLUS    { Plus   }
 | MINUS   { Minus  }
 | MULT    { Mult   }
 | SLASH   { DivRat }
 | DIV     { DivEuc }
 | PERCENT { Modulo }
 | SLASHPERCENT         { DivMod      }
 | LESS_EQUAL_GREATER   { ThreeWayCmp }
 | LESS_LESS_PIPE       { ShiftLeft   }
 | PIPE_GREATER_GREATER { ShiftRight  }

%inline unary_operator:
 | MINUS   { Uminus }
 | NOT     { Not }

%inline bin_operator:
| op=logical_operator    { Logical op }
| op=comparison_operator { Cmp op }
| op=arithmetic_operator { Arith op }

%inline un_operator:
| op=unary_operator      { Unary op }

%inline ord_operator:
| op=ordering_operator   { Cmp op }

%inline security_args:
| args=paren(separated_list(COMMA, security_arg)) { args}

%inline security_arg:
 | e=loc(security_arg_unloc) { e }

security_arg_unloc:
| id=ident                                     { Sident id }
| a=ident DOT b=ident                          { Sdot (a, b) }
| xs=bracket(snl2(OR, security_arg))           { Slist xs }
| id=ident xs=paren(sl(COMMA, security_arg))   { Sapp (id, xs) }
| id=ident BUT arg=security_arg                { Sbut (id, arg) }
| id=ident TO arg=security_arg                 { Sto (id, arg) }
| x=paren(security_arg_unloc)                  { x }

%inline postfix(X, P):
| x=X P { x }

%inline prefix(P, X):
| P x=X { x }
