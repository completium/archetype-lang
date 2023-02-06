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
%token ADDRESS_TO_CONTRACT
%token AGGREGATE
%token AMPEQUAL
%token AND
%token ANY
%token ARCHETYPE
%token AS
%token ASSET
%token ASSET_CONTAINER
%token ASSET_KEY
%token ASSET_VALUE
%token ASSET_VIEW
%token BEGIN
%token BIG_MAP
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
%token DETACH
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
%token EVENT
%token FAIL
%token FAIL_IF
%token FAILSOME
%token FALSE
%token FOLD
%token FOR
%token FROM
%token FUNCTION
%token GETTER
%token GLOBAL_CONSTANT
%token GREATER
%token GREATEREQUAL
%token IDENTIFIED
%token IF
%token IMPLY
%token IMPORT
%token IN
%token INITIAL
%token INITIALIZED
%token IS
%token ITER
%token ITERABLE_BIG_MAP
%token LAMBDA
%token LBRACE
%token LBRACKET
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
%token PIPE
%token PIPE_GREATER_GREATER
%token PIPEEQUAL
%token PLUS
%token PLUSEQUAL
%token QUESTION
%token QUESTIONCOLONEQUAL
%token QUESTIONDOT
%token QUESTIONEQUAL
%token QUESTIONIS
%token RBRACE
%token RBRACKET
%token RECORD
%token REQUIRE_ENTRYPOINT
%token REQUIRE
%token RETURN
%token RIGHT
%token RPAREN
%token SAPLING_STATE
%token SAPLING_TRANSACTION
%token SELF
%token SEMI_COLON
%token SET
%token SLASH
%token SLASHPERCENT
%token SOME
%token SORTED
%token SOURCED
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
%token UNPACK
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
%token <string> TZ_EXPR
%token <string> DURATION
%token <string> DATE
%token <string> BYTES
%token <string> PERCENT_LIT

%nonassoc IN

%left SEMI_COLON

%right OTHERWISE
%right THEN ELSE

%nonassoc prec_var
%nonassoc QUESTIONCOLONEQUAL COLONEQUAL PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL AMPEQUAL PIPEEQUAL
%right COLON

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
 | x=namespace          { x }
 | x=function_decl      { x }
 | x=getter_decl        { x }
 | x=view_decl          { x }
 | x=type_decl          { x }
 | INVALID_DECL         { Dinvalid }

archetype:
| ARCHETYPE x=ident ps=parameters m=metadata { Darchetype (x, ps, m) }

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

vc_decl(X):
| X x=ident COLON t=type_t dv=default_value?
    { (x, t, dv) }

constant:
  | x=vc_decl(CONSTANT) { let x, t, dv = x in
                          Dvariable (x, t, dv, VKconstant) }

variable:
  | x=vc_decl(VARIABLE) { let x, t, dv = x in
                          Dvariable (x, t, dv, VKvariable) }

%inline default_value:
| EQUAL x=expr { x }

namespace:
| NAMESPACE x=ident xs=braced(declarations) { Dnamespace (x, xs) }

%inline fun_body:
| e=expr { e }

%inline function_gen:
 | FUNCTION id=ident xs=function_args
     r=function_return? LBRACE b=fun_body RBRACE {
  {
    name   = id;
    args   = xs;
    ret_t  = r;
    body   = b;
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
  {
    name   = id;
    args   = xs;
    ret_t  = r;
    body   = b;
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
  {
    name   = id;
    args   = xs;
    ret_t  = r;
    body   = b;
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

type_decl:
| TYPE id=ident EQUAL t=type_t    { Dtype (id, t) }

enum:
| STATES body=prefix(EQUAL, enum_body)?
    {Denum (EKstate, (Tools.Option.get_dfl [] body))}

| ENUM x=ident body=prefix(EQUAL, enum_body)?
    {Denum (EKenum x, (Tools.Option.get_dfl [] body))}

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

%inline record_position:
| AS x=paren(expr) { x }

record:
| RECORD x=ident fields=asset_fields? pos=record_position?
{ let fs = match fields with | None -> [] | Some x -> x in
  Drecord (x, fs, pos) }

event:
| EVENT x=ident fields=asset_fields? pos=record_position?
{ let fs = match fields with | None -> [] | Some x -> x in
  Devent (x, fs, pos) }

asset:
| ASSET x=ident opts=asset_options?
        fields=asset_fields?
                 apo=asset_post_options
                       {
                         let fs = match fields with | None -> [] | Some x -> x in
                         let os = match opts with | None -> [] | Some x -> x in
                         Dasset (x, fs, os, apo, None) }

%inline by_or_with:
| BY {}
| WITH {}

asset_post_option:
| WITH STATES x=ident                                               { APOstates x }
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
| x=ident COLON y=type_t dv=default_value?
    { Ffield (x, y, dv) }

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
  ENTRY x=ident args=function_args xs=transitems_eq
      { let a, b = xs in Dentry (x, args, a, b) }

entry_simple:
  ENTRY x=ident args=function_args e=braced(block)
      { Dentry (x, args, dummy_entry_properties, Some e) }

transition_to_item:
| TO x=ident y=require_value? z=with_effect? { (x, y, z) }

%inline transitions:
 | xs=transition_to_item+ { xs }

on_value:
 | ON LPAREN x=ident COLON y=type_t RPAREN { x, y }

transition:
  TRANSITION x=ident args=function_args on=on_value? LBRACE
    xs=entry_properties FROM f=simple_expr trs=transitions
  RBRACE
      { Dtransition (x, args, on, f, xs, trs) }

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
  at=accept_transfer sb=sourcedby? cb=calledby? si=state_is? cst=constants? cs=require? fi=failif? fs=function_item*
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
    }
  }

calledby:
| CALLED BY x=expr o=otherwise_section? { (x, o) }

sourcedby:
| SOURCED BY x=expr o=otherwise_section? { (x, o) }

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
 | CONSTANT xs=braced(cfs)
       { xs }

%inline require:
 | REQUIRE xs=braced(rfs(OTHERWISE))
       { xs }

%inline failif:
 | FAIL_IF xs=braced(rfs(WITH))
       { xs }

%inline require_value:
| WHEN e=braced(expr) { e }

%inline with_effect:
| WITH e=effect { e }

effect:
 | EFFECT e=braced(block) { e }
 | INVALID_EFFECT         { mkloc Location.dummy Einvalid }

%inline function_return:
 | COLON ty=type_t { ty }

%inline function_args:
 | LPAREN xs=sl(COMMA, function_arg) RPAREN { xs }

%inline function_arg:
 | id=ident COLON ty=type_t
     { (id, ty) }

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

%inline colon_type_opt:
|                { None }
| COLON t=type_s { Some t }

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
 | GLOBAL_CONSTANT     { Location.dumloc "global_constant"     }
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

 | FOR x=for_ident IN y=expr DO body=block DONE
     { Efor (x, y, body) }

 | ITER x=ident a=from_expr TO b=expr DO body=block DONE
     { Eiter (x, a, b, body) }

 | WHILE c=expr DO body=block DONE
     { Ewhile (c, body) }

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

 | DETACH id=ident FROM x=simple_expr COLON f=simple_expr
     { Edetach (id, x, f) }

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

 | s=scope x=ident
     { Eterm (s, x) }

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

 | x=TZ_EXPR
     { Etz_expr x }

 | INVALID_EXPR
     { Einvalid }

 | x=paren(block_r)
     { x }

 | BEGIN x=block_r END
     { x }

literal:
 | x=NUMBERINT            { Lint            x }
 | x=NUMBERNAT            { Lnat            x }
 | x=DECIMAL              { Ldecimal        x }
 | x=TZ                   { Ltz             x }
 | x=MTZ                  { Lmtz            x }
 | x=UTZ                  { Lutz            x }
 | x=STRING               { Lstring         x }
 | x=ADDRESS              { Laddress        x }
 | x=TX_ROLLUP_L2_ADDRESS { Ltx_rollup_l2_address x }
 | x=bool_value           { Lbool           x }
 | x=DURATION             { Lduration       x }
 | x=DATE                 { Ldate           x }
 | x=BYTES                { Lbytes          x }
 | x=PERCENT_LIT          { Lpercent        x }

%inline bool_value:
 | TRUE  { true }
 | FALSE { false }

record_item:
 | e=simple_expr { (None, e) }
 | id=ident op=assignment_operator_record e=simple_expr
   { (Some (op, id), e) }

recupdate_item:
 | id=ident EQUAL e=simple_expr { (id, e) }

%inline logical_operator:
 | AND   { And }
 | OR    { Or }
 | XOR   { Xor }

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

%inline postfix(X, P):
| x=X P { x }

%inline prefix(P, X):
| P x=X { x }
