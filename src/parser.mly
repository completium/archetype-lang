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

  let dummy_entry_properties = {
      accept_transfer = true;
      calledby        = None;
      require         = None;
      failif          = None;
      spec_fun        = None;
      functions       = [];
    }

  let rec split_seq e =
    match unloc e with
    | Eseq (a, b) -> (split_seq a) @ (split_seq b)
    | _ -> [e]

%}

%token ACCEPT_TRANSFER
%token ADDED
%token AGGREGATE
%token AMPEQUAL
%token AND
%token ANY
%token ARCHETYPE
%token ASSERT
%token ASSET
%token AT
%token AT_ADD
%token AT_REMOVE
%token AT_UPDATE
%token BEFORE
%token BUT
%token BY
%token CALL
%token CALLED
%token COLON
%token COLONEQUAL
%token COMMA
%token CONSTANT
%token CONTRACT
%token DEFINITION
%token DIV
%token DIVEQUAL
%token DO
%token DOFAILIF
%token DONE
%token DOREQUIRE
%token DOT
%token EFFECT
%token ELSE
%token END
%token ENTRY
%token ENTRYPOINT
%token ENUM
%token EOF
%token EQUAL
%token EQUIV
%token EXISTS
%token EXTENSION
%token FAIL
%token FAILIF
%token FAILS
%token FALSE
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
%token IN
%token INITIAL
%token INITIALIZED
%token INVARIANT
%token ITER
%token LABEL
%token LBRACE
%token LBRACKET
%token LBRACKETPERCENT
%token LESS
%token LESSEQUAL
%token LET
%token LIST
%token LPAREN
%token MAP
%token MATCH
%token MINUS
%token MINUSEQUAL
%token MULT
%token MULTEQUAL
%token NAMESPACE
%token NEQUAL
%token NONE
%token NOT
%token ON
%token OPTION
%token OR
%token OTHERWISE
%token PARTITION
%token PERCENT
%token PERCENTRBRACKET
%token PIPE
%token PIPEEQUAL
%token PKEY
%token PLUS
%token PLUSEQUAL
%token POSTCONDITION
%token PREDICATE
%token RBRACE
%token RBRACKET
%token RECORD
%token REF
%token REFUSE_TRANSFER
%token REMOVED
%token REQUIRE
%token RETURN
%token RPAREN
%token SECURITY
%token SELF
%token SEMI_COLON
%token SET
%token SHADOW
%token SLASH
%token SOME
%token SORTED
%token SPECIFICATION
%token STATES
%token THEN
%token TO
%token TRANSFER
%token TRANSITION
%token TRUE
%token UNDERSCORE
%token UNMOVED
%token UNPACK
%token USE
%token VAR
%token VARIABLE
%token VIEW
%token WHEN
%token WHILE
%token WITH
%token XOR

%token INVALID_EXPR
%token INVALID_DECL
%token INVALID_EFFECT

%token <string> IDENT
%token <string> STRING
%token <Big_int.big_int> NUMBERINT
%token <Big_int.big_int> NUMBERNAT
%token <string> DECIMAL
%token <Big_int.big_int> TZ
%token <Big_int.big_int> MTZ
%token <Big_int.big_int> UTZ
%token <string> ADDRESS
%token <string> DURATION
%token <string> DATE
%token <string> BYTES
%token <Big_int.big_int> PERCENT_LIT

%nonassoc IN

%left COMMA SEMI_COLON

%right OTHERWISE
%right THEN ELSE

%nonassoc prec_var
%nonassoc COLONEQUAL PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL AMPEQUAL PIPEEQUAL

%right IMPLY
%nonassoc EQUIV

%left OR XOR
%left AND

%nonassoc EQUAL NEQUAL
%nonassoc prec_order
%nonassoc prec_labelexpr
%left GREATER GREATEREQUAL LESS LESSEQUAL

%left PLUS MINUS
%left MULT SLASH PERCENT
%left DIV

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
 | x=constant           { x }
 | x=variable           { x }
 | x=enum               { x }
 | x=asset              { x }
 | x=record             { x }
 | x=entry              { x }
 | x=entry_simple       { x }
 | x=transition         { x }
 | x=dextension         { x }
 | x=namespace          { x }
 | x=function_decl      { x }
 | x=getter_decl        { x }
 | x=specification_decl { x }
 | x=specasset          { x }
 | x=specfun            { x }
 | x=specentry          { x }
 | x=specgetter         { x }
 | x=specvariable       { x }
 | x=security_decl      { x }
 | INVALID_DECL         { Dinvalid }

archetype:
| ARCHETYPE exts=option(extensions) x=ident { Darchetype (x, exts) }

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
  }
}

getter_decl:
| f=getter_gen
    { Dfunction f }

%inline spec_predicate:
| PREDICATE id=ident xs=function_args e=braced(expr) { Vpredicate (id, xs, e) }

%inline spec_fail_item:
| lbl=ident WITH LPAREN arg=ident COLON t=type_t RPAREN COLON f=expr SEMI_COLON
{ (lbl, arg, t, f) }

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

enum:
| STATES exts=extensions? xs=equal_enum_values
    {Denum (EKstate, (xs, exts))}

| ENUM exts=extensions? x=ident xs=equal_enum_values
    {Denum (EKenum x, (xs, exts))}

equal_enum_values:
| /*nothing*/          { [] }
| EQUAL xs=enum_values { xs }

enum_values:
| /*nothing*/    { [] }
| xs=pipe_idents { xs }

%inline pipe_idents:
| xs=pipe_ident+ { xs }

%inline pipe_ident:
| PIPE x=ident opts=enum_options { (x, opts) }

%inline enum_options:
| /* nothing */    { [] }
| xs=enum_option+  { xs }

enum_option:
| INITIAL                     { EOinitial }
| WITH xs=braced(label_exprs) { EOspecification (xs) }

%inline type_t:
| e=loc(type_r) { e }

type_r:
| x=type_s xs=type_tuples { Ttuple (x::xs) }
| x=type_s_unloc          { x }

%inline type_s:
| x=loc(type_s_unloc)     { x }

type_s_unloc:
| x=ident                                          { Tref x            }
| c=container LESS x=type_t GREATER                { Tcontainer (x, c) }
| PKEY        LESS x=type_t GREATER                { Tkeyof x          }
| OPTION      LESS x=type_t GREATER                { Toption x         }
| LIST        LESS x=type_t GREATER                { Tlist x           }
| SET         LESS x=type_t GREATER                { Tset x            }
| MAP         LESS k=type_t COMMA v=type_s GREATER { Tmap (k, v)       }
| CONTRACT    LESS x=type_t GREATER                { Tcontract x       }
| x=paren(type_r)                                  { x                 }

%inline type_tuples:
| xs=type_tuple+ { xs }

%inline type_tuple:
| MULT x=type_s { x }

%inline container:
| AGGREGATE  { Aggregate }
| PARTITION  { Partition }
| VIEW       { View      }

%inline shadow_asset_fields:
| /* empty */ { [] }
| SHADOW x=asset_fields { x }

record:
| RECORD exts=extensions? x=ident fields=asset_fields?
{ let fs = match fields with | None -> [] | Some x -> x in
  Drecord (x, fs, exts) }

asset:
| ASSET exts=extensions? ops=bracket(asset_operation)? x=ident opts=asset_options?
        fields=asset_fields?
        sfields=shadow_asset_fields
                 apo=asset_post_options
                       {
                         let fs = match fields with | None -> [] | Some x -> x in
                         let os = match opts with | None -> [] | Some x -> x in
                         Dasset (x, fs, sfields, os, apo, ops, exts) }

asset_post_option:
| WITH STATES x=ident                                               { APOstates x }
| WITH xs=braced(label_exprs)                                       { APOconstraints (xs) }
| INITIALIZED BY LBRACE l=separated_nonempty_list(SEMI_COLON, record_expr) RBRACE { APOinit l }

%inline record_expr:
 | x=loc(record_expr_unloc) { x }

%inline record_expr_unloc:
 | LBRACE xs=separated_nonempty_list(SEMI_COLON, record_item) RBRACE
     { Erecord xs }

%inline asset_post_options:
 | xs=asset_post_option* { xs }

%inline asset_fields:
| fields=braced(fields) { fields }

%inline asset_options:
| xs=asset_option+ { xs }

asset_option:
| IDENTIFIED BY xs=ident+ { AOidentifiedby xs }
| SORTED BY x=ident       { AOsortedby x }
| TO i=ident              { AOto i }

%inline fields:
| xs=sl(SEMI_COLON, field) { xs }

field_r:
| x=ident exts=option(extensions)
      COLON y=type_t boption(REF)
          dv=default_value?
    { Ffield (x, y, dv, exts) }

%inline field:
| f=loc(field_r) { f }

%inline ident:
| x=loc(IDENT) { x }

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
    args=function_args on=on_value? LBRACE xs=entry_properties FROM f=expr trs=transitions RBRACE
      { Dtransition (x, args, on, f, xs, trs, exts) }

%inline transitems_eq:
| { (dummy_entry_properties, None) }
| LBRACE xs=entry_properties e=effect? RBRACE { (xs, e) }

%inline accept_transfer:
| /* empty */     { true }
| REFUSE_TRANSFER { false }
| ACCEPT_TRANSFER { true }

entry_properties:
  sp=specification_fun? at=accept_transfer cb=calledby? cs=require? fi=failif? fs=function_item*
  {
    {
      accept_transfer = at;
      calledby        = cb;
      require         = cs;
      failif          = fi;
      functions       = fs;
      spec_fun        = sp;
    }
  }

calledby:
 | CALLED BY exts=option(extensions) x=expr { (x, exts) }

%inline rfs(X):
| /* empty */   { [] }
| l=rfs_non_empty(X) { l }

%inline rfs_non_empty(X):
| l=snl(SEMI_COLON, rf(X)) { l }

rf(X):
| id=ident f=rfi(X)? COLON e=expr %prec prec_labelexpr { (id, e, f) }

%inline rfi(X):
| X e=expr { e }

%inline require:
 | REQUIRE exts=option(extensions) xs=braced(rfs(OTHERWISE))
       { (xs, exts) }

%inline failif:
 | FAILIF exts=option(extensions) xs=braced(rfs(WITH))
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

pattern:
  | PIPE UNDERSCORE { Pwild }
  | PIPE i=ident    { Pref i }

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
| FROM e=expr    { Some e }

expr_r:
 | LPAREN RPAREN
     { Enothing }

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

 | VAR i=ident t=colon_type_opt EQUAL e=expr %prec prec_var
     { Evar (i, t, e) }

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

 | xs=paren(snl2(COMMA, simple_expr))
     { Etuple xs }

 | x=expr op=assignment_operator_expr y=expr
     { Eassign (op, x, y) }

 | TRANSFER x=simple_expr TO y=simple_expr
     { Etransfer (x, TTsimple y) }

 | TRANSFER x=simple_expr TO y=simple_expr CALL id=ident LESS t=type_t GREATER args=paren(expr)
     { Etransfer (x, TTcontract (y, id, t, args)) }

 | TRANSFER x=simple_expr TO ENTRY id=ident arg=simple_expr
     { Etransfer (x, TTentry (id, arg)) }

 | TRANSFER x=simple_expr TO ENTRY SELF DOT id=ident args=paren(sl(COMMA, simple_expr))
     { Etransfer (x, TTself (id, args)) }

 | DOREQUIRE LPAREN x=expr COMMA y=expr RPAREN
     { Edorequire (x, y) }

 | DOFAILIF LPAREN x=expr COMMA y=expr RPAREN
     { Edofailif (x, y) }

 | FAIL e=paren(expr)
     { Efail e }

 | RETURN x=simple_expr
     { Ereturn x }

 | SOME x=paren(simple_expr)
     { Eoption (OSome x) }

 | NONE
     { Eoption ONone }

 | UNPACK LESS t=type_t GREATER x=paren(expr)
     { Eunpack (t, x) }

 | SELF DOT x=ident
     { Eself x }

 | ENTRYPOINT LESS t=type_t GREATER LPAREN a=expr COMMA b=expr RPAREN
     { Eentrypoint (t, a, b) }

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

 | id=ident a=app_args
     { Eapp ( Fident id, a) }

 | x=simple_expr DOT y=ident
     { Edot (x, y) }

 | i=simple_expr LBRACKET e=expr RBRACKET
     { Esqapp (i, e) }

 | x=simple_expr DOT id=ident a=app_args
     { Emethod (x, id, a) }

 | LBRACKET RBRACKET
     { Earray [] }

 | LBRACKET e=expr RBRACKET
     { Earray (split_seq e) }

 | LBRACE e=simple_expr WITH xs=separated_list(SEMI_COLON, recupdate_item) RBRACE
     { Erecupdate (e, xs) }

 | LBRACE xs=separated_list(SEMI_COLON, record_item) RBRACE
     { Erecord xs }

 | x=literal
     { Eliteral x }

 | vt=vt x=ident
     { Eterm (vt, x) }

 | ANY
     { Eany }

 | INVALID_EXPR
     { Einvalid }

 | x=paren(block_r)
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
 | x=NUMBERINT   { Lint      x }
 | x=NUMBERNAT   { Lnat      x }
 | x=DECIMAL     { Ldecimal  x }
 | x=TZ          { Ltz       x }
 | x=MTZ         { Lmtz      x }
 | x=UTZ         { Lutz      x }
 | x=STRING      { Lstring   x }
 | x=ADDRESS     { Laddress  x }
 | x=bool_value  { Lbool     x }
 | x=DURATION    { Lduration x }
 | x=DATE        { Ldate     x }
 | x=BYTES       { Lbytes    x }
 | x=PERCENT_LIT { Lpercent  x }

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

%inline asset_operation_enum:
| AT_ADD    { AOadd }
| AT_REMOVE { AOremove }
| AT_UPDATE { AOupdate }

%inline asset_operation:
| xs=asset_operation_enum+ args=option(simple_expr) { AssetOperation (xs, args) }

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
