/* parser */
%{
  open ParseTree
  open Location
  open ParseUtils

  let error ?loc code = raise (ParseError (loc, code))

  let dummy_action_properties = {
      calledby      = None;
      condition     = None;
      functions     = [];
      verif         = None;
    }

%}

%token USE
%token MODEL
%token CONSTANT
%token VARIABLE
%token IDENTIFIED
%token SORTED
%token BY
%token AS
%token FROM
%token TO
%token ON
%token WHEN
%token REF
%token FUN
%token EQUALGREATER
%token INITIALIZED
%token COLLECTION
%token QUEUE
%token STACK
%token SET
%token PARTITION
%token ASSET
%token MATCH
%token WITH
%token END
%token ASSERT
%token OBJECT
%token KEY
%token OF
%token ENUM
%token STATES
%token INITIAL
%token ACTION
%token CALLED
%token CONDITION
%token TRANSITION
%token VERIFICATION
%token PREDICATE
%token DEFINITION
%token AXIOM
%token THEOREM
%token INVARIANT
%token SPECIFICATION
%token EFFECT
%token FUNCTION
%token LET
%token IF
%token THEN
%token ELSE
%token FOR
%token IN
%token BREAK
%token TRANSFER
%token BACK
%token EXTENSION
%token NAMESPACE
%token CONTRACT
%token AT_ADD
%token AT_REMOVE
%token AT_UPDATE
%token COLONCOLON
%token LPAREN
%token RPAREN
%token LBRACKETPERCENT
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EQUAL
%token COMMA
%token COLON
%token SEMI_COLON
%token PERCENT
%token PIPE
%token DOT
%token COLONEQUAL
%token PLUSEQUAL
%token MINUSEQUAL
%token MULTEQUAL
%token DIVEQUAL
%token ANDEQUAL
%token OREQUAL
%token AND
%token OR
%token NOT
%token FORALL
%token EXISTS
%token TRUE
%token FALSE
%token IMPLY
%token EQUIV
%token NEQUAL
%token LESS
%token LESSEQUAL
%token GREATER
%token GREATEREQUAL
%token PLUS
%token MINUS
%token MULT
%token DIV
%token UNDERSCORE
%token EOF

%token <string> IDENT
%token <string> STRING
%token <Big_int.big_int> NUMBER
%token <Big_int.big_int * Big_int.big_int> RATIONAL
%token <Big_int.big_int> TZ
%token <string> ADDRESS
%token <string> DURATION
%token <string> DATE

%nonassoc prec_decl
%nonassoc prec_for prec_transfer

%nonassoc TO IN EQUALGREATER
%right THEN ELSE

%left COMMA

%nonassoc COLONEQUAL PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL ANDEQUAL OREQUAL

%right IMPLY
%nonassoc EQUIV

%left OR
%left AND

%nonassoc EQUAL NEQUAL
%nonassoc prec_order
%left GREATER GREATEREQUAL LESS LESSEQUAL

%left PLUS MINUS
%left MULT DIV PERCENT

%right NOT

%right STACK SET QUEUE PARTITION COLLECTION
%nonassoc above_coll

%type <ParseTree.model> main
%type <ParseTree.expr> start_expr

%start main start_expr

%%

%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos; }
      }

snl(separator, X):
  x = X { [ x ] }
| x = X; separator { [ x ] }
| x = X; separator; xs = snl(separator, X) { x :: xs }

%inline paren(X):
| LPAREN x=X RPAREN { x }

%inline braced(X):
| LBRACE x=X RBRACE { x }

%inline bracket(X):
| LBRACKET x=X RBRACKET { x }

start_expr:
| x=expr EOF { x }
| x=loc(error)
      { error ~loc:(loc x) PE_Unknown }

main:
 | x=loc(model_r) { x }
 | x=loc(error)
     { error ~loc:(loc x) PE_Unknown }

model_r:
 | x=implementation_model EOF { x }
 | x=model_extension      EOF { x }

model_extension:
 | MODEL EXTENSION id=ident xs=paren(declarations) EQUAL ys=braced(declarations)
     { Mmodelextension (id, xs, ys) }

implementation_model:
 | x=declarations { Mmodel x }

%inline declarations:
| xs=declaration+ { xs }

%inline declaration:
| e=loc(declaration_r) { e }

declaration_r:
 | x=use                { x }
 | x=model              { x }
 | x=constant           { x }
 | x=variable           { x }
 | x=enum               { x }
 | x=states             { x }
 | x=object_decl        { x }
 | x=key_decl           { x }
 | x=asset              { x }
 | x=action             { x }
 | x=transition         { x }
 | x=dextension         { x }
 | x=namespace          { x }
 | x=contract           { x }
 | x=function_decl      { x }
 | x=verification_decl  { x }

use:
| USE x=ident { Duse x }

model:
| MODEL exts=option(extensions) x=ident { Dmodel (x, exts) }

constant:
| CONSTANT exts=option(extensions) x=ident y=loc(type_ref)
    dv=default_value?
       { Dconstant (x, y, dv, exts) }

%inline type_ref:
| x=ident {Tref x}

variable:
| VARIABLE exts=extensions? x=ident y=loc(type_ref) z=option(value_options)
    dv=default_value?
      { Dvariable (x, y, z, dv, exts) }

%inline value_options:
| xs=value_option+ { xs }

value_option:
| x=from_value { VOfrom x }
| x=to_value   { VOto x }

%inline default_value:
| EQUAL x=expr { x }

%inline from_value:
| FROM x=qualid { x }

%inline to_value:
| TO x=qualid { x }

dextension:
| PERCENT x=ident xs=nonempty_list(simple_expr)? { Dextension (x, xs) }

%inline extensions:
| xs=extension+ { xs }

%inline extension:
| e=loc(extension_r) { e }

extension_r:
| LBRACKETPERCENT x=ident xs=option(simple_expr+) RBRACKET { Eextension (x, xs) }

namespace:
| NAMESPACE x=ident xs=braced(declarations) { Dnamespace (x, xs) }

contract:
| CONTRACT exts=option(extensions) x=ident EQUAL
    xs=braced(signatures)
      dv=default_value?
         { Dcontract (x, xs, dv, exts) }

%inline signatures:
| xs=signature+ { xs }

signature:
| ACTION x=ident COLON xs=types { Ssignature (x, xs) }

%inline fun_effect:
| EFFECT e=loc(expr_extended) { e }

%inline fun_body:
| e=expr { (None, e) }
| LBRACE
  s=loc(verification)
      e=fun_effect RBRACE
        { (Some s, e) }


%inline function_gen:
 | FUNCTION id=ident xs=function_args
     r=function_return? EQUAL b=fun_body {
  let (s, e) = b in
  {
    name  = id;
    args  = xs;
    ret_t = r;
    verif = s;
    body  = e;
  }
}

function_item:
| f=function_gen
    { f }

function_decl:
| f=function_gen
    { Dfunction f }

%inline verif_predicate:
| PREDICATE id=ident xs=function_args EQUAL e=expr { Vpredicate (id, xs, e) }

%inline verif_definition:
| DEFINITION id=ident EQUAL LBRACKET a=ident PIPE e=expr RBRACKET { Vdefinition (id, a, e) }

%inline verif_axiom:
| AXIOM id=ident EQUAL x=expr { Vaxiom (id, x) }

%inline verif_theorem:
| THEOREM id=ident EQUAL x=expr { Vtheorem (id, x) }

%inline verif_variable:
| VARIABLE id=ident t=type_t dv=default_value? { Vvariable (id, t, dv) }

%inline verif_invariant:
| INVARIANT id=ident xs=named_items { Vinvariant (id, xs) }

%inline verif_effect:
| EFFECT e=expr { Veffect e }

%inline verif_specification:
| SPECIFICATION xs=named_items { Vspecification xs }

verif_item:
| x=verif_predicate     { x }
| x=verif_definition    { x }
| x=verif_axiom         { x }
| x=verif_theorem       { x }
| x=verif_variable      { x }
| x=verif_invariant     { x }
| x=verif_effect        { x }
| x=verif_specification { x }

verif_items:
| xs=loc(verif_item)+ { xs }

verification_decl:
| x=loc(verification) { Dverification x }

verification:
 | x=loc(verif_specification)
       { ([x], None) }

 | VERIFICATION exts=option(extensions)
     xs=verif_items
       x=loc(verif_specification)
           { (xs@[x], exts) }

enum:
| ENUM x=ident EQUAL xs=pipe_idents {Denum (x, xs)}

states:
| STATES exts=extensions? x=ident? xs=states_values? {Dstates (x, xs, exts)}

states_values:
| EQUAL xs=pipe_ident_options { xs }

object_decl:
| OBJECT exts=extensions? x=ident y=expr { Dobject (x, y, exts) } %prec prec_decl

key_decl:
| KEY exts=extensions? x=ident OF y=expr { Dkey (x, y, exts) } %prec prec_decl

types:
| xs=separated_nonempty_list(COMMA, type_t) { xs }

%inline type_t:
| e=loc(type_r) { e }

type_r:
| x=type_s xs=type_tuples { Ttuple (x::xs) }
| x=type_s_unloc          { x }
| x=type_t IMPLY y=type_t { Tapp (x, y) }

%inline type_s:
| x=loc(type_s_unloc)     { x }

type_s_unloc:
| x=ident                 { Tref x }
| x=type_s c=container    { Tcontainer (x, c) }
| x=ident y=type_s %prec above_coll
                          { Tvset (x, y) }
| x=paren(type_r)         { x }

%inline type_tuples:
| xs=type_tuple+ { xs }

%inline type_tuple:
| MULT x=type_s { x }

%inline container:
| COLLECTION { Collection }
| QUEUE      { Queue }
| STACK      { Stack }
| SET        { Set }
| PARTITION  { Partition }

%inline pipe_idents:
| PIPE? xs=separated_nonempty_list(PIPE, ident) { xs }

%inline pipe_ident_options:
| PIPE? xs=separated_nonempty_list(PIPE, pipe_ident_option) { xs }

%inline pipe_ident_option:
| x=ident opts=state_options? { (x, opts) }

%inline state_options:
| xs=state_option+ { xs }

state_option:
| INITIAL                     { SOinitial }
| WITH xs=braced(named_items) { SOspecification xs }

asset:
| ASSET exts=extensions? ops=bracket(asset_operation)? x=ident opts=asset_options?
        fields=asset_fields?
                 apo=asset_post_options
          { Dasset (x, fields, opts, apo, ops, exts) }

asset_post_option:
| WITH STATES x=ident           { APOstates x }
| WITH xs=braced(named_items)   { APOconstraints xs }
| INITIALIZED BY e=simple_expr  { APOinit e }

%inline asset_post_options:
 | xs=asset_post_option* { xs }

%inline asset_fields:
| EQUAL fields=braced(fields) { fields }

%inline asset_options:
| xs=asset_option+ { xs }

asset_option:
| AS _x=ident           { AOasrole }
| IDENTIFIED BY x=ident { AOidentifiedby x }
| SORTED BY x=ident     { AOsortedby x }

%inline fields:
| xs=snl(SEMI_COLON, field) { xs }

field_r:
| x=ident exts=option(extensions)
      COLON y=type_t boption(REF)
          dv=default_value?
    { Ffield (x, y, dv, exts) }

%inline field:
| f=loc(field_r) { f }

%inline ident:
| x=loc(IDENT) { x }

action:
  ACTION exts=option(extensions) x=ident
    args=function_args xs=transitems_eq
      { let a, b = xs in Daction (x, args, a, b, exts) }

transition_to_item:
| TO x=ident y=condition_value? z=with_effect? { (x, y, z) }

%inline transitions:
 | xs=transition_to_item+ { xs }

on_value:
 | ON x=ident COLON y=ident { x, y }

transition:
  TRANSITION exts=option(extensions) x=ident
    args=function_args on=on_value? FROM f=expr EQUAL LBRACE xs=action_properties trs=transitions RBRACE
      { Dtransition (x, args, on, f, xs, trs, exts) }

%inline transitems_eq:
| { (dummy_action_properties, None) }
| EQUAL LBRACE xs=action_properties e=effect? RBRACE { (xs, e) }

action_properties:
  sp=loc(verification)? cb=calledby? cs=condition? fs=function_item*
  {
    {
      calledby      = cb;
      condition     = cs;
      functions     = fs;
      verif         = sp;
    }
  }

calledby:
 | CALLED BY exts=option(extensions) x=expr { (x, exts) }

condition:
 | CONDITION exts=option(extensions) xs=named_items
       { (xs, exts) }

%inline condition_value:
| WHEN x=expr { x }

%inline with_effect:
| WITH EFFECT x=expr { x }

invariant:
 | INVARIANT exts=option(extensions)
     id=ident xs=named_items
       { (id, xs, exts) }

%inline named_items:
 | xs=separated_nonempty_list(SEMI_COLON, named_item) { xs }

%inline named_item:
 | id=ident { let loc = loc id in (None, mkloc loc (Eterm (None, id))) }
 | id=label e=expr { (Some id, e) }

effect:
 | EFFECT exts=option(extensions) xs=loc(expr_extended) { (xs, exts) }

%inline function_return:
 | COLON ty=type_t { ty }

%inline function_args:
 |                   { [] }
 | xs=function_arg+  { xs }

%inline function_arg:
 | id=ident exts=option(extensions)
     { (id, None, exts) }

 | LPAREN id=ident exts=option(extensions)
     COLON ty=type_t RPAREN
       { (id, Some ty, exts) }

%inline assignment_value_operator:
 | EQUAL                  { ValueAssign }
 | op=assignment_operator { op }

%inline assignment_operator:
 | COLONEQUAL  { SimpleAssign }
 | PLUSEQUAL   { PlusAssign }
 | MINUSEQUAL  { MinusAssign }
 | MULTEQUAL   { MultAssign }
 | DIVEQUAL    { DivAssign }
 | ANDEQUAL    { AndAssign }
 | OREQUAL     { OrAssign }


expr_extended:
 | e1=expr SEMI_COLON e2=loc(expr_extended)
   { Eseq (e1, e2) }
 | e=expr_r { e }

qualid:
 | i=ident              { Qident i }
 | x=qualid DOT i=ident { Qdot (x, i) }

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

expr_r:
 | q=quantifier x=ident_typ1 COMMA y=expr
     { Equantifier (q, x, y)}

 | LET x=ident_typ1 EQUAL e=expr IN y=expr
     { Eletin (x, e, y) }

 | FUN xs=ident_typs EQUALGREATER x=expr
     { Efun (xs, x) }

 | ASSERT x=paren(expr)
     { Eassert x }

 | BREAK
     { Ebreak }

 | label=label? FOR LPAREN x=ident IN y=expr RPAREN body=expr %prec prec_for
     { Efor (x, y, body, label) }

 | IF c=expr THEN t=expr
     { Eif (c, t, None) }

 | IF c=expr THEN t=expr ELSE e=expr
     { Eif (c, t, Some e) }

 | MATCH x=expr WITH xs=branchs END { Ematchwith (x, xs) }

 | x=expr op=assignment_operator y=expr
     { Eassign (op, x, y) }

 | TRANSFER back=boption(BACK) x=expr y=ioption(to_value) %prec prec_transfer
     { Etransfer (x, back, y) }

 | x=order_operations %prec prec_order { x }

 | e1=expr op=loc(bin_operator) e2=expr
     { let loc = Location.make $startpos $endpos in
       Eapp ( mkloc loc (Eop (unloc op)), [e1; e2]) }

 | op=loc(un_operator) x=expr
     { let loc = Location.make $startpos $endpos in
       Eapp ( mkloc loc (Eop (unloc op)), [x]) }

 | x=simple_expr a=app_args
     { Eapp (x, a) }

 | x=simple_expr_r
     { x }

order_operation:
 | e1=expr op=loc(ord_operator) e2=expr
     { let loc = Location.make $startpos $endpos in
       Eapp ( mkloc loc (Eop (unloc op)), [e1; e2]) }

order_operations:
 | e=order_operation { e }
 | e1=loc(order_operations) op=loc(ord_operator) e2=expr
    { let loc = Location.make $startpos $endpos in
       Eapp ( mkloc loc (Eop (unloc op)), [e1; e2]) }

label:
 | id=ident COLON { id }

%inline app_args:
 | LPAREN RPAREN     { [] }
 | xs=simple_expr+   { xs }

%inline simple_expr:
 | x=loc(simple_expr_r) { x }

label_array:
 | COLON x=label { x }

simple_expr_r:
 | x=simple_expr DOT y=ident
     { Edot (x, y) }

 | xs=braced(separated_nonempty_list(SEMI_COLON, assign_field_r))
     { EassignFields xs }

 | LBRACKET l=label_array? xs=separated_list(COMMA, expr) RBRACKET
     { Earray (l, xs) }

 | x=literal
     { Eliteral x }

 | n=ident COLONCOLON x=ident
     { Eterm (Some n, x) }

 | x=ident
     { Eterm (None, x) }

 | x=paren(expr_extended)
     { x }

%inline ident_typs:
 | xs=ident+ COLON ty=type_t
     { List.map (fun x -> (x, Some ty, None)) xs }

 | xs=ident_typ+
     { List.flatten xs }

%inline ident_typ:
 | id=ident
     { [(id, None, None)] }

 | LPAREN ids=ident+ COLON ty=type_t RPAREN
     { List.map (fun id -> (id, Some ty, None)) ids }

%inline ident_typ1:
 | id=ident ty=option(COLON ty=type_t { ty })
     { (id, ty, None) }

literal:
 | x=NUMBER     { Lnumber   x }
 | x=RATIONAL   { let n, d = x in Lrational (n, d) }
 | x=TZ         { Ltz       x }
 | x=STRING     { Lstring   x }
 | x=ADDRESS    { Laddress  x }
 | x=bool_value { Lbool     x }
 | x=DURATION   { Lduration x }
 | x=DATE       { Ldate     x }

%inline bool_value:
 | TRUE  { true }
 | FALSE { false }

assign_field_r:
 | id=dot_ident op=assignment_value_operator e=expr
   { (op, id, e) }

%inline dot_ident:
 | x=ident DOT y=ident { (Some x, y) }
 | x=ident             { (None, x) }

%inline quantifier:
 | FORALL { Forall }
 | EXISTS { Exists }

%inline logical_operator:
 | AND   { And }
 | OR    { Or }
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
 | PLUS    { Plus }
 | MINUS   { Minus }
 | MULT    { Mult }
 | DIV     { Div }
 | PERCENT { Modulo }

%inline unary_operator:
 | PLUS    { Uplus }
 | MINUS   { Uminus }
 | NOT     { Not }

%inline bin_operator:
| op=logical_operator    { `Logical op }
| op=comparison_operator { `Cmp op }
| op=arithmetic_operator { `Arith op }

%inline un_operator:
| op=unary_operator      { `Unary op }

%inline ord_operator:
| op=ordering_operator   { `Cmp op }

%inline asset_operation_enum:
| AT_ADD    { AOadd }
| AT_REMOVE { AOremove }
| AT_UPDATE { AOupdate }

%inline asset_operation:
| xs=asset_operation_enum+ args=option(simple_expr+) { AssetOperation (xs, args) }
