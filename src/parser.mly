/* parser */
%{
  open ParseTree
  open Location
  open ParseUtils

  let error ?loc code = raise (ParseError (loc, code))
%}

%token USE
%token MODEL
%token CONSTANT
%token VALUE
%token ROLE
%token IDENTIFIED
%token SORTED
%token BY
%token AS
%token FROM
%token TO
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
%token WITH
%token ASSERT
%token OBJECT
%token KEY
%token OF
%token ENUM
%token STATES
%token INITIAL
%token ENSURE
%token INVARIANT
%token TRANSITION
%token TRANSACTION
%token ARGS
%token CALLED
%token CONDITION
%token ACTION
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
%token EOF

%token <string> IDENT
%token <string> STRING
%token <Big_int.big_int> NUMBER
%token <float> FLOAT
%token <string> ADDRESS
%token <string> DURATION
%token <string> DATE

%nonassoc prec_decl
%nonassoc prec_for prec_transfer

%nonassoc FROM TO IN EQUALGREATER
%right THEN ELSE

%left COMMA

%nonassoc COLONEQUAL PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL ANDEQUAL OREQUAL

%right IMPLY
%nonassoc EQUIV

%left OR
%left AND

%nonassoc EQUAL NEQUAL
%nonassoc prec_order
%nonassoc GREATER GREATEREQUAL LESS LESSEQUAL

%left PLUS MINUS
%left MULT DIV PERCENT

%right NOT

%right STACK SET QUEUE PARTITION COLLECTION
%nonassoc above_coll

%type <ParseTree.model> main

%start main

%%

%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos; }
  }

%inline paren(X):
| LPAREN x=X RPAREN { x }

%inline braced(X):
| LBRACE x=X RBRACE { x }

%inline bracket(X):
| LBRACKET x=X RBRACKET { x }

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
 | x=use         { x }
 | x=model       { x }
 | x=constant    { x }
 | x=value       { x }
 | x=role        { x }
 | x=enum        { x }
 | x=states      { x }
 | x=assert_decl { x }
 | x=object_decl { x }
 | x=key_decl    { x }
 | x=asset       { x }
 | x=transition  { x }
 | x=transaction { x }
 | x=dextension  { x }
 | x=namespace   { x }
 | x=contract    { x }

use:
| USE x=ident { Duse x }

model:
| MODEL x=ident { Dmodel x }

constant:
| CONSTANT exts=option(extensions) x=ident y=ident { Dconstant (x, y, exts) }

value:
| VALUE exts=extensions? x=ident y=ident z=option(value_options)
    dv=default_value?
      { Dvalue (x, y, z, dv, exts) }

%inline value_options:
| xs=value_option+ { xs }

value_option:
| x=from_value { VOfrom x }
| x=to_value   { VOto x }

role:
| ROLE exts=option(extensions) x=ident dv=default_value? { Drole (x, dv, exts) }

%inline default_value:
| COLONEQUAL x=expr { x }

%inline from_value:
| FROM x=expr { x }

%inline to_value:
| TO x=expr { x }

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
| TRANSACTION x=ident COLON xs=types SEMI_COLON { Ssignature (x, xs) }

enum:
| ENUM x=ident EQUAL xs=pipe_idents {Denum (x, xs)}

states:
| STATES x=ident_equal? xs=pipe_ident_options {Dstates (x, xs)}

assert_decl:
| ASSERT x=paren(expr) { Dassert x }

object_decl:
| OBJECT exts=extensions? x=ident y=expr { Dobject (x, y, exts) } %prec prec_decl

key_decl:
| KEY exts=extensions? x=ident OF y=expr { Dkey (x, y, exts) } %prec prec_decl

%inline ident_equal:
| x=ident EQUAL { x }

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
| xs=pipe_ident+ { xs }

%inline pipe_ident:
| PIPE x=ident { x }

%inline pipe_ident_options:
| xs=pipe_ident_option+ { xs }

%inline pipe_ident_option:
| PIPE x=ident opts=state_options? { (x, opts) }

%inline state_options:
| xs=state_option+ { xs }

state_option:
| INITIAL { SOinitial }

asset:
| ASSET x=ident opts=asset_options?
        fields=asset_fields?
            cs=asset_constraints?
                init=init_asset?
          { Dasset (x, fields, cs, opts, init) }

%inline asset_constraints:
 | xs=asset_constraint+ { xs }

%inline asset_constraint:
 | WITH x=braced(expr) { x }

%inline asset_fields:
| EQUAL fields=braced(fields) { fields}

%inline asset_options:
| xs=asset_option+ { xs }

%inline init_asset:
| INITIALIZED BY x=braced(expr) { x }

asset_option:
| AS ROLE               { AOasrole }
| IDENTIFIED BY x=ident { AOidentifiedby x }
| SORTED BY x=ident     { AOsortedby x }

%inline fields:
| xs=field+ { xs }

field_r:
| x=ident exts=option(extensions)
      COLON y=type_t boption(REF)
          dv=default_value? SEMI_COLON
    { Ffield (x, y, dv, exts) }

%inline field:
| f=loc(field_r) { f }

%inline ident:
| x=loc(IDENT) { x }

transition:
  TRANSITION exts=option(extensions) x=ident
    y=from_value z=to_value
      xs=transitems_eq
        { Dtransition (x, y, z, xs, exts) }

transaction:
  TRANSACTION exts=option(extensions) x=ident
    xs=transitems_eq
      { Dtransaction (x, xs, exts) }

%inline transitems_eq:
| { [] }
| EQUAL xs=braced(transitems) { xs }

%inline transitems:
 | xs=transitem+ { xs }

%inline transitem:
 | e=loc(transitem_r) { e }

transitem_r:
 | x=args             { x }
 | x=calledby         { x }
 | x=ensure           { x }
 | x=condition        { x }
 | x=transition_item  { x }
 | x=action           { x }

args:
 | ARGS exts=option(extensions) EQUAL fields=braced(fields) { Targs (fields, exts) }

calledby:
 | CALLED BY exts=option(extensions) x=expr SEMI_COLON { Tcalledby (x, exts) }

ensure:
 | ENSURE exts=option(extensions) COLON x=expr SEMI_COLON { Tensure (x, exts) }

condition:
 | CONDITION exts=option(extensions) COLON x=expr SEMI_COLON { Tcondition (x, exts) }

transition_item:
 | TRANSITION id=expr?
     exts=option(extensions)
     x=from_value
         y=to_value SEMI_COLON
             { Ttransition (x, y, id, exts) }

action:
 | ACTION exts=option(extensions) COLON xs=expr SEMI_COLON { Taction (xs, exts) }

%inline assignment_operator:
 | COLONEQUAL { SimpleAssign }
 | PLUSEQUAL  { PlusAssign }
 | MINUSEQUAL { MinusAssign }
 | MULTEQUAL  { MultAssign }
 | DIVEQUAL   { DivAssign }
 | ANDEQUAL   { AndAssign }
 | OREQUAL    { OrAssign }

%inline expr:
 | e=loc(expr_r) { e }

expr_r:
 | q=quantifier x=ident_typ1 COMMA y=expr
     { Equantifier (q, x, y)}

 | LET x=ident_typ1 EQUAL e=expr IN y=expr
     { Eletin (x, e, y) }

 | FUN xs=ident_typs EQUALGREATER x=expr
     { Efun (xs, x) }

 | e1=expr COMMA e2=expr
     { Eseq (e1, e2) }

 | ASSERT x=paren(expr)
     { Eassert x }

 | FOR LPAREN x=ident IN y=expr RPAREN is=invariants? body=expr %prec prec_for
     { Efor (x, y, body, is) }

 | BREAK
     { Ebreak }

 | IF c=expr THEN t=expr
     { Eif (c, t, None) }

 | IF c=expr THEN t=expr ELSE e=expr
     { Eif (c, t, Some e) }

 | x=expr op=assignment_operator y=expr
     { Eassign (op, x, y) }

 | TRANSITION TO x=expr
     { Etransition x }

 | TRANSFER back=boption(BACK) x=expr y=ioption(to_value) %prec prec_transfer
     { Etransfer (x, back, y) }

 | x=order_operations %prec prec_order { x }

 | e1=expr op=loc(bin_operator) e2=expr
     { let loc = Location.make $startpos $endpos in
       Eapp ( mkloc loc (Eop (unloc op)), [e1; e2]) }

 | op=loc(un_operator) x=expr
     { let loc = Location.make $startpos $endpos in
       Eapp ( mkloc loc (Eop (unloc op)), [x]) }

 | x=simple_with_app_expr_r
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

%inline invariants:
 | xs=invariant+   { xs }

%inline invariant:
 | INVARIANT x=paren(expr){ x }

%inline simple_with_app_expr_r:
 | x=simple_expr a=app_args
     { Eapp (x, a) }

 | x=simple_expr_r
     { x }

%inline app_args:
 | LPAREN RPAREN     { [] }
 | xs=simple_expr+   { xs }


%inline simple_expr:
 | x=loc(simple_expr_r) { x }

simple_expr_r:
 | x=simple_expr DOT y=ident
     { Edot (x, y) }

 | xs=braced(assign_field_r+)
     { EassignFields xs }

 | x=bracket(separated_list(COMMA, simple_expr))
     { Earray x }

 | x=literal
     { Eliteral x }

 | n=ident COLONCOLON x=ident
     { Eterm (Some n, x) }

 | x=ident
     { Eterm (None, x) }

 | x=paren(expr_r)
     { x }

%inline ident_typs:
 | xs=ident+ COLON ty=type_t
     { List.map (fun x -> (x, Some ty)) xs }

 | xs=ident_typ+
     { List.flatten xs }

%inline ident_typ:
 | id=ident
     { [(id, None)] }

 | LPAREN ids=ident+ COLON ty=type_t RPAREN
     { List.map (fun id -> (id, Some ty)) ids }

%inline ident_typ1:
 | id=ident ty=option(COLON ty=type_t { ty })
     { (id, ty) }

literal:
 | x=NUMBER     { Lnumber  x }
 | x=FLOAT      { Lfloat   x }
 | x=STRING     { Lstring  x }
 | x=ADDRESS    { Laddress x }
 | x=bool_value { Lbool    x }
 | x=DURATION   { Lduration x }
 | x=DATE       { Ldate x }

%inline bool_value:
 | TRUE  { true }
 | FALSE { false }

assign_field_r:
 | id=dot_ident op=assignment_operator e=expr SEMI_COLON
   { AassignField (op, id, e) }

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
