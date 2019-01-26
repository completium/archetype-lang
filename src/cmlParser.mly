/* parser */
%{
  open CmlParseTree
  open CmlLocation
  open CmlParseUtils

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
%token TRANSFERRED
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

%nonassoc FROM TO IN EQUALGREATER
%right THEN ELSE

%left COMMA

%nonassoc COLONEQUAL PLUSEQUAL MINUSEQUAL MULTEQUAL DIVEQUAL ANDEQUAL OREQUAL

%right IMPLY
%nonassoc EQUIV

%left OR
%left AND

%nonassoc EQUAL NEQUAL
%nonassoc GREATER GREATEREQUAL LESS LESSEQUAL

%left PLUS MINUS
%left MULT DIV

%right NOT

%type <CmlParseTree.model> main

%start main

%%

%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = CmlLocation.make $startpos $endpos; }
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

enum:
| ENUM x=ident EQUAL xs=pipe_idents {Denum (x, xs)}

states:
| STATES x=ident_equal? xs=pipe_ident_options {Dstates (x, xs)}

assert_decl:
| ASSERT x=paren(expr) { Dassert x }

object_decl:
| OBJECT exts=extensions? x=ident y=expr { Dobject (x, y, exts) }

key_decl:
| KEY exts=extensions? x=ident OF y=expr { Dkey (x, y, exts) }

%inline ident_equal:
 | x=ident EQUAL { x }

%inline type_t:
| e=loc(type_r) { e }

type_r:
| x=ident                     { Tref x }
| x=type_t c=container        { Tcontainer (x, c) }
| x=ident y=type_t            { Tvset (x, y) }
| x=type_t IMPLY y=type_t     { Tapp (x, y) }
/*| xs=separated_nonempty_list(MULT, type_t2) { Tnuplet (xs) }*/
| x=paren(type_r)             { x }

container:
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
 | x=transferred      { x }
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

transferred:
 | TRANSFERRED exts=option(extensions) COLON x=expr SEMI_COLON { Ttransferred (x, exts) }

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
 | q=quantifier x=ident_typ COMMA y=expr
     { Equantifier (q, x, y)}

 | LET x=ident_typ EQUAL e=expr IN y=expr
     { Eletin (x, e, y) }

 | FUN xs=ident_typs EQUALGREATER x=expr
     { Efun (xs, x) }

 | e1=expr COMMA e2=expr
     { Eseq (e1, e2) }

 | ASSERT x=paren(expr)
     { Eassert x }

 | FOR LPAREN x=ident IN y=expr RPAREN is=invariants? body=expr
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

 | TRANSFER back=boption(BACK) x=expr y=to_value?
     { Etransfer (x, back, y) }

 | e1=expr op=loc(bin_operator) e2=expr
     { Eapp ( mkloc (loc op) (Eop (unloc op)), [e1; e2]) }

 | op=loc(un_operator) x=expr
     { Eapp ( mkloc (loc op) (Eop (unloc op)), [x]) }

 | x=simple_with_app_expr_r
     { x }

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
 | xs=ident_typ+     { xs }

%inline ident_typ:
 | id=ident x=with_type?
     { (id, x) }

%inline with_type:
 | COLON x=type_t { x }

literal:
 | x=NUMBER     { Lnumber  x }
 | x=FLOAT      { Lfloat   x }
 | x=STRING     { Lstring  x }
 | x=ADDRESS    { Laddress x }
 | x=bool_value { Lbool    x }

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
 | GREATER      { Gt }
 | GREATEREQUAL { Ge }
 | LESS         { Lt }
 | LESSEQUAL    { Le }

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
