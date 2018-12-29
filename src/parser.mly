/* parser */
%{
  open Ast
  open Location
  open Parseutils

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
%token SUBSET
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
%token <int> NUMBER
%token <float> FLOAT

%type <Ast.model> main

%left AND
%left OR
%right IMPLY
%left EQUIV

%left NEQUAL
%left GREATER
%left GREATEREQUAL
%left LESS
%left LESSEQUAL

%left PLUS
%left MINUS
%left MULT
%left DIV

%start main


%%

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
| EQUAL x=expr { x }

%inline from_value:
| FROM x=expr { x }

%inline to_value:
| TO x=expr { x }

dextension:
|PERCENT x=ident xs=option(exprs) { Dextension (x, xs) }

%inline extensions:
| xs=extension+ { xs }

%inline extension:
| e=loc(extension_r) { e }

extension_r:
| LBRACKETPERCENT x=ident xs=option(exprs) RBRACKET { Eextension (x, xs) }

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

%inline type_r:
| x=ident c=container { Tcontainer (x, c)}
| x=ident             { Tref x }

container:
| COLLECTION { Collection }
| QUEUE      { Queue }
| STACK      { Stack }
| SET        { Set }
| SUBSET     { Subset }
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
| INITIALIZED BY x=braced(code) { x }

asset_option:
| AS ROLE               { AOasrole }
| IDENTIFIED BY x=ident { AOidentifiedby x }
| SORTED BY x=ident     { AOsortedby x }

%inline fields:
| xs=field+ { xs }

field_r:
| x=ident exts=option(extensions) COLON y=type_t boption(REF) SEMI_COLON
    { Ffield (x, y, exts) }

%inline field:
| f=loc(field_r) { f }

%inline ident:
| x=loc(IDENT) { x }

%inline idents:
| xs=ident+ { xs }

%inline paren(X):
| LPAREN x=X RPAREN { x }

%inline braced(X):
| LBRACE x=X RBRACE { x }

%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos; }
  }

transition:
  TRANSITION exts=option(extensions) x=ident
    y=from_value z=to_value
      EQUAL xs=braced(transitems)
        { Dtransition (x, y, z, xs, exts) }

transaction:
  TRANSACTION exts=option(extensions) x=ident
    EQUAL xs=braced(transitems)
      { Dtransaction (x, xs, exts) }

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
 | ARGS EQUAL fields=braced(fields) { Targs fields }

calledby:
 | CALLED BY exts=option(extensions) x=expr SEMI_COLON { Tcalledby (x, exts) }

ensure:
 | ENSURE COLON x=expr SEMI_COLON { Tensure x }

condition:
 | CONDITION COLON x=expr SEMI_COLON { Tcondition x }

transferred:
 | TRANSFERRED COLON x=expr SEMI_COLON { Ttransferred x }

transition_item:
 | TRANSITION id=expr?
     x=from_value
         y=to_value SEMI_COLON
             { Ttransition (x, y, id) }

action:
 | ACTION COLON xs=code SEMI_COLON { Taction xs }

%inline bcode:
 | xs=braced(code)  { xs }
/* | x=instr          { [x] }*/

%inline code:
 | x=instr xs=comma_instr+ { x::xs }
 | x=instr { [x] }

%inline comma_instr:
 | COMMA x=instr { x }


%inline instrs:
 | x=instr xs=instr+ { x :: xs }
 | x=instr { [x] }

%inline instr:
 | e=loc(instr_r) { e }

instr_r:
 | x=assign_instr     { x }
 | x=letin_instr      { x }
 | x=if_instr         { x }
 | x=for_instr        { x }
 | x=transfer_instr   { x }
 | x=transition_instr { x }
 | x=call_instr       { x }
 | x=assert_instr     { x }
 | x=break_instr      { x }

assign_instr:
 | x=expr op=assignment_operator y=expr { Iassign (op, x, y) }

%inline assignment_operator:
 | COLONEQUAL { Assign }
 | PLUSEQUAL  { PlusAssign }
 | MINUSEQUAL { MinusAssign }
 | MULTEQUAL  { MultAssign }
 | DIVEQUAL   { DivAssign }
 | ANDEQUAL   { AndAssign }
 | OREQUAL    { OrAssign }

expr_r:
 | x=logical_expr       { x }
 | x=comparison_expr    { x }
 | x=arithmetic_expr    { x }
 | x=array_expr         { x }
 | x=namespace_expr     { x }
 | x=dot_expr           { x }
 | x=fun_expr           { x }
 | x=assign_fields      { x }
 | x=literal_expr       { x }
 | x=quantifier_expr    { x }
 | x=term               { x }
 | x=call_expr          { x }
 | x=paren(expr_r)      { x }

letin_instr:
 | LET x=ident EQUAL e=expr IN b=code { Iletin (x, e, b) }

if_instr:
 | IF c=expr THEN t=bcode e=else_instr? { Iif (c, t, e) }

%inline else_instr:
 | ELSE x=bcode { x }

for_instr:
 | FOR LPAREN x=ident IN y=expr RPAREN body=bcode { Ifor (x, y, body) }

transfer_instr:
 | TRANSFER _b=option(BACK) x=expr y=to_value? { Itransfer (x, None, y) }

transition_instr:
 | TRANSITION TO x=expr { Itransition x }

call_instr:
 | x=loc(call_expr) { Icall x }

assert_instr:
 | ASSERT x=paren(expr) { Iassert x }

break_instr:
 | BREAK { Ibreak }

quantifier_expr:
 | q=quantifier x=ident COLON y=expr COMMA z=expr {Equantifier (q, x, y, z)}

%inline quantifier:
 | FORALL { Forall }
 | EXISTS { Exists }

%inline exprs:
 | xs=expr+ { xs }

%inline expr:
 | e=loc(expr_r) { e }

literal_expr:
 | x=literal { Eliteral x }

literal:
 | x=NUMBER { Lnumber x }
 | x=FLOAT  { Lfloat x }
 | x=STRING { Lstring x }

term:
 | x=ident { Eterm x }

lterm:
 | x=loc(term) { x }

logical_expr:
 | x=expr op=logical_operator y=expr { Elogical (op, x, y) }
 | NOT x=expr                        { Enot x }

%inline logical_operator:
 | AND   { And }
 | OR    { Or }
 | IMPLY { Imply }
 | EQUIV { Or }

arithmetic_expr:
 | x=expr op=arithmetic_operator y=expr { Earithmetic (op, x, y) }

comparison_expr:
 | x=expr op=comparison_operator y=expr { Ecomparison (op, x, y) }

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

array_expr:
 | LBRACKET xs=sep_comma_exprs RBRACKET { Earray xs }
 | LBRACKET RBRACKET                    { Earray [] }

%inline sep_comma_exprs:
 | x=expr xs=comma_expr+ { x :: xs }
 | x=expr { [x] }

%inline comma_expr:
 | COMMA x=expr { x }

fun_expr:
 | FUN ids=idents EQUALGREATER x=expr { Efun (ids, x) }

call_expr:
 | x=call_e xs=call_args { Ecall (x, xs) }

 %inline call_args:
 | LPAREN RPAREN { [] }
/* | x=expr        { [x] }*/
 | xs=exprs      { xs }

%inline call_e:
 | x=lterm         { x }
 | x=loc(dot_expr) { x }
 | x=paren(expr)   { x }

namespace_expr:
 | id=ident COLONCOLON x=expr   { Enamespace (id, x) }

dot_expr:
 | x=dot_expr2 DOT y=lterm      { Edot (x, y) }

dot_expr2:
 | x=loc(dot_expr3)    { x }
 | x=simple_expr       { x }

%inline dot_expr3:
 | x=dot_expr2 DOT y=simple_expr { Edot (x, y) }

simple_expr:
 | x=loc(simple_call)    { x }
 | x=lterm               { x }
 | x=paren(expr)         { x }

%inline simple_call:
 | x=lterm arg=zero_or_one_arg   { Ecall (x, arg) }

%inline zero_or_one_arg:
 | LPAREN RPAREN { []  }
 | x=paren(expr) { [x] }

assign_fields:
 | xs=braced(assign_fieldss) { EassignFields xs }

%inline assign_fieldss:
 | xs=assign_field+ { xs }

%inline assign_field:
 | id=expr op=assignment_operator e=expr SEMI_COLON { AassignField (op, id, e) }
