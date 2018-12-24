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
%token BY
%token AS
%token FROM
%token TO
%token REF
%token ASSET
%token ASSERT
%token ENUM
%token STATES
%token INITIAL
%token ENSURE
%token TRANSITION
%token TRANSACTION
%token ARGS
%token CALLED
%token CONDITION
%token ACTION
%token LET
%token IF
%token ELSE
%token FOR
%token IN
%token LPAREN
%token RPAREN
%token BEGIN_EXTENSION
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EQUAL
%token COMMA
%token COLON
%token SEMI_COLON
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

%type <Ast.model> main

%left DOT

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
| impl=implementation EOF
    { impl }

| x=loc(error)
    { error ~loc:(loc x) PE_Unknown }

implementation:
| x=declarations { Mmodel x }

%inline declarations:
| xs=declaration+ { xs }

declaration_r:
 | x=use         { x }
 | x=model       { x }
 | x=constant    { x }
 | x=value       { x }
 | x=role        { x }
 | x=enum        { x }
 | x=states      { x }
 | x=dassert     { x }
 | x=asset       { x }
 | x=transition  { x }
 | x=transaction { x }

%inline declaration:
| e=loc(declaration_r) { e }

use:
| USE x=ident { Duse x }

model:
| MODEL x=ident { Dmodel x }

constant:
| CONSTANT exts=option(extensions) x=ident y=ident { Dconstant (x, y, exts) }

value:
| VALUE x=ident exts=option(extensions) y=ident { Dvalue (x, y, exts) }

role:
| ROLE exts=option(extensions) x=ident dv=default_value? { Drole (x, dv, exts) }

%inline default_value:
| EQUAL x=expr { x }

%inline from_value:
| FROM x=ident { x }

%inline to_value:
| TO x=ident { x }

%inline extensions:
| xs=extension+ { xs }

%inline extension:
| e=loc(extension_r) { e }

extension_r:
| BEGIN_EXTENSION x=ident xs=option(exprs) RBRACKET { Eextension (x, xs) }

enum:
| ENUM x=ident EQUAL xs=pipe_idents {Denum (x, xs)}

states:
| STATES x=ident_equal? xs=pipe_ident_options {Dstates (x, xs)}

dassert:
| ASSERT x=paren(expr) { Dassert x }

%inline ident_t:
| e=loc(ident_r) { e }

%inline ident_r:
 | x=ident DOT y=ident { Idouble (x, y) }
 | x=ident             { Isimple x }

%inline ident_equal:
| x=ident EQUAL { x }

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
      EQUAL fields=braced(fields)
          { Dasset (x, fields, opts) }

%inline asset_options:
| xs=asset_option+ { xs }

asset_option:
| AS ROLE               { AOasrole }
| IDENTIFIED BY x=ident { AOidentifiedby x }

%inline fields:
| xs=field+ { xs }

field_r:
| x=ident COLON y=ident boption(REF) SEMI_COLON
    { Ffield (x, y) }

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
  TRANSITION x=ident FROM y=ident TO z=ident
    EQUAL xs=braced(transitems) { Dtransition (x, y, z, xs) }

transaction:
    TRANSACTION x=ident EQUAL xs=braced(transitems) { Dtransaction (x, xs) }

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
 | ARGS EQUAL fields=braced(fields) { Targs fields }

calledby:
 | CALLED BY exts=option(extensions) x=expr SEMI_COLON { Tcalledby (x, exts) }

ensure:
 | ENSURE COLON x=expr SEMI_COLON { Tensure x }

condition:
 | CONDITION COLON x=expr SEMI_COLON { Tcondition x }

transition_item:
 | TRANSITION FROM x=ident TO y=ident SEMI_COLON { Ttransition (x, y) }

action:
 | ACTION COLON xs=code SEMI_COLON { Taction xs }

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
 | x=assign_instr { x }
 | x=letin_instr  { x }
 | x=if_instr     { x }
 | x=for_instr    { x }
 | x=call_instr   { x }
 | x=assert_instr { x }

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
 | x=paren(expr_r)      { x }
 | x=logical_expr       { x }
 | x=comparison_expr    { x }
 | x=arithmetic_expr    { x }
 | x=array_expr         { x }
 | x=dot_expr           { x }
 | x=assign_fields      { x }
 | x=literal_expr       { x }
 | x=quantifier_expr    { x }
 | x=term               { x }

letin_instr:
 | LET x=ident EQUAL e=expr IN b=braced(instrs) { Iletin (x, e, b) }

if_instr:
 | IF c=paren(expr) t=braced(instrs) e=else_instr? { Iif (c, t, e) }

%inline else_instr:
 | ELSE x=braced(instrs) { x }

for_instr:
 | FOR LPAREN x=ident IN y=expr RPAREN body=braced(instrs) { Ifor (x, y, body) }

call_instr:
 | x=loc(term) { Icall x }

assert_instr:
 | ASSERT x=paren(expr) { Iassert x }

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
 | x=STRING { Lstring x }

term:
 | x=ident_t xs=exprs? { Eterm (x, xs) }

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

%inline comma_exprs:
 | xs=comma_expr+ { xs }

%inline comma_expr:
 | COMMA x=expr { x }

dot_expr:
 | x=expr DOT y=expr { Edot (x, y) }

assign_fields:
 | xs=braced(assign_fieldss) { EassignFields xs }

%inline assign_fieldss:
 | xs=assign_field+ { xs }

%inline assign_field:
 | id=ident_t op=assignment_operator e=expr SEMI_COLON { AassignField (op, id, e) }
