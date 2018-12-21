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
%token ROLE
%token IDENTIFIED
%token BY
%token REF
%token ASSET
%token ENUM
%token STATES
%token ENSURE
%token TRANSITION
%token TRANSACTION
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
%token PLUSEQ
%token MINUSEQ
%token EOF

%token <string> IDENT
%token <int> NUMBER

%type <Ast.model> main

%start main

%%

main:
| impl=implementation EOF
    { impl }

| x=loc(error)
    { error ~loc:(loc x) PE_Unknown }

implementation:
| e=entities { Imodel e }

%inline entities:
| xs=declaration+ { xs }

declaration_r:
 | x=use      { x }
 | x=model    { x }
 | x=constant { x }
 | x=role     { x }
 | x=enum     { x }
 | x=states   { x }
 | x=asset    { x }

%inline declaration:
| e=loc(declaration_r) { e }

use:
| USE x=ident { Tuse x }

model:
| MODEL x=ident { Tmodel x }

constant:
| CONSTANT x=ident y=ident { Tconstant (x, y) }

role:
| ROLE _ext=option(extension) x=ident { Trole x }

enum:
| ENUM x=ident EQUAL xs=pipe_idents {Tenum (x, xs)}

states:
| STATES x=ident_equal? xs=pipe_idents {Tstates (x, xs)}

%inline ident_equal:
| x=ident EQUAL { x }

%inline pipe_idents:
| xs=pipe_ident+ { xs }

%inline pipe_ident:
| PIPE x=ident { x }

asset:
| ASSET x=ident _id=option(IDENTIFIED BY y=ident { y })
    EQUAL fields=braced(fields)
        { Tasset (x, fields) }

%inline fields:
| xs=field+ { xs }

field_r:
| x=ident COLON y=ident boption(REF) SEMI_COLON
    { Tfield (x, y) }

%inline field:
| f=loc(field_r) { f }

extension:
| BEGIN_EXTENSION ids=idents RBRACKET { ids }

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
