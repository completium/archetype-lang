/* parser */
%{
  open Ident
  open Ast
%}
%token USE
%token MODEL
%token CONSTANT
%token ROLE
%token IDENTIFIED
%token BY
%token REF
%token ASSET
%token BEGIN_EXTENTION
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EQUAL
%token COLON
%token SEMI_COLON
%token <string> IDENT
%token <int> NUMBER
%token EOF
%start <model> main
%%

main: implementation EOF { $1 }

implementation: entities { Imodel $1 }

entities:
 | entity entities { $1 :: $2 }
 | entity          { [ $1 ] }

entity:
 | use      { $1 }
 | model    { $1 }
 | constant { $1 }
 | role     { $1 }
 | asset    { $1 }

use:
USE IDENT { Tuse $2 }

model:
MODEL IDENT { Tmodel $2 }

constant:
CONSTANT IDENT IDENT { Tconstant ($2, $3) }

role:
| ROLE extention IDENT { Trole $3 }
| ROLE IDENT           { Trole $2 }

asset:
| ASSET IDENT IDENTIFIED BY IDENT EQUAL LBRACE fields RBRACE { Tasset ($2, $8) }
| ASSET IDENT EQUAL LBRACE fields RBRACE { Tasset ($2, $5) }

fields:
| field fields { $1 :: $2 }
| field        { [ $1 ] }

field:
| IDENT COLON IDENT REF SEMI_COLON { Tfield ($1, $3) }
| IDENT COLON IDENT SEMI_COLON     { Tfield ($1, $3) }

extention:
BEGIN_EXTENTION idents RBRACKET { $2 }


idents:
 | IDENT idents { $1 :: $2 }
 | IDENT        { [ $1 ] }
