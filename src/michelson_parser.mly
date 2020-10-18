/* michelson parser */
%{
  open Michelson

  let emit_error loc =
    let str : string = "syntax error" in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

  let emit_error_msg loc msg =
    let str : string = "syntax error: " ^ msg in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

%}

%token <string> NUMBER
%token <string> STRING
%token <string> BYTES
%token <string> IDENT

%token <string> ANNOTATION

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMI_COLON
%token EOF

%type <Michelson.obj_micheline> main

%start main

%%

%inline paren(X):
| LPAREN x=X RPAREN { x }

%inline braced(X):
| LBRACE x=X RBRACE { x }

snl(separator, X):
  x = X { [ x ] }
| x = X; separator { [ x ] }
| x = X; separator; xs = snl(separator, X) { x :: xs }

main:
 | x=node EOF { x }

%inline annot:
| x=ANNOTATION { x }

%inline annots:
| xs=annot* { xs }

%inline aaa:
| xs=snl(SEMI_COLON, node) { xs }

%inline sequence:
| LBRACE RBRACE  { Oarray [] }
| xs=braced(aaa) { Oarray xs }

%inline rinteger:
| n=NUMBER { Oint n }

%inline rstring:
| s=STRING { Ostring s }

%inline rbytes:
| b=BYTES { Obytes b }

%inline prim:
| p=IDENT a=annots { Oprim {prim = p; args = []; annots = a} }

%inline arguments:
| xs=argument+ { xs }

%inline prim_app:
| p=IDENT a=annots args=arguments { Oprim {prim = p; args = args; annots = a} }

node:
| x=rinteger    { x }
| x=rstring     { x }
| x=rbytes      { x }
| x=prim        { x }
| x=prim_app    { x }
| x=sequence    { x }
| x=paren(node) { x }

argument:
| x=rinteger        { x }
| x=rstring         { x }
| x=rbytes          { x }
| x=prim            { x }
| x=paren(prim_app) { x }
| x=sequence        { x }
| x=paren(argument) { x }

