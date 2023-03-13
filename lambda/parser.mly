%{
open Syntax
%}

%token <string> IDENT
%token LAMBDA
%token LPAREN RPAREN
%token DOT
%token EOL

%start main
%type <Syntax.term> main
%%
main:
    t = term; EOL { t }
;

term:
    | at = appterm                      { at }
    | LAMBDA; id = IDENT; DOT; t = term { TmAbs(id, t) }
;

appterm:
    | at = aterm              { at }
    | at = appterm; a = aterm { TmApp(at, a) }
;

aterm:
    | LPAREN; t = term; RPAREN  { t }
    | id = IDENT                { TmVar(id) }
;
