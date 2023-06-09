%{
open Syntax
%}

%token <string> IDENT
%token LAMBDA
%token IF THEN ELSE
%token LPAREN RPAREN
%token DOT COLON
%token TRUE FALSE
%token BOOL
%token ARROW
%right ARROW
%token EOL

%start main
%type <Syntax.typed_term> main
%%
main:
    t = term; EOL { t }
;

term:
    | at = appterm { at }
    | LAMBDA; id = IDENT; COLON; tp = typ; DOT; body = term { TypedAbs(id, tp, body) }
    | IF; cond = term; THEN; t = term; ELSE; f = term { TypedCond(cond, t, f) }
;

appterm:
    | at = aterm              { at }
    | at = appterm; a = aterm { TypedApp(at, a) }
;

aterm:
    | LPAREN; t = term; RPAREN  { t }
    | id = IDENT                { TypedVar(id) }
    | TRUE  { TypedTrue }
    | FALSE { TypedFalse }
;

typ:
    | BOOL                      { Bool }
    | t1 = typ; ARROW; t2 = typ { Arrow(t1, t2) }
;
