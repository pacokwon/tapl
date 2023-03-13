{
open Parser
exception Eof
}

let word = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = word (word | digit)*

rule token = parse
    | [' ' '\t']    { token lexbuf }
    | "true"        { TRUE }
    | "false"       { FALSE }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    | "->"          { ARROW }
    | "bool"        { BOOL }
    | ':'           { COLON }
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | ['\n']        { EOL }
    | '\\'          { LAMBDA }
    | '.'           { DOT }
    | ident as lxm  { IDENT(lxm) }
    | eof           { raise Eof }
