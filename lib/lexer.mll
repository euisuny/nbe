{
  open Parser
}

let num = ['0'-'9'] ['0'-'9']*

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf } (* skip whitespace *)
  | "lam"  { LAM }
  | "app"  { APP }
  | "let"  { LET }
  | num as num { VAR (int_of_string num) }
  | ','    { COMMA }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | eof    { EOF }
