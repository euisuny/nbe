%{

open Debruijn


%}

%token LAM
%token APP
%token <int> VAR
%token LPAREN RPAREN
%token COMMA
%token EOF
%token LET


%start parse /* entry points */
%type <Debruijn.tm> parse
%type <Debruijn.tm> expr

%%

parse:
  | expr EOF { $1 }
;

expr:
  | VAR { Var($1) }
  | APP LPAREN expr COMMA expr RPAREN
      { App ($3, $5) }
  | LAM LPAREN expr RPAREN { Lam($3) }
  | LET LPAREN expr COMMA expr RPAREN
      { Let ($3, $5) }
;

%%

