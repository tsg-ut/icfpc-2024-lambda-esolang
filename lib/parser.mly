%{
  open Syntax.Lambda
  open Syntax.Combinators
%}

%token <string> IDENT COMBINATOR DOLLAR
%token <int> NUM
%token <char> PUTC
%token DOT LPAR RPAR QUEST ASTER EOF ARROW
%right ARROW

%start main
%type < Syntax.Combinators.com_str Syntax.Lambda.lambda > main
%%
main:
| expr EOF { $1 }
;
expr:
| IDENT DOT expr { Abs(`Str $1,$3) }
| term_list {
    match $1 with
    | [] -> assert false
    | x :: xs -> List.fold_left (fun m n -> App(m,n)) x xs
  }
;
term_list:
| term term_list { $1 :: $2 }
| term { [$1] }
;
term:
| IDENT { Var (`Str $1) }
| PUTC { Var (`Str ("." ^ String.make 1 $1)) }
| COMBINATOR { Var (`Com (str_to_combinators $1)) }
| ASTER NUM { Var (`Str("*" ^ string_of_int $2)) }
| DOLLAR { Var (`Str("$" ^ $1)) }
| LPAR expr RPAR { $2 }
;