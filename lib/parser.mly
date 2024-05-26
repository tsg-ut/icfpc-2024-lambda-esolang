%{
  open Syntax.Lambda
  open Syntax.Combinators

  let list2sum f = function
    | [] -> assert false
    | x :: xs -> List.fold_left f x xs
  
  let list2Apps = list2sum (fun m n -> App(m,n))

%}

%token <string> IDENT COMBINATOR DOLLAR
%token <int> NUM
%token <char> PUTC
%token DOT LPAR RPAR QUEST ASTER HASH EOF ARROW LBRACE RBRACE
%right ARROW

%start main
%type < Syntax.Combinators.com_str Syntax.Lambda.lambda > main
%%
main:
| expr EOF { $1 }
;
expr:
| IDENT DOT expr { Abs(`Str $1,$3) }
| term_list { list2Apps $1 }
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
| HASH NUM { Var (`Str("#" ^ string_of_int $2)) }
| DOLLAR { Var (`Str("$" ^ $1)) }
| LPAR expr RPAR { $2 }
| LBRACE expr RBRACE { App(Var(`Str "optimize"),$2) }
;