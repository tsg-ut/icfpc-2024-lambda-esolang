%{
  open Interpreter
%}

%token <string> IDENT COMBINATOR DOLLAR
%token <int> NUM
%token <char> PUTC
%token DOT LPAR RPAR QUEST ASTER EOF ARROW
%right ARROW

%start main
%type <string Interpreter.lambda> main
%%
main:
| expr EOF { $1 }
;
expr:
| IDENT DOT expr { Abs($1,$3) }
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
| IDENT { Var $1 }
| PUTC { Var ("." ^ String.make 1 $1) }
| COMBINATOR { Var $1 }
| ASTER NUM { n2charchnum $2 }
| DOLLAR {
    try
      List.assoc $1 (!library)
    with
      | Not_found -> failwith ("Undefined variable $" ^ $1)
  }
| LPAR expr RPAR { $2 }
;