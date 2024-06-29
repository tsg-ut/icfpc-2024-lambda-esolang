%{
  open Syntax.Lambda

  let list2sum f = function
    | [] -> assert false
    | x :: xs -> List.fold_left f x xs
  
  let list2Apps = list2sum (fun m n -> App(m,n))
%}

%token <string> STR UOP BOP DOLLAR
%token <Z.t> INT
%token <int> LABS LVAR IDENT
%token TRUE FALSE EOF BAPP IF DOT LPAR RPAR


%start main
%type < Syntax.Icfpc.t Syntax.Lambda.lambda > main
%%
main:
| expr EOF { $1 }
;
expr:
| IDENT DOT expr { Abs(`Fv $1,$3) }
| term_list { list2Apps $1 }
;
term_list:
| term term_list { $1 :: $2 }
| term { [$1] }
;
term:
| IDENT { Var (`Fv $1) }
| DOLLAR { Var (`Str("$" ^ $1)) }
| LPAR expr RPAR { $2 }
| TRUE { Var(`Bool true) }
| FALSE { Var(`Bool false) }
| STR { Var(`Str $1) }
| INT { Var(`Int $1) }
| LABS term { Abs(`Fv $1, $2)}
| LVAR { Var(`Fv $1) }
| UOP term { App(Var(`Uop $1),$2) }
| BOP term term { App(App(Var(`Bop $1),$2),$3) }
| BAPP term term { App($2,$3) }
| IF term term term { App(App(App(Var(`Top "IF"),$2),$3),$4) }
;
