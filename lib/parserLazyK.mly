%{
  open Syntax.Combinator
  open Syntax.Combinators

  let list2sum f = function
    | [] -> assert false
    | x :: xs -> List.fold_left f x xs
  
  let list2CApps = list2sum (fun m n -> CApp(m,n))

%}

%token <string> COMBINATOR
%token LPAR RPAR ASTER EOF GRAVE IOTA

%start main
%type < Syntax.Combinators.com_str Syntax.Combinator.combinator > main
%%

main:
| lazyKExpr_list EOF { list2CApps $1 }
;
lazyKExpr_list:
| lazyKExpr lazyKExpr_list { $1 :: $2 }
| lazyKExpr { [$1] }
;
lazyKExpr:
| COMBINATOR { CVar (`Com (str_to_combinators $1)) }
| GRAVE lazyKExpr lazyKExpr { CApp($2,$3) }
| ASTER IOTA lazyKExpr { CApp(CVar(`Com `Iota),$3) }
| ASTER lazyKExpr IOTA { CApp($2, CVar(`Com `Iota)) }
| LPAR lazyKExpr_list RPAR { list2CApps $2 }
;