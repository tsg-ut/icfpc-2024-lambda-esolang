%{
  open Syntax.Lambda

%}

%token <string> STR UOP BOP
%token <Z.t> INT
%token <int> LABS LVAR
%token TRUE FALSE EOF BAPP IF


%start main
%type < Syntax.Icfpc.t Syntax.Lambda.lambda > main
%%
main:
| expr EOF { $1 }
;
expr:
| TRUE { Var(`Bool true) }
| FALSE { Var(`Bool false) }
| STR { Var(`Str $1) }
| INT { Var(`Int $1) }
| LABS expr { Abs(`Fv $1, $2)}
| LVAR { Var(`Fv $1) }
| UOP expr { App(Var(`Uop $1),$2) }
| BOP expr expr { App(App(Var(`Bop $1),$2),$3) }
| BAPP expr expr { App($2,$3) }
| IF expr expr expr { App(App(App(Var(`Top "IF"),$2),$3),$4) }
;