type 'var lambda =
  | Abs of 'var * 'var lambda
  | App of 'var lambda * 'var lambda
  | Var of 'var

type 'var combinator =
  | CVar of 'var
  | CApp of 'var combinator * 'var combinator