open Syntax
open Syntax.Lambda
open Syntax.Combinator

let s2comb s =
  let rec aux = function
    | Var v -> CVar v
    | Abs _ -> assert false
    | App (m, n) -> CApp (aux m, aux n)
  in
  aux @@ Library.s2lam s

let rec scomb_to_lambda :
    string combinator -> [ `Com of combinators | `Str of string ] lambda =
  function
  | CVar "S" -> Var (`Com `S)
  | CVar "K" -> Var (`Com `K)
  | CVar "I" -> Var (`Com `I)
  | CVar v -> failwith ("Unsupported combinator: " ^ v)
  | CApp (m, n) -> App (scomb_to_lambda m, scomb_to_lambda n)

exception StepLimit

let jot2comb n =
  let v = n2bin n in
  List.fold_left
    (fun acc x ->
      if x = 0 then CApp (CApp (acc, CVar (`Com `S)), CVar (`Com `K))
      else CApp (CVar (`Com `S), CApp (CVar (`Com `K), acc)))
    (CVar (`Com `I))
    v

let reduce_comb m =
  let step = ref 0 in
  let rec aux m =
    if !step > 1000 then raise StepLimit;
    step := !step + 1;
    match m with
    | CApp (CVar (`Com `I), m) -> aux m
    | CApp (CApp (CVar (`Com `K), m), _) -> aux m
    | CApp (CApp (CApp (CVar (`Com `S), m), n), o) ->
        aux (CApp (CApp (m, o), CApp (n, o)))
    | CVar (`Com (`Jot x)) -> aux @@ jot2comb x
    | CVar _ as m -> m
    | CApp (m, n) as a ->
        let b = CApp (aux m, aux n) in
        if a = b then b else aux b
  in
  let _pp = Combinator.pp pp_ski_str in
  try
    let tm = aux m in
    (* Format.eprintf "Reduction: %a => %a\n" pp m pp tm ; *)
    Some tm
  with StepLimit -> (* Format.eprintf "Reduction Timeout: %a\n" pp m; *)
                    None

let rec size = function
  | Var _ -> 1
  | Abs (_, m) -> size m + 1
  | App (m, n) -> size m + size n + 1

(* #2進数で実装したほうがいいらしい
   #32bit固定のほうが楽らしい
   #true,false の組でやって、末尾をvにする *)

(* def n2lam(n,bl=$binlength)
   	if $ischurchnum then
   		return s2lam('s. z.' + '(s' * n + ' z' + ')' * n)
   	end
   	#s = 's. z.' + '(s' * n + ' z' + ')' * n
   	def rec(n,d,bl)
   		if d == bl then 'V'
   		elsif n % 2 > 0 then
   			'($cons $t ' + rec(n/2,d+1,bl) + ')'
   		else
   			'($cons $f ' + rec(n/2,d+1,bl) + ')'
   		end
   	end
   	s2lam(rec(n,0,bl))
   end *)
