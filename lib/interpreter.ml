type var = int

type 'var lambda =
  | Abs of 'var * 'var lambda
  | App of 'var lambda * 'var lambda
  | Var of 'var

type 'var combinator =
  | CVar of 'var
  | CApp of 'var combinator * 'var combinator

(* let rec combinator_to_str comb =
   let Comb(v,cs) = comb in
   match cs with
   | [] -> v
   | [c] -> "`" ^ v ^ combinator_to_str c
   | _ :: _ :: _ -> "(" ^ v ^ String.concat "" (List.map combinator_to_str cs) ^ ")" *)

let pp_var fmt v = Format.fprintf fmt "%s" v

let pp_lambda pp_var =
  let rec aux fmt = function
    | Var v -> Format.fprintf fmt "%a" pp_var v
    | Abs (v, m) -> Format.fprintf fmt "(%a. %a)" pp_var v aux m
    | App (m, n) -> Format.fprintf fmt "(%a %a)" aux m aux n
  in
  aux

let combinators_to_str = function `S -> "S" | `K -> "K" | `I -> "I"

let pp_combinator pp_var =
  let rec aux fmt = function
    | CVar v -> Format.fprintf fmt "%a" pp_var v
    | CApp _ as x -> (
        let combs : 'a combinator list =
          let rec aux d =
            match d with CVar _ -> [ d ] | CApp (x, y) -> y :: aux x
          in
          aux x
        in
        let combs = List.rev combs in
        match combs with
        | [] | _ :: [] -> assert false
        | [ c1; c2 ] -> Format.fprintf fmt "`%a%a" aux c1 aux c2
        | _ :: _ :: _ ->
            Format.fprintf fmt "(";
            List.iter (fun c -> Format.fprintf fmt "%a" aux c) combs;
            Format.fprintf fmt ")")
  in
  aux

let pp_combinators fmt c = Format.fprintf fmt "%s" (combinators_to_str c)
let combinator_to_str m = Format.asprintf "%a" (pp_combinator pp_combinators) m

let rec is_free m v =
  match m with
  | Var w -> v <> w
  | Abs (w, n) -> if v = w then true else is_free n v
  | App (n, o) -> is_free n v && is_free o v

let rec map f = function
  | Var v -> Var (f v)
  | Abs (v, m) -> Abs (f v, map f m)
  | App (m, n) -> App (map f m, map f n)

type combinators = [ `S | `K | `I ]

let ski (m : string lambda) : combinators combinator =
  let rec lambda_to_comb :
      [ `Com of combinators | `Str of string ] lambda -> combinators combinator
      = function
    | Var (`Com v) -> CVar v
    | Var (`Str _) | Abs _ -> assert false
    | App (m, n) -> CApp (lambda_to_comb m, lambda_to_comb n)
  in
  let com s = Var (`Com s) in
  let rec aux m =
    Format.eprintf "Converting: %a\n"
      (pp_lambda (fun fmt v ->
           let s =
             match v with `Com s -> combinators_to_str s | `Str s -> s
           in
           Format.fprintf fmt "%s" s))
      m;
    match m with
    | Var _ -> m
    | Abs (v, m) when is_free m v -> App (com `K, aux m)
    | Abs (v, (Abs _ as m)) -> aux (Abs (v, aux m))
    | Abs (v, App (m, n)) ->
        let tm = aux (Abs (v, m)) in
        let tn = aux (Abs (v, n)) in
        App (App (com `S, tm), tn)
    | Abs (v, Var w) when v = w -> com `I
    | Abs (_, (Var _ as m)) -> App (com `K, m)
    | App (m, n) -> App (aux m, aux n)
  in
  let m = aux @@ map (fun v -> `Str v) m in
  Format.eprintf "Converted: %a\n"
    (pp_lambda (fun fmt v ->
         let s = match v with `Com s -> combinators_to_str s | `Str s -> s in
         Format.fprintf fmt "%s" s))
    m;
  lambda_to_comb m

exception StepLimit

let reduce_comb m =
  let step = ref 0 in
  let rec aux m =
    if !step > 1000 then raise StepLimit;
    step := !step + 1;
    match m with
    | CApp (CVar `I, m) -> aux m
    | CApp (CApp (CVar `K, m), _) -> aux m
    | CApp (CApp (CApp (CVar `S, m), n), o) ->
        aux (CApp (CApp (m, o), CApp (n, o)))
    | CVar _ as m -> m
    | CApp (m, n) as a ->
        let b = CApp (aux m, aux n) in
        if a = b then b else aux b
  in
  let pp_var fmt v =
    Format.fprintf fmt "%s"
    @@ match v with `S -> "S" | `K -> "K" | `I -> "I" | `Str s -> s
  in
  let _pp = pp_combinator pp_var in
  try
    let tm = aux m in
    (* Format.eprintf "Reduction: %a => %a\n" pp m pp tm ; *)
    tm
  with StepLimit -> (* Format.eprintf "Reduction Timeout: %a\n" pp m; *)
                    m

let rec size = function
  | Var _ -> 1
  | Abs (_, m) -> size m + 1
  | App (m, n) -> size m + size n + 1

let rec simplify (m : combinators combinator) =
  let rec aux :
      combinators combinator -> [ `S | `K | `I | `Str of string ] combinator =
    function
    | CVar ((`S | `K | `I) as v) -> CVar v
    | CApp (m, n) -> CApp (aux m, aux n)
  in
  let cm = aux m in
  let n = CApp (CApp (cm, CVar (`Str "X")), CVar (`Str "Y")) in
  if reduce_comb n = CVar (`Str "X") then CVar `K
  else
    let n = CApp (cm, CVar (`Str "X")) in
    if reduce_comb n = CVar (`Str "X") then CVar `I
    else
      match m with CVar _ -> m | CApp (m, n) -> CApp (simplify m, simplify n)

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

let n2charchnum n =
  let rec aux n = if n = 0 then Var "z" else App (Var "s", aux (n - 1)) in
  Abs ("s", Abs ("z", aux n))

let library = ref ([] : (string * string lambda) list)

let register_library name m =
  Format.eprintf "----------------Register %s\n" name;
  flush_all ();
  library := (name, m) :: !library
