open Syntax

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

let combinators_to_str = function `S -> "S" | `K -> "K" | `I -> "I" | `Z -> "0"

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

let size_of_pp_combinator =
  let rec aux = function
    | CVar v -> 1
    | CApp _ as x -> (
        let combs : 'a combinator list =
          let rec aux d =
            match d with CVar _ -> [ d ] | CApp (x, y) -> y :: aux x
          in
          aux x
        in
        match combs with
        | [] | _ :: [] -> assert false
        | [ c1; c2 ] -> aux c1 + aux c2 + 1
        | _ :: _ :: _ -> List.fold_left ( + ) 2 (List.map aux combs))
  in
  aux

let pp_combinators fmt c = Format.fprintf fmt "%s" (combinators_to_str c)
let combinator_to_str m = Format.asprintf "%a" (pp_combinator pp_combinators) m

let pp_ski_str fmt v =
  Format.fprintf fmt "%s"
  @@ match v with `S -> "S" | `K -> "K" | `I -> "I" | `Z -> "0" | `Str s -> s

let rec is_free m v =
  match m with
  | Var w -> v <> w
  | Abs (w, n) -> if v = w then true else is_free n v
  | App (n, o) -> is_free n v && is_free o v

let rec map f = function
  | Var v -> Var (f v)
  | Abs (v, m) -> Abs (f v, map f m)
  | App (m, n) -> App (map f m, map f n)

let rec subst m v by =
  match m with
  | CVar w -> if v = w then by else m
  | CApp (m, n) -> CApp (subst m v by, subst n v by)

let s2comb s =
  let rec aux = function
    | Var v -> CVar v
    | Abs _ -> assert false
    | App (m, n) -> CApp (aux m, aux n)
  in
  aux @@ Library.s2lam s

let rec pow x y =
  if y = 0 then 1
  else
    let d = pow x (y / 2) in
    d * d * if y mod 2 = 1 then x else 1

type func = Unary of (int -> int) | Binary of (int -> int -> int)

(* Based on https://yshl.hatenadiary.com/entry/20081006/1223304302 *)

let pp_option pp_var fmt v =
  match v with
  | None -> Format.fprintf fmt "None"
  | Some v -> Format.fprintf fmt "Some(%a)" pp_var v

let pp_string fmt s = Format.fprintf fmt "%s" s

let b_based_churchnum_table =
  let converts =
    [
      (Unary (fun n -> n + 1), s2comb "(S S n)");
      (Binary (fun m n -> m + n), s2comb "(S (S m S) n)");
      (Binary (fun m n -> n * m), s2comb "(S (S I m) n)");
      (Binary (fun m n -> pow m n), s2comb "(S n m)");
      (Unary (fun m -> pow m m), s2comb "(S S I m)");
      (Unary (fun n -> n * (n + 1)), s2comb "(S (S S S) n)");
    ]
  in

  Format.eprintf "Converters\n";
  let n = 1300 in
  let tst = Array.init n (fun _ -> []) in
  for i = 1 to n - 1 do
    List.iter
      (fun (f, m) ->
        match f with
        | Unary f ->
            let tf = f i in
            if 0 <= tf && tf < n then tst.(tf) <- (i, i, m) :: tst.(tf)
        | Binary _ -> ())
      converts;
    for j = 1 to n - 1 do
      List.iter
        (fun (f, m) ->
          match f with
          | Unary _ -> ()
          | Binary f ->
              let tf = f i j in
              if 0 <= tf && tf < n then tst.(tf) <- (i, j, m) :: tst.(tf))
        converts
    done
  done;
  Format.eprintf "Make etable\n";
  let btable = Array.make n (s2comb "I") in
  btable.(0) <- s2comb "K(KI)";
  btable.(1) <- s2comb "KI";
  for i = 2 to n - 1 do
    let tm =
      tst.(i)
      |> List.fold_left
           (fun res (a, b, m) ->
             if a >= i || b >= i then res
             else
               (* Format.eprintf "%d <- %d %d | %a\n" i a b (pp_option (pp_combinator pp_string)) res; *)
               let ta = btable.(a) in
               let tb = btable.(b) in
               let tm = subst (subst m "m" ta) "n" tb in
               match res with
               | None -> Some tm
               | Some bm ->
                   if size_of_pp_combinator bm > size_of_pp_combinator tm then
                     Some tm
                   else res)
           None
    in
    Format.eprintf "%d %a\n" i (pp_option (pp_combinator pp_string)) tm;
    btable.(i) <- Option.get tm
  done;
  btable

let shortest_churchnum_table =
  let bcom = s2comb "(S(KS)K)" in
  Array.map (fun d -> CApp (d, bcom)) b_based_churchnum_table

type combinators = [ `S | `K | `I | `Z ]

let rec comb_to_lambda :
    string combinator -> [ `Com of combinators | `Str of string ] lambda =
  function
  | CVar "S" -> Var (`Com `S)
  | CVar "K" -> Var (`Com `K)
  | CVar "I" -> Var (`Com `I)
  | CVar v -> failwith ("Unsupported combinator: " ^ v)
  | CApp (m, n) -> App (comb_to_lambda m, comb_to_lambda n)

let n2charchnum n =
  if n < Array.length shortest_churchnum_table then
    comb_to_lambda @@ shortest_churchnum_table.(n)
  else (
    Format.eprintf "Generate non-optimized church num: %d\n" n;
    let rec aux n = if n = 0 then Var "z" else App (Var "s", aux (n - 1)) in
    map (fun v -> `Str v) @@ Abs ("s", Abs ("z", aux n)))

let ski (m : string lambda) : combinators combinator =
  let rec lambda_to_comb :
      [ `Com of combinators | `Str of string ] lambda -> combinators combinator
      = function
    | Var (`Com v) -> CVar v
    | Var (`Str s) -> failwith @@ "Not-converted free variable " ^ s
    | Abs _ -> assert false
    | App (m, n) -> CApp (lambda_to_comb m, lambda_to_comb n)
  in
  let com s = Var (`Com s) in
  let pp =
    pp_lambda (fun fmt v ->
        let s = match v with `Com s -> combinators_to_str s | `Str s -> s in
        Format.fprintf fmt "%s" s)
  in
  let rec aux m =
    (* Format.eprintf "Converting: %a\n"
       (pp_lambda (fun fmt v ->
            let s =
              match v with `Com s -> combinators_to_str s | `Str s -> s
            in
            Format.fprintf fmt "%s" s))
       m; *)
    match m with
    | Var (`Str s) when String.get s 0 = '$' -> (
        let s = String.sub s 1 (String.length s - 1) in
        try aux @@ map (fun v -> `Str v) @@ List.assoc s !Library.library
        with Not_found -> failwith ("Undefined variable $" ^ s))
    | Var (`Str s) when String.get s 0 = '*' ->
        let n = int_of_string @@ String.sub s 1 (String.length s - 1) in
        let res = aux @@ n2charchnum n in
        Format.eprintf "Numconv: %d => %a\n" n pp res;
        res
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
  let res = lambda_to_comb m in
  Format.eprintf "Converted in comb: %a\n" (pp_combinator pp_combinators) res;
  res

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
    | CVar `Z -> aux @@ CApp (CVar `S, CVar `K)
    | CVar _ as m -> m
    | CApp (m, n) as a ->
        let b = CApp (aux m, aux n) in
        if a = b then b else aux b
  in
  let _pp = pp_combinator pp_ski_str in
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
