open Syntax
open Syntax.Lambda
open Syntax.Combinator
open Numbers

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
    Lambda.pp (fun fmt v ->
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
  Format.eprintf "Converting: %a\n"
    (Lambda.pp (fun fmt v ->
         let s = match v with `Com s -> combinators_to_str s | `Str s -> s in
         Format.fprintf fmt "%s" s))
    m;
  let res = lambda_to_comb m in
  Format.eprintf "Converted in comb: %a\n" (Combinator.pp pp_combinators) res;
  res
