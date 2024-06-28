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

let com_to_lambda : 'a combinator -> 'a lambda =
  let rec aux = function
    | CVar v -> Var v
    | CApp (m, n) -> App (aux m, aux n)
  in
  aux

exception StepLimit

let reduce_comb_one_step =
  let rec aux m =
    match m with
    | CApp (CVar (`Com `I), m) -> m
    | CApp (CApp (CVar (`Com `K), m), _) -> m
    | CApp (CApp (CApp (CVar (`Com `S), m), n), o) ->
        CApp (CApp (m, o), CApp (n, o))
    | CApp (CVar (`Com `Iota), m) -> iota2skicomb m
    | CVar (`Com (`Jot x)) -> jot2skicomb x
    | CVar _ as m -> m
    | CApp (m, n) -> CApp (aux m, aux n)
  in
  aux

let reduce_comb m =
  let base_size = Combinator.size m in
  let step = ref 0 in

  let _pp = Combinator.pp ComStrFv.pp in

  (* Logs.info (fun a -> a "Start Reduction: %a" _pp m); *)
  let rec loop m =
    if Combinator.size m > base_size * 100 then raise StepLimit;
    if !step > 10000 then raise StepLimit;
    step := !step + 1;
    let tm = reduce_comb_one_step m in
    (* Logs.info (fun a -> a "Reduction: %a -> %a" _pp m _pp tm); *)
    if tm = m then m else loop tm
  in
  try
    let tm = loop m in
    (* Logs.info (fun a -> a "ReductionCompleted: %a => %a" _pp m _pp tm); *)
    Some tm
  with StepLimit ->
    (* Logs.info (fun a -> a "Reduction Timeout: %a" pp m); *)
    None

let reduce_lambda_one_step m =
  let rec aux m =
    match m with
    | Var _ -> m
    | App (Abs (`Fv x, m), n) -> Lambda.fv_safe_subst m x n
    | Abs (x, m) -> Abs (x, aux m)
    | App (m, n) -> App (aux m, aux n)
  in
  aux m

let reduce_lambda m =
  let base_size = Lambda.size m in
  let step = ref 0 in

  let _pp = Lambda.pp ComStrFv.pp in
  let rec loop m =
    if Lambda.size m > base_size * 100 then raise StepLimit;
    if !step > 10000 then raise StepLimit;
    step := !step + 1;
    let tm = reduce_lambda_one_step m in
    (* Format.eprintf "Reduction: %a -> %a\n" _pp m _pp tm; *)
    if tm = m then m else loop tm
  in
  try
    let tm = loop m in
    (* Format.eprintf "Reduction: %a => %a\n" pp m pp tm ; *)
    Some tm
  with StepLimit -> (* Format.eprintf "Reduction Timeout: %a\n" pp m; *)
                    None

let reduce_lambda_icfpc_greedy =
  let pp = Lambda.pp Icfpc.pp in
  let rec aux m =
    let _mm = m in
    match m with
    | Var _ -> m
    | App (Abs (`Fv x, m), n) -> aux @@ Lambda.fv_safe_subst m x n
    | Abs (x, m) -> Abs (x, aux m)
    | App (App (App (Var (`Top op), m), n), o) -> (
        let m = aux m in
        match op with
        | "IF" -> (
            match m with
            | Var (`Bool true) -> aux n
            | Var (`Bool false) -> aux o
            | _ -> _mm)
        | _ -> failwith (Format.sprintf "Unknown ternary op: %s" op))
    | App (App (Var (`Bop op), m), n) -> (
        let m = aux m in
        let n = aux n in
        match (op, m, n) with
        | "+", Var (`Int m), Var (`Int n) -> Var (`Int (m + n))
        | "-", Var (`Int m), Var (`Int n) -> Var (`Int (m - n))
        | "*", Var (`Int m), Var (`Int n) -> Var (`Int (m * n))
        | "/", Var (`Int m), Var (`Int n) -> Var (`Int (m / n))
        | "%", Var (`Int m), Var (`Int n) -> Var (`Int (m mod n))
        | "<", Var (`Int m), Var (`Int n) -> Var (`Bool (m < n))
        | ">", Var (`Int m), Var (`Int n) -> Var (`Bool (m > n))
        | "&", Var (`Bool m), Var (`Bool n) -> Var (`Bool (m && n))
        | "|", Var (`Bool m), Var (`Bool n) -> Var (`Bool (m || n))
        | "=", Var m, Var n -> Var (`Bool (m = n))
        | ".", Var (`Str m), Var (`Str n) -> Var (`Str (m ^ n))
        | "T", Var (`Int m), Var (`Str n) -> Var (`Str (String.sub n 0 m))
        | "D", Var (`Int m), Var (`Str n) ->
            Var (`Str (String.sub n m (String.length n - m)))
        | _ ->
            failwith
              (Format.asprintf "Unknown binary op: %s: with value (%a) (%a)" op
                 pp m pp n))
    | App (Var (`Uop op), m) -> (
        let m = aux m in
        match (op, m) with
        | "-", Var (`Int m) -> Var (`Int (-m))
        | "!", Var (`Bool m) -> Var (`Bool (not m))
        | "#", Var (`Str m) -> Var (`Int Icfpc.(read2s m |> s2int))
        | "$", Var (`Int m) ->
            (* let ts = Icfpc.int2s m in
               Format.eprintf "int2s %d -> %s@." m ts; *)
            Var (`Str Icfpc.(int2s m |> s2read))
        | _ ->
            failwith
              (Format.asprintf "Unknown unnary op: %s: with value (%a)" op pp m)
        )
    | App (m, n) ->
        let tm = aux m in
        let tn = aux n in
        if m = tm && n = tn then _mm else aux @@ App (tm, tn)
  in
  fun m -> aux m

let reduce_lambda_icfpc m =
  let base_size = Lambda.size m in
  let step = ref 0 in

  let _pp = Lambda.pp ComStrFv.pp in
  let rec loop m =
    if Lambda.size m > base_size * 100 then raise StepLimit;
    if !step > 10000 then raise StepLimit;
    step := !step + 1;
    let tm = reduce_lambda_icfpc_greedy m in
    (* Format.eprintf "Reduction: %a -> %a\n" _pp m _pp tm; *)
    if tm = m then m else loop tm
  in
  try
    let tm = loop m in
    (* Format.eprintf "Reduction: %a => %a\n" pp m pp tm ; *)
    Some tm
  with StepLimit -> (* Format.eprintf "Reduction Timeout: %a\n" pp m; *)
                    None
