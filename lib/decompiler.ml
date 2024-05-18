open Syntax
open Syntax.Lambda
open Syntax.Combinator
open Syntax.Combinators

let ( let* ) = Option.bind
let ( let+ ) v f = Option.map f v

let split_start_end_with ~s h t =
  if String.starts_with ~prefix:h s && String.ends_with ~suffix:t s then
    let s = String.sub s 0 (String.length s - String.length t) in
    let lh = String.length h in
    let s = String.sub s lh (String.length s - lh) in
    Some s
  else None

let detect_Y_emphilically m =
  let pp = Combinator.pp ComStrFv.pp in
  let hash m = Format.asprintf "%a" pp m in
  let get_head_v m =
    let rec aux m acc =
      match m with CApp (CVar (`Fv 0), m) -> aux m (acc + 1) | _ -> (acc, m)
    in
    aux m 0
  in
  let db = Hashtbl.create 100 in
  let base_size = Combinator.size m in
  let step = ref 0 in
  let rec loop m =
    let tm = Interpreter.reduce_comb_one_step m in
    if Combinator.size m > base_size * 100 then false
    else if !step > 1000 then false
    else (
      step := !step + 1;
      let d, b = get_head_v tm in
      let b = hash b in
      match Hashtbl.find_opt db b with
      | None ->
          Hashtbl.add db b d;
          loop tm
      | Some td -> if td < d then true else false)
  in
  let m = Combinator.map (function `Str v -> `Str v | `Com c -> `Com c) m in
  let m = CApp (m, CVar (`Fv 0)) in
  Format.eprintf "try is_Ycom: %a\n" pp m;
  loop m

let reverse_constants =
  let is_num_hash h =
    (* Example: 2_`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0`v0v1 *)
    Format.eprintf "h: %s\n" h;
    let* s = split_start_end_with ~s:h "2_" "v1" in
    Format.eprintf "h: %s\n" s;
    let rec aux s n =
      if String.length s = 0 then if n <= 2 then None else Some n
      else
        let* ts = split_start_end_with ~s "" "`v0" in
        Format.eprintf "h: %s\n" ts;
        aux ts (n + 1)
    in
    aux s 0
  in
  let pp_vn vn = `Str (Format.sprintf "v%d" vn) in
  let pp_hash vn m =
    Some (Format.asprintf "%d_%a" vn (Combinator.pp pp_com_str) m)
  in
  let hash_of_constants =
    !Library.library
    |> List.filter (fun (s, _) -> not @@ List.mem s [ "to_c"; "t"; "f" ])
    |> List.filter_map (fun (s, m) ->
           let m = Ski.ski m in
           let m = Optimize.com_to_com_str m in
           let* h = Optimize.generate_behavior_hash_base pp_vn pp_hash m in
           Format.eprintf "lib %s => h: %s\n" s h;
           Some (h, s))
  in
  let rec aux m =
    let tm =
      let* h = Optimize.generate_behavior_hash_base pp_vn pp_hash m in
      match is_num_hash h with
      | None -> (
          match List.assoc_opt h hash_of_constants with
          | None -> None
          | Some s -> Some (`Str s))
      | Some v -> Some (`Str ("*" ^ string_of_int v))
    in
    match tm with
    | Some v -> CVar v
    | None -> (
        if detect_Y_emphilically m then CVar (`Str "Y")
        else match m with CVar _ -> m | CApp (m, n) -> CApp (aux m, aux n))
  in
  aux

let gen_fv =
  let i = ref 0 in
  fun () ->
    let res = !i in
    i := !i + 1;
    `Fv res

let hoist_abs_with_x m x =
  let rec aux m =
    match m with
    | Var _ -> None
    | App (m, n) ->
        let* m = aux m in
        let* n = aux n in
        Some (App (m, n))
    | Abs (y, m) -> Some (Lambda.subst m y (Var x))
  in
  aux m

let reverse_ski =
  let rec aux m =
    let mm = m in
    match m with
    | CVar (`Str s) -> Var (`Str s)
    (* | CVar (`Com `I) -> Var (`Str "I")
       | CVar (`Com `K) -> Var (`Str "K")
       | CVar (`Com `S) -> Var (`Str "S") *)
    | CVar (`Com `I) ->
        let x = gen_fv () in
        Abs (x, Var x)
    | CVar (`Com `K) ->
        let x = gen_fv () in
        let y = gen_fv () in
        Abs (x, Abs (y, Var x))
    | CVar (`Com `S) ->
        let x = gen_fv () in
        let y = gen_fv () in
        let z = gen_fv () in
        Abs (x, Abs (y, Abs (z, App (App (Var x, Var z), App (Var y, Var z)))))
    | CVar (`Com (`Jot _)) -> failwith "Jot decompile isn't implemented yet"
    | CApp (CApp (CVar (`Com `S), m), n) -> (
        match
          let x = gen_fv () in
          let* m = hoist_abs_with_x (aux m) x in
          let* n = hoist_abs_with_x (aux n) x in
          Some (Abs (x, App (m, n)))
        with
        | None -> recursively mm
        | Some m -> m)
    | m -> (
        let rec wrap_vn vn m =
          if vn = 0 then m else wrap_vn (vn - 1) @@ Abs (`Fv (vn - 1), m)
        in
        let conv_to_lambda =
          let rec aux = function
            | CVar ((`Str _ | `Fv _) as v) -> Some (Var v)
            (* | CVar (`Com c) -> Some (Var (`Str (combinators_to_str c))) *)
            | CVar (`Com _) -> None
            | CApp (m, n) ->
                let* m = aux m in
                let* n = aux n in
                Some (App (m, n))
          in
          aux
        in
        match
          Format.eprintf "Try generate hash of %a\n"
            (Combinator.pp ComStrFv.pp)
            m;
          let* h =
            Optimize.generate_behavior_hash_base
              (fun vn -> `Fv vn)
              (fun vn m ->
                if vn = 0 then None
                else
                  let* m = conv_to_lambda m in
                  Some (wrap_vn vn m))
              m
          in
          Format.eprintf "Hash generated: %a\n" (Lambda.pp ComStrFv.pp) h;
          Some h
        with
        | Some h -> h
        | None -> recursively m)
  and recursively = function
    | CVar _ -> assert false
    | CApp (m, n) -> App (aux m, aux n)
  in
  aux

let rec max_v = function
  | Var (`Fv v) -> v
  | Var _ -> 0
  | Abs (`Fv x, m) -> max x (max_v m)
  | Abs (_, m) -> max_v m
  | App (m, n) -> max (max_v m) (max_v n)

let lift_v d =
  let rec aux env m =
    match m with
    | Var (`Fv v) when List.mem v env -> Var (`Fv (v + d))
    | Var _ -> m
    | Abs (`Fv x, m) -> Abs (`Fv (x + d), aux (x :: env) m)
    | Abs (x, m) -> Abs (x, aux env m)
    | App (m, n) -> App (aux env m, aux env n)
  in
  aux []

let pp_str_fv fmt = function
  | `Fv x -> Format.fprintf fmt "v%d" x
  | `Str s -> Format.fprintf fmt "%s" s

let simplify_lam =
  let rec aux m =
    match m with
    | App (Abs (x, m), n)
      when Lambda.count m x <= 1 || match n with Var _ -> true | _ -> false ->
        (* Format.eprintf "com: %a / count: %d\n"
           (Lambda.pp pp_str_fv) m (Lambda.count m x); *)
        let d = max_v m in
        let n = lift_v (d + 1) n in
        Lambda.subst m x n
    (* | App(Abs _,_) ->
        begin match try_simplify_by_reduction m with
        | Some tm -> tm
        | None -> recursively m
        end *)
    | _ -> recursively m
  and recursively m =
    match m with
    | Var _ -> m
    | Abs (x, m) -> Abs (x, aux m)
    | App (m, n) -> App (aux m, aux n)
  in
  let rec loop i m =
    flush_all ();
    Format.eprintf "simplify: %a\n" (Lambda.pp pp_str_fv) m;
    let tm = aux m in
    if i < 100 && tm <> m then loop (i + 1) tm else tm
  in
  loop 0

let hash_based_reduction m =
  let rec wrap_vn vn m =
    if vn = 0 then m else wrap_vn (vn - 1) @@ Abs (`Fv (vn - 1), m)
  in
  let conv_to_lambda =
    let rec aux = function
      | CVar ((`Str _ | `Fv _) as v) -> Some (Var v)
      | CVar (`Com c) -> Some (Var (`Str (combinators_to_str c)))
      | CApp (m, n) ->
          let* m = aux m in
          let* n = aux n in
          Some (App (m, n))
    in
    aux
  in
  match
    let* h =
      Optimize.generate_behavior_hash_base
        (fun vn -> `Fv vn)
        (fun vn m ->
          let* m = conv_to_lambda m in
          Some (wrap_vn vn m))
        m
    in
    Some h
  with
  | Some h -> h
  | None -> assert false

let com_str_to_com_str_fv =
  Combinator.map (fun v -> match v with `Str v -> `Str v | `Com v -> `Com v)

let decompile m =
  let m = reverse_constants m in
  let m = com_str_to_com_str_fv m in
  (* let m = hash_based_reduction m in *)
  let m = reverse_ski m in
  let _m = simplify_lam m in
  Format.eprintf "%a\n" (Lambda.pp_with_indent pp_str_fv) m;
  exit 0
