open Syntax
open Syntax.Lambda
open Syntax.Combinator
open Syntax.Combinators
open Utils

let pp_str_fv fmt = function
  | `Fv x -> Format.fprintf fmt "v%d" x
  | `Str s -> Format.fprintf fmt "%s" s

let com_str_to_com_str_fv =
  Combinator.map (fun v -> match v with `Str v -> `Str v | `Com v -> `Com v)

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
  Logs.info (fun a -> a "try is_Ycom: %a" pp m);
  loop m

let is_num_comb =
  let rec aux m acc =
    match m with
    | App (Var (`Fv 0), m) -> aux m (acc + 1)
    | Var (`Fv 1) -> Some acc
    | _ -> None
  in
  let check = function
    | Abs (`Fv 0, Var (`Fv 0)) -> Some 1
    | Abs (`Fv 0, Abs (`Fv 1, m)) -> aux m 0
    | _ -> None
  in
  fun m ->
    let m = com_str_to_com_str_fv m in
    let* h = Optimize.generate_behavior_hash_as_lambda m in
    Logs.info (fun a ->
        a "Check is_num_comb: %a => %a" (Combinator.pp pp_com_str) m
          (Lambda.pp pp_str_fv) h);
    check h

let is_num_func_narg m args =
  let rec aux args acc =
    match args with
    | [] -> Some acc
    | arg :: args ->
        let ims =
          List.map
            (fun i ->
              let si = Format.asprintf "*%d" i in
              let im = Ski.ski (Var (`Str si)) in
              let im = Combinator.map (fun c -> `Com c) im in
              im)
            arg
        in
        let* t =
          is_num_comb (List.fold_left (fun m im -> CApp (m, im)) m ims)
        in
        aux args ((arg, t) :: acc)
  in
  let* v = aux args [] in
  let non_num_funcs =
    [
      (fun (x, y) -> x = [ y ]);
      (fun (x, y) -> match x with [ x1; _ ] when x1 = y -> true | _ -> false);
      (fun (x, y) -> match x with [ _; x2 ] when x2 = y -> true | _ -> false);
    ]
  in
  if List.exists (fun f -> List.for_all f v) non_num_funcs then None
  else
    Some
      (Format.asprintf "numfun[%a]"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
            (fun fmt (x, y) ->
              Format.fprintf fmt "{%a} -> %d"
                (Format.pp_print_list
                   ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
                   Format.pp_print_int)
                x y))
         v)

let is_num_func m =
  let _ = m in
  None
(* match is_num_func_narg m (List.init 6 (fun i -> [i])) with
   | None ->
     (* None *)
     let args = [[3;1];[4;1];[5;9];[2;6];[5;3];[5;8];[9;7]] in
     is_num_func_narg m args
   | Some s -> Some s *)

let reverse_constants =
  Library.load_library ();
  let pp_vn vn = `Str (Format.sprintf "v%d" vn) in
  let pp_hash vn m =
    Some (Format.asprintf "%d_%a" vn (Combinator.pp pp_com_str) m)
  in
  let hash_of_constants =
    !Library.library
    |> List.filter (fun (s, _) ->
           not @@ List.mem s [ "to_c"; "t"; "f"; "iota" ])
    |> List.filter_map (fun (s, m) ->
           let m = Ski.ski m in
           let m = Optimize.com_to_com_str m in
           let* h = Optimize.generate_behavior_hash_base pp_vn pp_hash m in
           Logs.info (fun a -> a "lib %s => h: %s" s h);
           Some (h, s))
  in
  let rec aux m =
    let tm =
      match is_num_comb m with
      | Some v when v >= 2 -> Some (`Str ("*" ^ string_of_int v))
      | _ -> (
          match is_num_func m with
          | None -> (
              let* h = Optimize.generate_behavior_hash_base pp_vn pp_hash m in
              match List.assoc_opt h hash_of_constants with
              | None -> None
              | Some s -> Some (`Str ("$" ^ s)))
          | Some v -> Some (`Str v))
    in
    match tm with
    | Some v -> CVar v
    | None -> (
        if detect_Y_emphilically m then CVar (`Str "Y")
        else match m with CVar _ -> m | CApp (m, n) -> CApp (aux m, aux n))
  in
  aux

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
  let gen_fv = Lambda.gen_gen_fv 0 in
  let rec aux m =
    let _mm = m in
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
    (* | CVar (`Com (`Jot _)) -> failwith "Jot decompile isn't implemented yet" *)
    | CVar (`Com (`Jot j)) -> Syntax.Combinator.jot2skicomb j |> aux
    | CVar (`Com `Iota) ->
        let x = gen_fv () in
        Abs (x, App (App (Var x, aux @@ CVar (`Com `S)), aux @@ CVar (`Com `K)))
    (* | CApp (CApp (CVar (`Com `S), m), n) -> (
        match
          let x = gen_fv () in
          let* m = hoist_abs_with_x (aux m) x in
          let* n = hoist_abs_with_x (aux n) x in
          Some (Abs (x, App (m, n)))
        with
        | None -> recursively mm
        | Some m -> m) *)
    | m -> (
        match
          Logs.info (fun a ->
              a "Try generate hash of %a" (Combinator.pp ComStrFv.pp) m);

          (* let* s = Optimize.generate_behavior_hash m in
             Logs.info (fun a -> a "Hash generated(Str): %s" s); *)
          let* h = Optimize.generate_behavior_hash_as_lambda m in
          Logs.info (fun a -> a "Hash generated: %a" (Lambda.pp ComStrFv.pp) h);
          Some h
        with
        | Some h -> h
        | None -> recursively m)
  and recursively = function
    | CVar _ -> assert false
    | CApp (m, n) -> App (aux m, aux n)
  in
  aux

let try_simplify_by_reduction m =
  let* tm = Interpreter.reduce_lambda m in
  if Lambda.size tm < Lambda.size m then Some tm else None

let simplify_lam =
  let rec aux m =
    match m with
    (* | App (Abs (`Fv x, m), n)
       when Lambda.count m (`Fv x) <= 1
            || match n with Var _ -> true | _ -> false ->
         (* Logs.info (fun a -> a "com: %a / count: %d\n"
            (Lambda.pp pp_str_fv) m (Lambda.count m x)); *)
         Lambda.fv_safe_subst m x n *)
    | App (_, _) -> (
        match try_simplify_by_reduction m with
        | Some tm -> tm
        | None -> recursively m)
    | _ -> recursively m
  and recursively m =
    match m with
    | Var _ -> m
    | Abs (x, m) -> Abs (x, aux m)
    | App (m, n) -> App (aux m, aux n)
  in
  let rec loop i m =
    flush_all ();
    Logs.info (fun a -> a "simplify: %a" (Lambda.pp pp_str_fv) m);
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

let decompile m =
  let m = reverse_constants m in
  let m = com_str_to_com_str_fv m in
  Logs.info (fun a ->
      a "After reverse consts : %a" (Combinator.pp ComStrFv.pp) m);
  (* let m = hash_based_reduction m in
     Logs.info (fun a -> a "After Hash based reduction: %a" (Lambda.pp_with_indent pp_str_fv) m); *)
  let m = reverse_ski m in
  Logs.info (fun a ->
      a "After reverse_ski: %a" (Lambda.pp_with_indent pp_str_fv) m);
  let m = simplify_lam m in
  Logs.info (fun a ->
      a "After simplify: %a" (Lambda.pp_with_indent pp_str_fv) m);
  m
