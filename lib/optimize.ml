open Syntax
open Syntax.Combinator
open Interpreter
open Utils

let lift_fv d =
  if d = 0 then fun m -> m
  else
    let rec aux = function
      | CVar (`Fv x) -> CVar (`Fv (x + d))
      | CVar c -> CVar c
      | CApp (c1, c2) -> CApp (aux c1, aux c2)
    in
    aux

let pp_ski_fv_str_combinator = Combinator.pp ComStrFv.pp

let rec is_ski_free m =
  match m with
  | CVar (`Com _) -> false
  | CVar _ -> true
  | CApp (m, n) -> is_ski_free m && is_ski_free n

let generate_behavior_hash_base vn_to_var vn_com_to_hash m =
  let rec aux vn m =
    match reduce_comb m with
    | None -> None
    | Some tm ->
        if vn > 4 then Some (vn, m)
        else if is_ski_free tm then Some (vn, tm)
        else aux (vn + 1) (CApp (tm, CVar (vn_to_var vn)))
  in
  match aux 0 m with
  | None -> None
  | Some (vn, m) ->
      let rec rev_eta vn m =
        match m with
        | CApp (m, CVar v) when v = vn_to_var (vn - 1) -> rev_eta (vn - 1) m
        | CApp _ | CVar _ -> (vn, m)
      in
      let vn, m = rev_eta vn m in
      vn_com_to_hash vn m

let generate_behavior_hash =
  generate_behavior_hash_base
    (fun vn -> `Str (Format.sprintf "v%d" vn))
    (fun vn m -> Some (Format.asprintf "%d_%a" vn pp_ski_fv_str_combinator m))

let hash_db = Hashtbl.create 2000000

let enumerate_ski ~(size : int) ~(fvn : int) =
  let table =
    Array.make_matrix (size + 1) (fvn + 1)
      ([] : ComStrFv.com_str_fv combinator list)
  in

  let register_db c =
    match generate_behavior_hash c with
    | None -> true (* XXX: Is this appropriate? *)
    | Some h -> (
        let l = ShortestPp.approx_size c in
        (* Format.eprintf "Hash %s => %s\n" s h; *)
        match Hashtbl.find_opt hash_db h with
        | Some (_, cl) ->
            if cl > l then (
              Hashtbl.replace hash_db h (c, l);
              true)
            else false
        | None ->
            Hashtbl.replace hash_db h (c, l);
            true)
  in

  table.(0).(0) <-
    List.filter register_db
    @@ List.map
         (fun v -> CVar (`Com v))
         ([ `S; `K; `I; `Iota] @ List.init (2+4+8+16) (fun i -> `Jot (i + 2)));
  if fvn > 0 then table.(0).(1) <- List.filter register_db [ CVar (`Fv 0) ];

  for i = 1 to size do
    for ai = 0 to fvn do
      let ttl = ref [] in
      for j = 0 to i - 1 do
        for aj = 0 to ai do
          let t1 = table.(j).(aj) in
          let t2 = table.(i - j - 1).(ai - aj) in
          let tl =
            List.concat_map
              (fun l1 -> List.rev_map (fun l2 -> CApp (l1, lift_fv aj l2)) t2)
              t1
          in
          let tl = List.filter register_db tl in
          Format.eprintf "size: %d\n" (List.length tl);
          ttl := tl :: !ttl
        done
      done;
      table.(i).(ai) <- List.concat_map (fun x -> x) !ttl
    done
  done;
  let res =
    Array.to_list table
    |> List.concat_map Array.to_list
    |> List.concat_map (fun x -> x)
  in
  Format.eprintf "Hash Table generated with size %d\n" (List.length res);
  flush_all ();
  (* assert false; *)
  (* List.iter (fun c ->
       Format.eprintf "%a\n" pp_ski_fv_str_combinator c;
     ) res; *)
  res

let init_hash_db =
  cached (fun () ->
      let _ = enumerate_ski ~size:4 ~fvn:2 in

      (* Hashtbl.iter
         (fun h (c, _) ->
           Format.eprintf "HashResult %s => %a\n" h pp_ski_fv_str_combinator c)
         hash_db; *)
      (* assert false; *)
      ())

let com_to_com_str :
    Combinators.t combinator -> [> `Com of Combinators.t ] combinator =
  let rec aux = function
    | CVar v -> CVar (`Com v)
    | CApp (m, n) -> CApp (aux m, aux n)
  in
  aux

let com_str_to_com :
    [> `Com of Combinators.t ] combinator -> Combinators.t combinator =
  let rec aux = function
    | CVar (`Com v) -> CVar v
    | CVar _ -> assert false
    | CApp (m, n) -> CApp (aux m, aux n)
  in
  aux

let enumerate_partial_repr (m : ComStrFv.com_str_fv combinator) ~(vn : int) =
  let rec aux (m : ComStrFv.com_str_fv combinator) ~(vn : int) ~(off : int) =
    match m with
    | CVar _ ->
        if vn = 0 then [ (m, fun m -> m) ]
        else if vn = 1 then [ (CVar (`Fv off), fun n -> subst n (`Fv off) m) ]
        else []
    | CApp (m, n) as o ->
        if vn = 0 then [ (o, fun m -> m) ]
        else
          let rems =
            List.init (vn + 1) (fun i ->
                let tm = aux m ~vn:i ~off in
                let tn = aux n ~vn:(vn - i) ~off:(off + i) in
                List.concat_map
                  (fun (m, fm) ->
                    List.concat_map
                      (fun (n, fn) ->
                        let base = [ (CApp (m, n), fun o -> fn (fm o)) ] in
                        if
                          i = 1
                          && vn - i = 1
                          && fm (CVar (`Fv off)) = fn (CVar (`Fv (off + 1)))
                        then (CApp (m, lift_fv (-1) n), fun o -> fm o) :: base
                        else base)
                      tn)
                  tm)
            |> List.concat_map (fun x -> x)
          in
          if vn = 1 then (CVar (`Fv off), fun n -> subst n (`Fv off) o) :: rems
          else rems
  in
  List.concat_map (fun x -> x) (List.init (vn + 1) (fun vn -> aux m ~vn ~off:0))

module TermSet = Set.Make (Combinator.Order (ComStrFv.Order))

let is_optimal = ref TermSet.empty

let optimize_with_simpler_term m =
  let rec aux m =
    if TermSet.mem m !is_optimal then m
    else
      let thms = enumerate_partial_repr m ~vn:2 in
      let ml = ShortestPp.approx_size m in
      (* (if String.length ms < 100 then begin
         Format.eprintf "Base %s\n" ms;
         List.iter (fun (tm,f) ->
           Format.eprintf "%a / %a\n" pp_ski_fv_str_combinator tm pp_ski_fv_str_combinator (f m)
         ) thms end);
      *)
      let thm =
        List.fold_left
          (fun acc (pm, f) ->
            (* Format.eprintf "try generate hash pm: %a (acc %a) (m %a)\n"
               pp_ski_fv_str_combinator pm
               pp_ski_fv_str_combinator (match acc with Some (m,_,_) -> m | None -> CVar(`Str "None"))
               pp_ski_fv_str_combinator m; *)
            match acc with
            | Some (tm, _, _, _) when tm <> m -> acc
            | Some _ | None -> (
                match generate_behavior_hash pm with
                | Some h -> (
                    (* Format.eprintf "pm: %a -> h: %s\n" pp_ski_fv_str_combinator pm h; *)
                    match Hashtbl.find_opt hash_db h with
                    | None -> None
                    | Some (tm, l) ->
                        let ftm = f tm in
                        let ftl = ShortestPp.approx_size ftm in
                        if ftl < ml then Some (ftm, tm, pm, l) else acc)
                | None -> None))
          None thms
      in
      match thm with
      | Some (tm, h, pm, _) when tm <> m ->
          Format.eprintf "Reduce %a => %a by %a with pm %a\n"
            pp_ski_fv_str_combinator m pp_ski_fv_str_combinator tm
            pp_ski_fv_str_combinator h pp_ski_fv_str_combinator pm;
          tm
      | _ ->
          let res =
            match m with CVar _ -> m | CApp (m, n) -> CApp (aux m, aux n)
          in
          if res = m then is_optimal := TermSet.add m !is_optimal;
          res
  in
  let m = aux m in
  m

let optimize m =
  let () = init_hash_db () in
  let rec loop m cnt =
    if cnt >= 15 then m
    else
      let tm = optimize_with_simpler_term m in
      (* Format.eprintf "Optimized: %a\n" (Combinator.pp ComStrFv.pp) tm; *)
      if tm = m then tm else loop tm (cnt + 1)
  in
  let m = com_to_com_str m in
  let m = loop m 0 in
  let m = com_str_to_com m in
  (* Format.eprintf "size_of_optimal: %d\n" (TermSet.cardinal !is_optimal); *)
  flush_all ();
  m
