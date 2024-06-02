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
        if vn > 4 then Some (vn, m) (* if vn > 4 then None *)
        else if is_ski_free tm then Some (vn, tm)
        else aux (vn + 1) (CApp (tm, CVar (vn_to_var vn)))
  in
  match aux 0 m with
  | None -> None
  | Some (vn, m) ->
      let rec rev_eta vn m =
        match m with
        | CApp (m, CVar v) when v = vn_to_var (vn - 1) && Combinator.is_free v m
          ->
            rev_eta (vn - 1) m
        | CApp _ | CVar _ -> (vn, m)
      in
      let vn, m = rev_eta vn m in
      vn_com_to_hash vn m

let generate_behavior_hash =
  generate_behavior_hash_base
    (fun vn -> `Str (Format.sprintf "v%d" vn))
    (fun vn m -> Some (Format.asprintf "%d_%a" vn pp_ski_fv_str_combinator m))

let generate_behavior_hash_as_lambda m =
  let open Lambda in
  let rec wrap_vn vn m =
    if vn = 0 then m else wrap_vn (vn - 1) @@ Abs (`Fv (vn - 1), m)
  in
  let conv_to_lambda =
    let rec aux = function
      | CVar ((`Str _ | `Fv _) as v) -> Some (Var v)
      (* | CVar (`Com c) -> Some (Var (`Str (Combinators.combinators_to_str c))) *)
      | CVar (`Com _) -> None
      | CApp (m, n) ->
          let* m = aux m in
          let* n = aux n in
          Some (App (m, n))
    in
    aux
  in
  generate_behavior_hash_base
    (fun vn -> `Fv vn)
    (fun vn m ->
      let* m = conv_to_lambda m in
      Some (wrap_vn vn m))
    m

let generate_beta_eta_behavior_hash m =
  Format.eprintf "%a@." (Combinator.pp ComStrFv.pp) m;
  let m = DeBruijn.of_comb m in
  let+ m = DeBruijn.reduce_beta_eta m in
  let s = DeBruijn.to_hash m in
  Format.eprintf "%s@." s;
  s

let hash_db = Hashtbl.create 2000000

let enumerate_ski ~(size : int) ~(fvn : int) ~(jotLen : int) =
  let table =
    Array.make_matrix (size + 1) (fvn + 1)
      ([] : ComStrFv.com_str_fv combinator list)
  in

  let register_db c =
    match generate_behavior_hash c with
    | None -> true (* XXX: Is this appropriate? *)
    | Some h -> (
        (* let l = ShortestPp.approx_size c in *)
        let l = ShortestPp.str_based_size c in
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

  (* 2 + 4 + 8 + 16 ... *)
  let jotnum =
    let rec aux i b = if i <= jotLen then b + aux (i + 1) (b * 2) else 0 in
    aux 1 2
  in

  table.(0).(0) <-
    List.filter register_db
    @@ List.map
         (fun v -> CVar (`Com v))
         ([ `S; `K; `I; `Iota ] @ List.init jotnum (fun i -> `Jot (i + 2)));
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
          (* Format.eprintf "size: %d\n" (List.length tl); *)
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
  Logs.info (fun a -> a "Hash Table generated with size %d" (List.length res));
  flush_all ();
  (* assert false; *)
  (* List.iter (fun c ->
       Format.eprintf "%a\n" pp_ski_fv_str_combinator c;
     ) res; *)
  res

(*
   module Pqueue = Psq.Make (Combinator.Order (ComStrFv.Order)) (Int)

   module EnumerateSkiSeq = struct

     (* WIP: (というよりも無限再帰になってしまっているので多分実装しない) *)

     (*
       Seqから取り出す際は、
       pquequeのmin と 各seqのpossible minを比べて、
       possible min がより小さい seq に関して分解する、を繰り返す
     *)

     type breakable_seq = (int * (t list) Seq.t)
     and t = Value of Pqueue.t * breakable_seq list

     type 'v with_len = ('v * int)

     module Seq_with_rems = struct
       type 'v t = ('v with_len list) * ('v with_len Seq.t Lazy.t)

       let of_seq (s:'v with_len Seq.t Lazy.t) : 'v t = ([],s)

       let head_seq ((_,s): 'v t) : 'v with_len = Seq.uncons (Lazy.force s) |> Option.get |> fst

       let rems ((v,_) : 'v t) : ('v with_len list) = v

       let uncons ((v,s): 'v t) : 'v t =
         let (h,tl) = Seq.uncons (Lazy.force s) |> Option.get in
         (h :: v, lazy tl)
     end

     module type Multiple_uncons_seq = sig
       type 'v t
       val get_possible_min : 'v t -> int
       val get_heads : ('v t) -> ('v -> 'v -> 'v) -> ('v with_len list * ('v t))
     end

     module Mult_seq = struct
       type 'v t = ('v Seq_with_rems.t) * ('v Seq_with_rems.t)

       let get_mult_seq s1 s2 =
         let open Seq_with_rems in
         (of_seq s1, of_seq s2)

       let might_generate_smaller l ((s1,s2):'v t) =
         if l < 2 then false else begin
           let open Seq_with_rems in
           let _,l1 = head_seq s1 in
           let _,l2 = head_seq s2 in
           min l1 l2 + 2 <= l
         end

       let get_possible_min ((s1,s2):'v t) =
         let open Seq_with_rems in
         let _,l1 = head_seq s1 in
         let _,l2 = head_seq s2 in
         min l1 l2 + 2

       let get_heads ((s1,s2):'v t) (prod: 'v -> 'v -> 'v) : ('v with_len list * ('v t)) =
         (* 各seqはgen由来なので長さが最小単位の仮定をおいてよさそう *)
         let open Seq_with_rems in
         let h1,l1 = head_seq s1 in
         let h2,l2 = head_seq s2 in
         if l1 < l2 then begin
           let ts1 = uncons s1 in
           ((List.map (fun (c2,l2) -> (prod h1 c2, l1 + l2 + 1)) (rems s2)), (ts1,s2))
         end else begin
           let ts2 = uncons s2 in
           ((List.map (fun (c1,l1) -> (prod c1 h2, l1 + l2 + 1)) (rems s1)), (s1,ts2))
         end

         (* let rec aux s1 s2 =
           let ((_,l1) as v1),rs1 = Seq.uncons s1 |> Option.get in
           let ((_,l2) as v2),rs2 = Seq.uncons s2 |> Option.get in
           if l1 <= l2 then Seq.cons v1 (aux rs1 s2) else Seq.cons v2 (aux s1 rs2)
         in
           aux (SH.of_seq s1) (SH.of_seq s2) *)
     end

     let rec first_map cond f v =
       match v with
       | [] -> []
       | x :: xs ->
           if cond x then (f x) :: xs
           else x :: first_map cond f xs

     module Mut_seq_List = struct
       type 'v t = 'v Mult_seq.t list

       let might_generate_smaller l (vs:'v t) =
         List.exists (Mult_seq.might_generate_smaller l) vs

       let get_possible_min (vs:'v t) =
         match List.map Mult_seq.get_possible_min vs with
         | [] -> assert false
         | x :: xs -> List.fold_left min x xs

       let get_heads (vs: 'v t) (prod: 'v -> 'v -> 'v) :  ('v with_len list * ('v t)) =
         let l = get_possible_min vs in
         let res = ref None in
         let resl =
         first_map
           (fun x -> Mult_seq.get_possible_min x = l)
           (fun x -> let (r,tx) = Mult_seq.get_heads x prod in
             res := Some r; tx) vs
         in
           (!res |> Option.get, resl)
     end

     let rec gen_seq_base (fvmin,fvmax)
       : ((ComStrFv.com_str_fv Combinator.combinator) * int) Seq.t =
       Format.eprintf "gen_seq_base %d %d@." fvmin fvmax;
       Seq.memoize @@
         let ss : (ComStrFv.com_str_fv Combinator.combinator) Mut_seq_List.t = List.init
           (fvmax-fvmin+1)
           (fun i ->
             let i = i + fvmin in
             Mult_seq.get_mult_seq
             (lazy (gen_seq_base (fvmin,i))) (lazy (gen_seq_base (i,fvmax))))
         in
         let ss,init_pque =
           if fvmin >= fvmax then
             (* No fv *)
             ss,Pqueue.(empty |>
               add_seq (List.to_seq @@
               List.map (fun c -> (CVar (`Com c), String.length (Combinators.combinators_to_str c)))
               @@ ([ `S; `K; `I; `Iota ]
                    @ List.init (2+4+8+16+32+64+128) (fun i -> `Jot (i + 2))))
               )
           else if fvmin + 1 = fvmax then
             ss,Pqueue.(empty |> add (CVar (`Fv fvmin)) 1)
           else
             ss,Pqueue.empty
         in
         let prod c1 c2 = CApp(c1,c2) in
         let rec aux pque ss =
           Format.eprintf "pop (%d,%d)@." fvmin fvmax;
           match Pqueue.min pque with
           | None ->
               let vs,rss = Mut_seq_List.get_heads ss prod in
               aux (Pqueue.of_list vs) rss
           | Some (q,l) -> begin
               Format.eprintf "pop succeeded (%d,%d) %d@." fvmin fvmax l;
               Format.eprintf "Is_smaller %b@." (Mut_seq_List.might_generate_smaller l ss);
               if Mut_seq_List.might_generate_smaller l ss then begin
                 let vs,rss = Mut_seq_List.get_heads ss prod in
                 aux (Pqueue.add_seq (List.to_seq vs) pque) rss
               end else begin
                 let rem = Pqueue.rest pque |> Option.get in
                 Format.eprintf "pop pque %d %a@." l (Combinator.pp ComStrFv.pp) q;
                 let res = Seq.Cons((q,l),(fun () -> aux rem ss)) in
                 Format.eprintf "pop return %d@." l;
                 res
               end
             end
           in
             fun () -> aux init_pque ss
   end *)

(* Generates exprs
   - whose size is at most `size`
   - who have at most fvn free variables. Each fvs appear at most once. *)
let enumerate_ski_with_size ~(size : int) ~(fvn : int) =
  (* Index: size, fvmin, fvmax, *)
  (* size(com) = size, and fvs(com) = [fvmin,fvmax) *)
  let table = Array.make_matrix (size + 1) (fvn + 1) (Array.make 0 []) in
  for i = 0 to size do
    for j = 0 to fvn do
      table.(i).(j) <-
        (Array.make (fvn + 1) [] : ComStrFv.com_str_fv combinator list array)
    done
  done;

  let register_db l c =
    match generate_behavior_hash c with
    | None -> true (* XXX: Is this appropriate? *)
    | Some h -> (
        (* let l = ShortestPp.approx_size c in *)
        (* let l = ShortestPp.str_based_size c in *)
        (* Format.eprintf "Hash %a => %s\n" (Combinator.safe_pp ComStrFv.pp) c h; *)
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

  (* 2 + 4 + 8 + 16 ... *)
  table.(1).(0).(0) <-
    List.filter (register_db 1)
    @@ List.map
         (fun v -> CVar (`Com v))
         ([ `S; `K; `I; `Iota ] @ List.init 2 (fun i -> `Jot (i + 2)));

  if fvn > 0 then
    for i = 0 to fvn - 1 do
      table.(1).(i + 1).(i + 1) <- table.(1).(i).(i);
      table.(1).(i).(i + 1) <- List.filter (register_db 1) [ CVar (`Fv i) ]
    done;

  for size = 2 to size do
    for fvmin = 0 to fvn do
      for fvmax = fvmin to fvn do
        (* Format.eprintf "Item bef of @@ (%d)(%d)(%d)@." size fvmin fvmax;
           List.iter (fun c ->
             Format.eprintf "%a@." (Combinator.safe_pp ComStrFv.pp) c;
           ) table.(size).(fvmin).(fvmax); *)
        if fvmin = fvmax && fvmin > 0 then
          table.(size).(fvmin).(fvmin) <- table.(size).(fvmin - 1).(fvmin - 1)
        else
          let ttl = ref [] in
          (if fvmin = fvmax && size <= 5 then
             let b =
               let rec aux k b = if k >= size then b else aux (k + 1) (b * 2) in
               aux 1 2
             in
             ttl :=
               (List.filter (register_db size)
               @@ List.map (fun v -> CVar (`Com v))
               @@ List.init b (fun i -> `Jot (i + b)))
               :: !ttl);

          for j = 1 to size - 2 do
            for fvmid = fvmin to fvmax do
              let t1 = table.(j).(fvmin).(fvmid) in
              let t2 = table.(size - j - 1).(fvmid).(fvmax) in
              (* Format.eprintf "Try (%d)(%d)(%d) & (%d)(%d)(%d)@."
                 j fvmin fvmid
                 (size - j - 1) (fvmid) (fvmax); *)
              let tl =
                List.concat_map
                  (fun l1 -> List.rev_map (fun l2 -> CApp (l1, l2)) t2)
                  t1
              in
              let tl = List.filter (register_db size) tl in
              ttl := tl :: !ttl;
              if fvmin <> fvmax then
                let tl =
                  List.concat_map
                    (fun l1 -> List.rev_map (fun l2 -> CApp (l2, l1)) t2)
                    t1
                in
                let tl = List.filter (register_db size) tl in
                ttl := tl :: !ttl
            done
          done;
          let tls = List.concat_map (fun x -> x) !ttl in
          (* Format.eprintf "size @@ (%d)(%d)(%d) : %d\n" i fvmin fvmax (List.length tls); *)
          table.(size).(fvmin).(fvmax) <- tls
          (* Format.eprintf "Item of @@ (%d)(%d)(%d)@." size fvmin fvmax;
             List.iter (fun c ->
               Format.eprintf "%a@." (Combinator.safe_pp ComStrFv.pp) c;
             ) table.(size).(fvmin).(fvmax); *)
      done
    done
  done;

  let idxs =
    List.init size (fun s -> List.init (fvn + 1) (fun tfv -> (s, tfv)))
    |> List.concat
  in
  let res = List.concat_map (fun (s, tfv) -> table.(s).(0).(tfv)) idxs in
  Logs.info (fun a -> a "Hash Table generated with size %d" (List.length res));
  flush_all ();
  (* assert false; *)
  (* List.iter (fun c ->
       Format.eprintf "%a\n" pp_ski_fv_str_combinator c;
     ) res; *)
  res

(* let _g =
   Seq.iter (fun (c,l) ->
     Format.eprintf "%3d : %a@." l (Combinator.pp ComStrFv.pp) c;
     assert false;
   ) @@ Seq.take 1 @@ EnumerateSkiSeq.gen_seq_base (0,2);
   assert false *)

let _g () =
  Format.eprintf "Run g@Optimize@.";
  let ms = enumerate_ski_with_size ~size:12 ~fvn:2 in
  List.iter
    (fun c -> Format.eprintf "%a@." (Combinator.safe_pp ComStrFv.pp) c)
    ms;
  assert false

let _f () =
  Format.eprintf "Run f@Optimize@.";
  let ms = enumerate_ski_with_size ~size:18 ~fvn:0 in
  Logs.info (fun a -> a "Table generated with size %d" (List.length ms));

  (* Hashtbl.iter (fun h _ ->
       Format.eprintf "Hash example: %s@." h
     ) hash_db;
     assert false; *)
  List.iter
    (fun m ->
      let hs =
        List.map
          (fun n ->
            let s =
              match generate_behavior_hash (CApp (m, n)) with
              | None -> "None"
              | Some h -> h
            in
            s)
          ((* [ CApp (CVar (`Com `K), CApp (CVar (`Com `K), CVar (`Com `I))) ] *)
           [ CVar (`Com `I) ]
          @ List.map (fun c -> CVar (`Com c)) [ `K; `I; `Jot 2 ])
      in
      if
        List.for_all (fun s -> s = List.hd hs) hs
        || List.exists (fun s -> String.length s > 15) hs
        || List.exists (fun s -> s = "None") hs
        || List.nth hs 1 <> List.nth hs 2
        (* || List.nth hs 0 = List.nth hs 1 *)
        (* || List.nth hs 0 = List.nth hs 3 *)
        || List.nth hs 1 = List.nth hs 3
        (* || (not (String.starts_with ~prefix:"3_" @@ List.nth hs 1)) *)
        (* || (not (String.starts_with ~prefix:"3_" @@ List.nth hs 3)) *)
        (* || List.nth hs 1 <> List.nth hs 3 *)
        (* || List.nth hs 1 <> "3_`v0v1" || (List.nth hs 2 <> List.nth hs 0) *)
      then ()
      else (
        Format.eprintf "%30s : "
          (Format.asprintf "%a" (Combinator.safe_pp ComStrFv.pp) m);
        List.iter (fun s -> Format.eprintf " %20s |" s) hs;
        Format.eprintf "@.");
      flush_all ())
    ms;
  assert false

let init_hash_db =
  cached (fun () ->
      (* let _ = enumerate_ski ~size:4 ~fvn:2 ~jotLen:4 in *)
      let _ = enumerate_ski_with_size ~size:14 ~fvn:2 in

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
                  tm
                |> List.filter (fun (x, _) -> ShortestPp.str_based_size x < 50))
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
      (* let ml = ShortestPp.approx_size m in *)
      let ml = ShortestPp.str_based_size m in
      let thms = enumerate_partial_repr m ~vn:2 in
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
                        (* let ftl = ShortestPp.approx_size ftm in *)
                        let ftl = ShortestPp.str_based_size ftm in
                        if ftl < ml then Some (ftm, tm, pm, l) else acc)
                | None -> None))
          None thms
      in
      match thm with
      | Some (tm, h, pm, _) when tm <> m ->
          Logs.info (fun a ->
              a "Reduce %a => %a by %a with pm %a" pp_ski_fv_str_combinator m
                pp_ski_fv_str_combinator tm pp_ski_fv_str_combinator h
                pp_ski_fv_str_combinator pm;
              flush_all ());
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

let optimize_only_annot m =
  let unknown_str s = failwith ("Unknown str@optimize_only_annot: " ^ s) in
  let rec aux m =
    match m with
    | CApp (CVar (`Str "optimize"), m) ->
        let m =
          Combinator.map (function `Com c -> c | `Str s -> unknown_str s) m
        in
        optimize m
    | CVar (`Com v) -> CVar v
    | CVar (`Str s) -> unknown_str s
    | CApp (m, n) -> CApp (aux m, aux n)
  in
  aux m
