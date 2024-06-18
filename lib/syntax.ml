module Lambda = struct
  type 'var lambda =
    | Abs of 'var * 'var lambda
    | App of 'var lambda * 'var lambda
    | Var of 'var

  let pp_var fmt v = Format.fprintf fmt "%s" v

  let pp pp_var =
    let rec aux ~par fmt = function
      | Var v -> Format.fprintf fmt "%a" pp_var v
      | Abs (v, m) ->
          (if par then Format.fprintf fmt "(%a. %a)"
           else Format.fprintf fmt "%a. %a")
            pp_var v (aux ~par:false) m
      | App (m, n) ->
          let ms : 'a lambda list =
            let rec aux d =
              match d with Var _ | Abs _ -> [ d ] | App (x, y) -> y :: aux x
            in
            List.rev (n :: aux m)
          in
          if par then Format.fprintf fmt "(";
          Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
            (aux ~par:true) fmt ms;
          if par then Format.fprintf fmt ")"
    in
    aux ~par:true

  let pp_with_indent pp_var =
    let rec aux fmt = function
      | Var v -> Format.fprintf fmt "%a" pp_var v
      | Abs (v, m) -> Format.fprintf fmt "@[<2>(%a.@;%a@]@;)" pp_var v aux m
      | App (m, n) -> Format.fprintf fmt "@[<2>(@;%a@;%a@]@;)" aux m aux n
    in
    aux

  let rec is_free m v =
    match m with
    | Var w -> v <> w
    | Abs (w, n) -> if v = w then true else is_free n v
    | App (n, o) -> is_free n v && is_free o v

  let rec map f = function
    | Var v -> Var (f v)
    | Abs (v, m) -> Abs (f v, map f m)
    | App (m, n) -> App (map f m, map f n)

  let rec subst m x n =
    match m with
    | Var y -> if x = y then n else m
    | Abs (y, m) as mm -> if x = y then mm else Abs (y, subst m x n)
    | App (m, o) -> App (subst m x n, subst o x n)

  let rec count m x =
    match m with
    | Var y -> if x = y then 1 else 0
    | Abs (y, m) -> if x = y then 0 else count m x
    | App (m, n) -> count m x + count n x

  let rec size = function
    | Var _ -> 1
    | Abs (_, m) -> size m + 1
    | App (m, n) -> size m + size n

  let abs_lift d =
    let rec aux env m =
      match m with
      | Var (`Fv v) when List.mem v env -> Var (`Fv (v + d))
      | Var _ -> m
      | Abs (`Fv x, m) -> Abs (`Fv (x + d), aux (x :: env) m)
      | Abs (x, m) -> Abs (x, aux env m)
      | App (m, n) -> App (aux env m, aux env n)
    in
    aux []

  let max_free_fv m =
    let rec aux env = function
      | Var (`Fv v) -> if List.mem v env then 0 else v
      | Var _ -> 0
      | Abs (`Fv x, m) -> aux (x :: env) m
      | Abs (_, m) -> aux env m
      | App (m, n) -> max (aux env m) (aux env n)
    in
    aux [] m

  let all_fv =
    let rec aux = function
      | Var (`Fv v) -> v
      | Var _ -> -1
      | Abs (`Fv x, m) -> max x (aux m)
      | Abs (_, m) -> aux m
      | App (m, n) -> max (aux m) (aux n)
    in
    aux

  let free_fvs m =
    let rec aux env = function
      | Var (`Fv v) -> if List.mem v env then [] else [ v ]
      | Var _ -> []
      | Abs (`Fv x, m) -> aux (x :: env) m
      | Abs (_, m) -> aux env m
      | App (m, n) -> aux env m @ aux env n
    in
    aux [] m

  let gen_gen_fv start =
    let i = ref start in
    fun () ->
      let res = !i in
      i := !i + 1;
      `Fv res

  (* m中でnの自由変数が束縛されかかった場合は、mのほうのabsを十分な高さまで持ち上げる *)
  let fv_safe_subst m x n =
    (* let _pp = pp (fun fmt -> function (`Fv i) -> Format.fprintf fmt "v%d" i | _ -> assert false) in *)
    let max_fv = max (max (all_fv m) (max_free_fv n)) x in
    let gen_fv = gen_gen_fv (max_fv + 1) in
    let n_fvs = free_fvs n in
    let rec aux m =
      (* Format.eprintf "aux: %a@." _pp m; *)
      match m with
      | Var y -> if `Fv x = y then n else m
      | Abs (`Fv y, m) as mm ->
          if x = y then mm
          else if List.mem y n_fvs then
            let ty = gen_fv () in
            (* Format.eprintf "Befrep: %a@." _pp m; *)
            let m = subst m (`Fv y) (Var ty) in
            (* Format.eprintf "Aftrep: %a@." _pp m; *)
            Abs (ty, aux m)
          else Abs (`Fv y, aux m)
      | Abs (y, m) -> Abs (y, aux m)
      | App (m, n) -> App (aux m, aux n)
    in
    aux m
end

module Combinators = struct
  type t = [ `S | `K | `I | `D | `Jot of int | `Iota ]
  type com_str = [ `Com of t | `Str of string ]

  let n2bin n =
    assert (n >= 2);
    let rec aux n =
      let b = n mod 2 in
      if n <= 1 then [ b ] else aux (n / 2) @ [ b ]
    in
    match aux n with 1 :: v -> v | _ -> assert false

  let size_of_n2jotstr n = List.length @@ n2bin n

  let n2jotstr n =
    let v = n2bin n in
    String.concat "" (List.map (fun x -> string_of_int x) v)

  let jotstr2n s =
    let ts = "1" ^ s in
    let rec aux acc i =
      let acc =
        match String.get ts i with
        | '0' -> acc * 2
        | '1' -> (acc * 2) + 1
        | c -> failwith (Format.sprintf "Invalid Character: %c" c)
      in
      let i = i + 1 in
      if i >= String.length ts then acc else aux acc i
    in
    let res = aux 0 0 in
    (* Format.eprintf "%s -> %d -> %s\n" s res (n2jotstr res); *)
    assert (n2jotstr res = s);
    res

  let str_to_combinators = function
    | "S" -> `S
    | "K" -> `K
    | "I" -> `I
    | "s" -> `S
    | "k" -> `K
    | "i" -> `I
    | "D" -> `D
    (* | "i" -> `Iota *)
    | s when String.get s 0 = 'J' ->
        let s = String.sub s 1 (String.length s - 1) in
        `Jot (jotstr2n s)
    | x -> failwith ("Unknown combinator: " ^ x)

  let combinators_to_str = function
    | `S -> "S"
    | `K -> "K"
    | `I -> "I"
    | `D -> "D"
    | `Iota -> "i"
    | `Jot n -> n2jotstr n

  let size_of_pp = function
    | `S | `K | `I | `D | `Iota -> 1
    | `Jot n -> size_of_n2jotstr n

  let pp fmt c = Format.fprintf fmt "%s" (combinators_to_str c)

  let pp_com_str fmt v =
    Format.fprintf fmt "%s"
    @@ match v with `Com (c : t) -> combinators_to_str c | `Str s -> s

  let compare c d =
    match (c, d) with
    | `S, `S -> 0
    | `S, (`K | `I | `Jot _) -> 1
    | `K, `K -> 0
    | `K, (`I | `Jot _) -> 1
    | `I, `I -> 0
    | `I, `Jot _ -> 1
    | `Jot v, `Jot w -> Int.compare v w
    | _ -> -1
end

module ComFv = struct
  type t = [ `Com of Combinators.t | `Fv of int ]

  let pp fmt = function
    | `Fv x -> Format.fprintf fmt "a%d" x
    | `Com _ as v -> Combinators.pp_com_str fmt v

  module Order = struct
    type nonrec t = t

    let compare (m : t) (n : t) =
      match (m, n) with
      | `Com v, `Com w -> Combinators.compare v w
      | `Com _, `Fv _ -> 1
      | `Fv _, `Com _ -> -1
      | `Fv v, `Fv w -> Int.compare v w
  end
end

module ComStrFv = struct
  type t = [ `Com of Combinators.t | `Str of string | `Fv of int ]

  let pp fmt = function
    | `Fv x -> Format.fprintf fmt "a%d" x
    | (`Com _ | `Str (_ : string)) as v -> Combinators.pp_com_str fmt v

  module Order = struct
    type nonrec t = t

    let compare (m : t) (n : t) =
      match (m, n) with
      | `Com v, `Com w -> Combinators.compare v w
      | `Com _, (`Str _ | `Fv _) -> 1
      | `Str _, `Com _ -> -1
      | `Str s, `Str t -> String.compare s t
      | `Str _, `Fv _ -> 1
      | `Fv _, (`Com _ | `Str _) -> -1
      | `Fv v, `Fv w -> Int.compare v w
  end
end

module Combinator = struct
  type 'var combinator =
    | CVar of 'var
    | CApp of 'var combinator * 'var combinator

  let safe_pp pp_var =
    let rec aux fmt = function
      | CVar v -> Format.fprintf fmt "%a" pp_var v
      | CApp (m, n) -> Format.fprintf fmt "(%a %a)" aux m aux n
    in
    aux

  let pp pp_var =
    let rec aux fmt = function
      | CVar v -> Format.fprintf fmt "%a" pp_var v
      | CApp _ as x -> (
          let combs : 'a combinator list =
            let rec aux d =
              match d with CVar _ -> [ d ] | CApp (x, y) -> y :: aux x
            in
            aux x
          in
          (* XXX: Jot cannot be continuous thus should be wrapped with () *)
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

  let combinator_pp = pp
  let rec size = function CVar _ -> 1 | CApp (m, n) -> size m + size n

  let rec map f = function
    | CVar v -> CVar (f v)
    | CApp (m, n) -> CApp (map f m, map f n)

  let rec is_free v = function
    | CVar w -> v <> w
    | CApp (m, n) -> is_free v m && is_free v n

  let rec subst m v by =
    match m with
    | CVar w when v = w -> by
    | CVar _ -> m
    | CApp (m, n) -> CApp (subst m v by, subst n v by)

  let iota2skicomb m = CApp (CApp (m, CVar (`Com `S)), CVar (`Com `K))

  let jot2skicomb n =
    let v = Combinators.n2bin n in
    List.fold_left
      (fun acc x ->
        if x = 0 then CApp (CApp (acc, CVar (`Com `S)), CVar (`Com `K))
        else CApp (CVar (`Com `S), CApp (CVar (`Com `K), acc)))
      (CVar (`Com `I))
      v

  module ShortestPp = struct
    let pp_combinator = pp

    type 'a pp_wrapper =
      | Raw of 'a
      | Par of 'a pp_wrapper list (* Par内のデータは連続適用に従って逆向きでconsされているものとする *)
      | Grave of 'a pp_wrapper * 'a pp_wrapper
      | Star of 'a pp_wrapper * 'a pp_wrapper

    (* Need to ensure that size(Par(x.par)) >= size(x.nonpar) *)
    type 'a best_repr_with_parenthes =
      | Repr of {
          par : 'a pp_wrapper list;
          nonpar : 'a pp_wrapper;
          par_sealed : 'a pp_wrapper list;
          nonpar_sealed : 'a pp_wrapper;
        }

    let pp pp_var =
      let rec aux fmt = function
        | Raw v -> Format.fprintf fmt "%a" pp_var v
        | Par ps ->
            Format.fprintf fmt "(";
            List.iter (fun c -> Format.fprintf fmt "%a" aux c) (List.rev ps);
            Format.fprintf fmt ")"
        | Grave (x, y) -> Format.fprintf fmt "`%a%a" aux x aux y
        | Star (x, y) -> Format.fprintf fmt "*%a%a" aux x aux y
      in
      aux

    let pp_pp_wrapper = pp

    let pp_easyvalidate pp_var =
      let rec aux fmt = function
        | Raw v -> Format.fprintf fmt "{%a}" pp_var v
        | Par ps ->
            Format.fprintf fmt "(";
            List.iter (fun c -> Format.fprintf fmt "%a" aux c) (List.rev ps);
            Format.fprintf fmt ")"
        | Grave (x, y) -> Format.fprintf fmt "`%a%a" aux x aux y
        | Star (x, y) -> Format.fprintf fmt "*%a%a" aux x aux y
      in
      aux

    let size_of_wrapped_pp =
      let rec aux = function
        | Raw x -> Combinators.size_of_pp x
        | Par ps -> List.fold_left (fun acc x -> acc + aux x) 2 ps
        | Grave (l, r) | Star (l, r) -> aux l + aux r + 1
      in
      aux

    let is_valid_pp_wrapper =
      (* Returns (left_most_is_jot, rightmost_is_jot)*)
      let is_Iota r =
        match r with
        | Raw `Iota -> true
        | Raw _ | Par _ | Grave _ | Star _ -> false
      in
      let is_Jot_visible_left r =
        match r with
        | Raw (`Jot _) -> true
        | Raw _ | Par _ | Grave _ | Star _ -> false
      in
      let is_Jot_visible_right r =
        let rec aux = function
          | Raw (`Jot _) -> true
          | Raw _ | Par _ -> false
          | Grave (_, r) | Star (_, r) -> aux r
        in
        aux r
      in
      let rec aux (r : Combinators.t pp_wrapper) =
        match r with
        | Raw _ -> true
        | Par ps ->
            let rec aux = function
              | [ Raw `Iota ] -> false
              | [] | [ _ ] -> true
              | s :: (r :: _ as xs) ->
                  (* 逆向きconsのため *)
                  check_adjucent ~allow_iota:false r s && aux xs
            in
            aux ps
        | Grave (r, s) -> check_adjucent ~allow_iota:false r s
        | Star (r, s) -> check_adjucent ~allow_iota:true r s
      and check_adjucent ~allow_iota l r =
        aux l && aux r
        && (not (is_Jot_visible_right l && is_Jot_visible_left r))
        &&
        if allow_iota then is_Iota l || is_Iota r
        else not (is_Iota l || is_Iota r)
      in
      aux

    let validate_repr (Repr r) =
      match Repr r with
      | Repr
          {
            par = [ Raw `Iota ];
            nonpar = Raw `Iota;
            par_sealed = [ Raw `Iota ];
            nonpar_sealed = Raw `Iota;
          } ->
          ()
      | _ ->
          let pp = pp Combinators.pp in
          let ppe = pp_easyvalidate Combinators.pp in
          if not @@ is_valid_pp_wrapper r.nonpar then
            failwith
              (Format.asprintf "%a [as x.nonpar] is invalid repr" ppe r.nonpar);
          if not @@ is_valid_pp_wrapper (Par r.par) then
            failwith
              (Format.asprintf "%a [as x.par] is invalid repr" ppe (Par r.par));
          (let lp = size_of_wrapped_pp (Par r.par) in
           let lnp = size_of_wrapped_pp r.nonpar in
           if lp < lnp then
             failwith
               (Format.asprintf "%a [len:%d] is shorter than %a [len:%d]" pp
                  (Par r.par) lp pp r.nonpar lnp));
          (* Format.eprintf "OK: %a / %a\n" ppe (Par(r.par)) ppe r.nonpar; *)
          ()

    (* (Jot Jot Jot) のことなどを考えると、項をいい感じに()で包む必要がある。 *)
    (* `Jot単体 ... ()で包むか否か *)
    (* そうでないとき ... 左端は必ず非Jot。右端をJotにするか否かで最大1byte伸びる *)
    (* Jot連続可能性は CApp(x, CVar `Jot) に限る *)
    (* ↑の解決法は、xの右端を非Jotにする or 右を()で包む、の2択  *)

    let shortest_wrapper =
      let smaller_repr r1 r2 =
        let s1 = size_of_wrapped_pp r1 in
        let s2 = size_of_wrapped_pp r2 in
        if s1 <= s2 then r1 else r2
      in
      let smaller_reprs r1 r2 =
        let s1 = size_of_wrapped_pp (Par r1) in
        let s2 = size_of_wrapped_pp (Par r2) in
        if s1 <= s2 then r1 else r2
      in
      (* 効率を無視して最小のreprを返すようにする *)
      let rec aux (c : Combinators.t combinator) :
          Combinators.t best_repr_with_parenthes =
        let res =
          match c with
          | CVar v ->
              let r = Raw v in
              let sr = match v with `Jot _ -> Par [ r ] | _ -> r in
              Repr
                {
                  par = [ r ];
                  nonpar = r;
                  par_sealed = [ sr ];
                  nonpar_sealed = sr;
                }
          | CApp ((CVar `Iota as x), y) | CApp (x, (CVar `Iota as y)) ->
              let (Repr x) = aux x in
              let (Repr y) = aux y in
              let r = Star (x.nonpar, y.nonpar) in
              let sr = Star (x.nonpar, y.nonpar_sealed) in
              Repr
                {
                  par = [ r ];
                  nonpar = r;
                  par_sealed = [ sr ];
                  nonpar_sealed = sr;
                }
          | CApp (x, CVar (`Jot _ as y)) ->
              let (Repr x) = aux x in
              let y = Raw y in
              let par_sealed = Par [ y ] :: x.par in
              let par = smaller_reprs par_sealed (y :: x.par_sealed) in

              let p1 = Grave (x.nonpar, Par [ y ]) in
              let p2 = Grave (Par x.par, y) in
              let p3 = Grave (x.nonpar_sealed, y) in
              let p4 = Par par in
              let nonpar =
                smaller_repr (smaller_repr p1 p2) (smaller_repr p3 p4)
              in
              let nonpar_sealed = smaller_repr p1 p4 in

              Repr { par; nonpar; par_sealed; nonpar_sealed }
          | CApp (x, y) ->
              let (Repr x) = aux x in
              let (Repr y) = aux y in

              let par = y.nonpar :: x.par in
              let par_sealed = y.nonpar_sealed :: x.par in

              let p = Par par in
              let nonpar = smaller_repr p (Grave (x.nonpar, y.nonpar)) in
              let nonpar_sealed =
                smaller_repr p (Grave (x.nonpar, y.nonpar_sealed))
              in

              Repr { par; nonpar; par_sealed; nonpar_sealed }
        in
        validate_repr res;
        res
      in
      aux

    type one_or_two = One | Two

    type size =
      | Size of {
          par : int;
          nonpar : int;
          (* topmost repr is right_jot_free *)
          par_sealed : int;
          (* repr is right_jot_free *)
          nonpar_sealed : int;
        }

    let pp_size fmt (Size s) =
      Format.fprintf fmt "{par=%d; nonpar=%d; par_sealed=%d; nonpar_sealed=%d}"
        s.par s.nonpar s.par_sealed s.nonpar_sealed

    let validate_size c (Size s) =
      let pp = pp Combinators.pp in
      let (Repr c_wrapped) = shortest_wrapper c in
      (let ps = Format.asprintf "%a" pp c_wrapped.nonpar in
       let l = String.length ps in
       if l <> s.nonpar then
         failwith
           (Format.asprintf
              "Size for %a [len:%d] [nonpar] is miscalculated as %d [%a]" pp
              c_wrapped.nonpar l s.nonpar pp_size (Size s)));

      (let pss =
         List.rev_map (fun c -> Format.asprintf "%a" pp c) c_wrapped.par
       in
       let ps = String.concat "" pss in
       let l = String.length ps in
       if l <> s.par then
         failwith
           (Format.asprintf
              "Size for Par(%s) [len:%d] [par] is miscalculated as %d [%a]" ps l
              s.par pp_size (Size s)));

      assert (s.par <= s.par_sealed);
      assert (s.par_sealed <= s.par + 2);

      assert (s.nonpar <= s.nonpar_sealed);
      assert (s.par_sealed <= s.nonpar_sealed);

      assert (s.par <= s.nonpar);
      assert (s.nonpar <= s.par + 2);
      assert (s.nonpar_sealed <= s.par + 2);

      ()

    let size =
      let rec aux (c : Combinators.t combinator) : size =
        let res =
          match c with
          | CVar (`Jot x) ->
              let l = List.length @@ Combinators.n2bin x in
              Size
                {
                  par = l;
                  nonpar = l;
                  par_sealed = l + 2;
                  nonpar_sealed = l + 2;
                }
          | CVar _ ->
              Size { par = 1; nonpar = 1; par_sealed = 1; nonpar_sealed = 1 }
          | CApp ((CVar `Iota as x), y) | CApp (x, (CVar `Iota as y)) ->
              let (Size x) = aux x in
              let (Size y) = aux y in
              let l = x.nonpar + y.nonpar + 1 in
              (* Star (x.nonpar, y.nonpar) *)
              let sl = x.nonpar + y.nonpar_sealed + 1 in
              Size { par = l; nonpar = l; par_sealed = sl; nonpar_sealed = sl }
          | CApp (x, CVar (`Jot y)) ->
              let (Size x) = aux x in
              let y = List.length @@ Combinators.n2bin y in
              (*
              y :: tx :: xs
              Par([y]) :: x :: xs
              y :: x.par　when not @@ when is_Jot_visible_right x
            *)
              let par = min (y + x.par_sealed) (y + 2 + x.par) in
              let par_sealed = y + 2 + x.par in
              (*
              Grave(x.nonpar,(Par([y])))
              Grave(Par(x.par),y)
              Grave(fst @@ seal_jot x.nonpar, y)
              Grave(x.nonpar, y) when not @@ when is_Jot_visible_right x.nonpar
            *)
              let nonpar =
                let p1 = 1 + x.nonpar + 2 + y in
                (* let p2 = 1 + 2 + x.par + y in *)
                let p3 = 1 + x.nonpar_sealed + y in
                (* p3 includes p2 *)
                let p2 = p1 in
                min (min p1 p2) (min p3 (par + 2))
              in
              let nonpar_sealed = min (1 + x.nonpar + 2 + y) (par + 2) in
              Size { par; nonpar; par_sealed; nonpar_sealed }
          | CApp (x, y) ->
              let (Size x) = aux x in
              let (Size y) = aux y in
              (* (y.nonpar :: x.par) *)
              let par = y.nonpar + x.par in
              let par_sealed = y.nonpar_sealed + x.par in
              (*
              (Par(par))
              (Grave (x.nonpar,y.nonpar))
            *)
              let nonpar = min (2 + par) (1 + x.nonpar + y.nonpar) in
              let nonpar_sealed =
                min (2 + par) (1 + x.nonpar + y.nonpar_sealed)
              in
              Size { par; nonpar; par_sealed; nonpar_sealed }
        in
        (* Format.eprintf "size of %a is %a\n"
           (pp_combinator Combinators.pp)
           c pp_size res; *)
        validate_size c res;
        res
      in
      aux

    let approx_size c =
      let c = map (function `Str _ | `Fv _ -> `D | `Com c -> c) c in
      match size c with Size { par; _ } -> par + 2

    let str_based_size m =
      String.length (Format.asprintf "%a" (combinator_pp ComStrFv.pp) m)

    let pp fmt m =
      let (Repr r) = shortest_wrapper m in
      pp Combinators.pp fmt r.nonpar
  end

  let combinator_to_str m = Format.asprintf "%a" (pp Combinators.pp) m

  module Order (V : Set.OrderedType) = struct
    type t = V.t combinator

    let rec compare m n =
      match (m, n) with
      | CVar v, CVar w -> V.compare v w
      | CVar _, CApp _ -> -1
      | CApp _, CVar _ -> 1
      | CApp (m1, m2), CApp (n1, n2) ->
          let x = compare m1 n1 in
          if x <> 0 then x else compare m2 n2
  end
end

module DeBruijn = struct
  type t = { size : int; v : v }
  and v = Var of int | Abs of t | App of t * t

  let _Var v = { size = 1; v = Var v }
  let _Abs m = { size = m.size + 1; v = Abs m }
  let _App m n = { size = m.size + n.size + 1; v = App (m, n) }

  (* let size = function
     | Var _ -> 1
     | Abs m -> size m + 1
     | App (m, n) -> size m + size n + 1 *)

  let size m = m.size

  let to_hash =
    let rec aux fmt m =
      match m with
      | Var v -> Format.fprintf fmt "%d" v
      | Abs m -> Format.fprintf fmt ".%a" aux m.v
      | App (m, n) -> Format.fprintf fmt "(%a %a)" aux m.v aux n.v
    in
    fun fmt m -> aux fmt m.v

  let xor_shift_rand x =
    let x = x lxor (x lsl 13) in
    let x = x lxor (x lsr 7) in
    let x = x lxor (x lsl 17) in
    x

  let to_xor_hash =
    let next_hash x =
      (* (x + 123546789) mod 2356234135347 *)
      xor_shift_rand x
    in
    let rec aux m =
      match m with
      | Var v -> next_hash ((next_hash v lsl 2) lxor 1)
      | Abs m -> next_hash ((aux m.v lsl 2) lxor 2)
      | App (m, n) -> next_hash (((aux m.v + (123 * aux n.v)) lsl 2) lxor 3)
    in
    fun m -> aux m.v

  let of_lambda =
    let index v =
      let rec aux d = function
        | [] -> -1
        | x :: xs -> if x = v then d else aux (d + 1) xs
      in
      aux 0
    in
    let rec aux env = function
      | Lambda.Var v -> _Var (index v env)
      | Lambda.Abs (v, m) -> _Abs (aux (v :: env) m)
      | Lambda.App (m, n) -> _App (aux env m) (aux env n)
    in
    fun m -> aux [] m

  let is_0_free =
    let rec aux d = function
      | Var x -> if x = d then false else true
      | Abs m -> aux (d + 1) m.v
      | App (m, n) -> aux d m.v && aux d n.v
    in
    fun m -> aux 0 m.v

  let remove_eta m =
    let rec aux d = function
      | Var x when x > d -> _Var (x - 1)
      | Var x when x < d -> _Var x
      | Var _ -> assert false
      | Abs m -> _Abs (aux (d + 1) m.v)
      | App (m, n) -> _App (aux d m.v) (aux d n.v)
    in
    aux 0 m.v

  let lift_large_fv d =
    let rec aux nd = function
      | Var x when x >= nd -> _Var (x + d)
      | Var x -> _Var x
      | Abs m -> _Abs (aux (nd + 1) m.v)
      | App (m, n) -> _App (aux nd m.v) (aux nd n.v)
    in
    aux 0

  (* Lift not substituted Vars *)
  let beta m n =
    let rec aux d m =
      match m with
      | Var x when x = d -> lift_large_fv d n.v
      | Var x when x < d -> _Var x
      | Var x (* when x > d *) -> _Var (x - 1)
      | Abs m -> _Abs (aux (d + 1) m.v)
      | App (m, n) -> _App (aux d m.v) (aux d n.v)
    in
    aux 0 m.v

  let reduce_beta_one_step modified m =
    let rec beta_aux m =
      let { v; _ } = m in
      match v with
      | Var _ -> m
      | App ({ v = Abs m; _ }, n) ->
          modified := true;
          beta m n
      | Abs m -> _Abs (beta_aux m)
      | App (m, n) -> _App (beta_aux m) (beta_aux n)
    in
    beta_aux m

  let reduce_eta_one_step modified m =
    let rec eta_aux m =
      let { v; _ } = m in
      match v with
      | Var _ -> m
      | Abs { v = App (m, { v = Var 0; _ }); _ } when is_0_free m ->
          modified := true;
          remove_eta m
      | Abs m -> _Abs (eta_aux m)
      | App (m, n) -> _App (eta_aux m) (eta_aux n)
    in
    eta_aux m

  exception StepLimit

  let gen_reduce modified reduce_one_step m =
    let base_size = size m in
    let step = ref 0 in

    (* let _pp fmt m = Format.fprintf fmt "%s" (to_hash m) in *)
    let rec loop m =
      if size m > base_size * 3 then raise StepLimit;
      if !step > 1000 then raise StepLimit;
      step := !step + 1;
      let modified_local = ref false in
      match reduce_one_step modified_local m with
      | None -> None
      | Some tm ->
          (* Format.eprintf "Reduction: %a => %a@." _pp m _pp tm; *)
          if !modified_local then (
            modified := true;
            loop tm)
          else Some m
    in
    try
      let tm = loop m in
      (* Format.eprintf "Reduction: %a => %a\n" pp m pp tm ; *)
      tm
    with StepLimit -> (* Format.eprintf "Reduction Timeout: %a\n" pp m; *)
                      None

  let reduce_beta modified =
    gen_reduce modified (fun modified m ->
        Some (reduce_beta_one_step modified m))

  let reduce_eta modified =
    gen_reduce modified (fun modified m ->
        Some (reduce_eta_one_step modified m))

  let reduce_beta_eta =
    let modified = ref false in
    gen_reduce modified (fun modified m ->
        Option.bind (reduce_eta modified m) (reduce_beta modified))
  (* let reduce_beta_eta m = Some m *)

  let rec of_combs =
    let ls =
      _Abs
        (_Abs (_Abs (_App (_App (_Var 2) (_Var 0)) (_App (_Var 1) (_Var 0)))))
    in
    let lk = _Abs (_Abs (_Var 1)) in
    let li = _Abs (_Var 0) in
    let liota = _Abs (_App (_App (_Var 0) ls) lk) in

    fun (c : Combinators.t) ->
      match c with
      | `S -> ls
      | `K -> lk
      | `I -> li
      | `Jot n ->
          let x = Combinator.jot2skicomb n in
          of_comb x
      | `Iota -> liota
      | `D -> assert false

  and of_comb c =
    let open Combinator in
    match c with
    | CVar (`Com c) -> of_combs c
    | CVar (`Fv x) ->
        assert (x < 0);
        _Var x
    | CApp (m, n) -> _App (of_comb m) (of_comb n)
end
