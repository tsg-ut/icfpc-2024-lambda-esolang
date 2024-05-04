module Lambda = struct
  type 'var lambda =
    | Abs of 'var * 'var lambda
    | App of 'var lambda * 'var lambda
    | Var of 'var

  let pp_var fmt v = Format.fprintf fmt "%s" v

  let pp pp_var =
    let rec aux fmt = function
      | Var v -> Format.fprintf fmt "%a" pp_var v
      | Abs (v, m) -> Format.fprintf fmt "(%a. %a)" pp_var v aux m
      | App (m, n) -> Format.fprintf fmt "(%a %a)" aux m aux n
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
    | "D" -> `D
    | "i" -> `Iota
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
    @@ match v with `Com c -> combinators_to_str c | `Str s -> s

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

module ComStrFv = struct
  type com_str_fv = [ `Com of Combinators.t | `Str of string | `Fv of int ]

  let pp fmt = function
    | `Fv x -> Format.fprintf fmt "a%d" x
    | (`Com _ | `Str (_ : string)) as v -> Combinators.pp_com_str fmt v

  module Order = struct
    type t = com_str_fv

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

  module ShortestPp = struct
    let pp_combinator = pp

    type 'a pp_wrapper =
      | Raw of 'a
      | Par of 'a pp_wrapper list (* Par内のデータは連続適用に従って逆向きでconsされているものとする *)
      | Grave of 'a pp_wrapper * 'a pp_wrapper
      | Star of 'a pp_wrapper * 'a pp_wrapper

    (* Need to ensure that size(Par(x.par)) >= size(x.nonpar) *)
    type 'a best_repr_with_parenthes =
      | Repr of { par : 'a pp_wrapper list; nonpar : 'a pp_wrapper }

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
      | Repr { par = [ Raw `Iota ]; nonpar = Raw `Iota } -> ()
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
      let is_Jot_visible_right r =
        let rec aux = function
          | Raw (`Jot _) -> true
          | Raw _ | Par _ -> false
          | Grave (_, r) | Star (_, r) -> aux r
        in
        aux r
      in
      let seal_jot =
        (* Assume that given repr is jot_visible_right *)
        let rec aux = function
          | Raw (`Jot _) as x -> (Par [ x ], 2)
          | Raw _ | Par _ -> assert false
          | Grave (x, y) -> (Par [ y; x ], 1)
          | Star (Raw `Iota, y) ->
              let ty, d = aux y in
              (Star (Raw `Iota, ty), d)
          | Star _ -> assert false
        in
        aux
      in
      let smaller_repr r1 r2 =
        let s1 = size_of_wrapped_pp r1 in
        let s2 = size_of_wrapped_pp r2 in
        if s1 <= s2 then r1 else r2
      in
      (* 効率を無視して最小のreprを返すようにする *)
      let rec aux (c : Combinators.t combinator) :
          Combinators.t best_repr_with_parenthes =
        let res =
          match c with
          | CVar v ->
              let r = Raw v in
              Repr { par = [ r ]; nonpar = r }
          | CApp ((CVar `Iota as x), y) | CApp (x, (CVar `Iota as y)) ->
              let (Repr x) = aux x in
              let (Repr y) = aux y in
              let r = Star (x.nonpar, y.nonpar) in
              Repr { par = [ r ]; nonpar = r }
          | CApp (x, CVar (`Jot _ as y)) ->
              let (Repr x) = aux x in
              let y = Raw y in
              let par =
                match x.par with
                | x :: xs when is_Jot_visible_right x ->
                    let tx, d = seal_jot x in
                    if d < 2 then y :: tx :: xs else Par [ y ] :: x :: xs
                | _ -> y :: x.par
              in
              let nonpar =
                let p1 = Grave (x.nonpar, Par [ y ]) in
                let p2 = Grave (Par x.par, y) in
                let p3 =
                  if is_Jot_visible_right x.nonpar then
                    Grave (fst @@ seal_jot x.nonpar, y)
                  else Grave (x.nonpar, y)
                in
                smaller_repr (smaller_repr p1 p2) p3
              in
              let nonpar = smaller_repr (Par par) nonpar in
              Repr { par; nonpar }
          | CApp (x, y) ->
              let (Repr x) = aux x in
              let (Repr y) = aux y in
              let par = y.nonpar :: x.par in
              let nonpar =
                smaller_repr (Par par) (Grave (x.nonpar, y.nonpar))
              in
              Repr { par; nonpar }
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
                let p2 = 1 + 2 + x.par + y in
                let p3 = 1 + x.nonpar_sealed + y in
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
        Format.eprintf "size of %a is %a\n"
          (pp_combinator Combinators.pp)
          c pp_size res;
        validate_size c res;
        res
      in
      aux
  end

  let combinator_to_str m = Format.asprintf "%a" (pp Combinators.pp) m

  let rec map f = function
    | CVar v -> CVar (f v)
    | CApp (m, n) -> CApp (map f m, map f n)

  let rec subst m v by =
    match m with
    | CVar w when v = w -> by
    | CVar _ -> m
    | CApp (m, n) -> CApp (subst m v by, subst n v by)

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
