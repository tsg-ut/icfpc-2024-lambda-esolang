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
  type t = [ `S | `K | `I | `D | `Jot of int ]
  type com_str = [ `Com of t | `Str of string ]

  let n2bin n =
    assert (n >= 2);
    let rec aux n =
      let b = n mod 2 in
      if n <= 1 then [ b ] else aux (n / 2) @ [ b ]
    in
    match aux n with 1 :: v -> v | _ -> assert false

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
    | s when String.get s 0 = 'J' ->
        let s = String.sub s 1 (String.length s - 1) in
        `Jot (jotstr2n s)
    | x -> failwith ("Unknown combinator: " ^ x)

  let combinators_to_str = function
    | `S -> "S"
    | `K -> "K"
    | `I -> "I"
    | `D -> "D"
    | `Jot n -> n2jotstr n

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

  let size_of_pp =
    let rec aux = function
      | CVar (`Jot x) -> List.length @@ Combinators.n2bin x
      | CVar _ -> 1
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
