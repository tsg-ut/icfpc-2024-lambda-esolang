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

module Combinator = struct
  type combinators = [ `S | `K | `I | `Jot of int ]

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

  let combinators_to_str = function
    | `S -> "S"
    | `K -> "K"
    | `I -> "I"
    | `Jot n -> n2jotstr n

  let pp_combinators fmt c = Format.fprintf fmt "%s" (combinators_to_str c)

  let pp_ski_str fmt v =
    Format.fprintf fmt "%s"
    @@
    match v with
    | `Com c -> (
        match c with `S -> "S" | `K -> "K" | `I -> "I" | `Jot n -> n2jotstr n)
    | `Str s -> s

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
      | CVar (`Jot x) -> List.length @@ n2bin x
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

  let combinator_to_str m = Format.asprintf "%a" (pp pp_combinators) m

  let rec subst m v by =
    match m with
    | CVar w -> if v = w then by else m
    | CApp (m, n) -> CApp (subst m v by, subst n v by)
end
