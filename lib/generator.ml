module R = Random.State

module Choice = struct
  let list ~rand v =
    if List.length v = 0 then failwith "Choice from empty list";
    let l = List.length v in
    List.nth v (R.int rand l)
end

let random_ski ~rand =
  match R.int rand 3 with 0 -> `S | 1 -> `Iota | _ -> `Jot (R.int rand 6 + 2)

let _random_com_str_fv ~rand =
  match R.int rand 1 with
  | 0 -> `Com (random_ski ~rand)
  | 1 -> `Str (string_of_int @@ R.int rand 3)
  | _ -> `Fv (R.int rand 3)

let random_fv ~rand = `Fv (R.int rand 10)

let random_comb_fv ~neg_fv =
  let fv_rand rand =
    if neg_fv then (-1 * R.int rand 10) - 1 else R.int rand 10
  in
  fun ~rand ->
    if R.bool rand then `Com (random_ski ~rand) else `Fv (fv_rand rand)

let random_comb ~rand ~size:s ~random_var =
  let open Syntax.Combinator in
  let rec aux size =
    if size = 0 then CVar (random_var ~rand)
    else
      let n = R.int rand size in
      CApp (aux n, aux (size - n - 1))
  in
  aux s

let random_comb ~rand ~random_var ~maxsize =
  let size = R.int rand maxsize in
  random_comb ~rand ~size ~random_var

let random_lambda ~rand ~random_var ~depth =
  let open Syntax.Lambda in
  let random_icom () =
    let v = random_var ~rand in
    Abs (v, Var v)
  in
  let random_varExpr env =
    if env <> [] then Var (Choice.list ~rand env) else random_icom ()
  in
  let rec aux depth env =
    if depth <= 0 then random_varExpr env
    else
      let r = R.int rand 105 in
      if r < 5 then random_varExpr env
      else if r < 55 then
        let v = random_var ~rand in
        Abs (v, aux (depth - 1) (v :: env))
      else if r < 105 then App (aux (depth - 1) env, aux (depth - 1) env)
      else assert false
  in
  aux depth []

let random_lambda ~rand ~random_var ~max_depth =
  let depth = R.int rand max_depth + 3 in
  random_lambda ~rand ~random_var ~depth

let enumerate_all_possible_reprs =
  let open Syntax.Combinator in
  let open ShortestPp in
  let gen_pars cs =
    List.filter_map
      (fun v -> match v with Par _ -> None | _ -> Some (Par [ v ]))
      cs
  in
  let rec aux = function
    | CVar v -> [ Raw v; Par [ Raw v ] ]
    | CApp (x, y) ->
        let x = aux x in
        let y = aux y in
        let x = x @ gen_pars x in
        let y = y @ gen_pars y in
        let tl =
          List.concat_map
            (fun y ->
              List.concat_map
                (fun x ->
                  (match x with Par ps -> [ Par (y :: ps) ] | _ -> [])
                  @ [ Grave (x, y); Star (x, y); Par [ y; x ] ])
                x)
            y
        in
        let tl = List.filter is_valid_pp_wrapper tl in
        tl
  in
  aux
