open Lambda_esolang
module R = Random.State

let random_ski ~rand =
  match R.int rand 3 with 0 -> `S | 1 -> `Iota | _ -> `Jot (R.int rand 2 + 2)

let _random_com_str_fv ~rand =
  match R.int rand 1 with
  | 0 -> `Com (random_ski ~rand)
  | 1 -> `Str (string_of_int @@ R.int rand 3)
  | _ -> `Fv (R.int rand 3)

let random_comb ~rand ~size:s ~random_var =
  let open Syntax.Combinator in
  let rec aux size =
    if size = 0 then CVar (random_var ~rand)
    else
      let n = R.int rand size in
      CApp (aux n, aux (size - n - 1))
  in
  aux s

let random_comb ~rand ~random_var =
  let size = R.int rand 5 in
  random_comb ~rand ~size ~random_var

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

let test_shortest_combs_pp () =
  let rand = R.make [| 314 |] in
  let ppp = Syntax.(Combinator.ShortestPp.pp Combinators.pp) in
  for _ = 1 to 10000 do
    let c = random_comb ~rand ~random_var:random_ski in
    let all_pps = enumerate_all_possible_reprs c in
    let open Syntax.Combinator.ShortestPp in
    let (Repr best_pp) = shortest_wrapper c in

    let best_size = size_of_wrapped_pp best_pp.nonpar in

    List.iter
      (fun repr ->
        if is_valid_pp_wrapper repr then
          let repr_size = size_of_wrapped_pp repr in
          if repr_size < best_size then
            Alcotest.failf "%a [len:%d] has shorter repr %a [len:%d]" ppp
              best_pp.nonpar best_size ppp repr repr_size)
      all_pps
  done

let test_size_of_pp () =
  let rand = R.make [| 314 |] in
  let pp = Syntax.(Combinator.ShortestPp.pp Combinators.pp) in
  for _ = 1 to 10000 do
    let c = random_comb ~rand ~random_var:random_ski in
    let (Repr c_wrapped) = Syntax.Combinator.ShortestPp.shortest_wrapper c in
    (let s = Format.asprintf "%a" pp c_wrapped.nonpar in
     let l1 = String.length s in
     let (Size l2) = Syntax.Combinator.ShortestPp.size c in
     Alcotest.(check int) s l1 l2.nonpar);
    let s = List.map (fun c -> Format.asprintf "%a" pp c) c_wrapped.par in
    let l1 = List.fold_left (fun acc s -> acc + String.length s) 0 s in
    let (Size l2) = Syntax.Combinator.ShortestPp.size c in
    let s = "Par(" ^ String.concat "" s ^ ")" in
    Alcotest.(check int) s l1 l2.par
  done

let () =
  let open Alcotest in
  run "Utils"
    [
      ( "ski",
        [
          test_case "size_of_pp" `Quick test_size_of_pp;
          test_case "shortest_combs_pp" `Slow test_shortest_combs_pp;
        ] );
    ]
