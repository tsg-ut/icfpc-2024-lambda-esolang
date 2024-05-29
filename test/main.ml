open Lambda_esolang
open Generator
open Utils

let test_shortest_combs_pp () =
  let rand = R.make [| 314 |] in
  let ppp = Syntax.(Combinator.ShortestPp.pp_pp_wrapper Combinators.pp) in
  for _ = 1 to 10000 do
    let c = random_comb ~rand ~random_var:random_ski ~maxsize:6 in
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
  let pp = Syntax.(Combinator.ShortestPp.pp_pp_wrapper Combinators.pp) in
  for _ = 1 to 10000 do
    let c = random_comb ~rand ~random_var:random_ski ~maxsize:30 in
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

let test_decompiler () =
  let rand = R.make [| 314 |] in
  let pp = Syntax.(Combinator.safe_pp ComStrFv.pp) in
  for _idx = 1 to 10000 do
    let c = random_comb ~rand ~random_var:random_ski ~maxsize:4 in

    let c = Syntax.Combinator.map (fun c -> `Com c) c in
    let m = Decompiler.decompile c in

    let m =
      Syntax.Lambda.map
        (function `Str s -> `Str s | `Fv i -> `Str (Format.sprintf "v%d" i))
        m
    in
    let tc = Ski.ski_allow_str m in
    let tc =
      Syntax.Combinator.map (function `Com c -> `Com c | `Str s -> `Str s) tc
    in

    match Optimize.generate_behavior_hash c with
    | None -> ()
    | Some h ->
        let th = Optimize.generate_behavior_hash tc in
        let s = Format.asprintf "%a" pp c in
        Alcotest.(check (option string)) s (Some h) th
  done

let test_reduce_lambda () =
  let rand = R.make [| 314 |] in
  let pp = Syntax.(Lambda.pp ComStrFv.pp) in

  let hash m =
    let m =
      Syntax.(Lambda.map (fun (`Fv v) -> `Str (Format.sprintf "v%d" v)) m)
    in
    let c = Ski.ski m in
    let c = Syntax.(Combinator.map (fun c -> `Com c) c) in
    Optimize.generate_behavior_hash c
  in
  for _idx = 1 to 10000 do
    let _ =
      let m = random_lambda ~rand ~random_var:random_fv ~max_depth:6 in

      let* tm = Interpreter.reduce_lambda m in
      let* h = hash m in
      let th = hash tm in
      let s = Format.asprintf "%a -> %a" pp m pp tm in
      Some (Alcotest.(check (option string)) s (Some h) th)
    in
    ()
  done

let _ = (test_size_of_pp, test_shortest_combs_pp, test_decompiler)

let () =
  let open Alcotest in
  run "Utils"
    [
      ( "ski",
        [
          (* test_case "size_of_pp" `Quick test_size_of_pp; *)
          (* test_case "shortest_combs_pp" `Slow test_shortest_combs_pp; *)
          test_case "decompiler" `Quick test_decompiler;
          test_case "fv_safe_subst" `Quick test_reduce_lambda;
        ] );
    ]
