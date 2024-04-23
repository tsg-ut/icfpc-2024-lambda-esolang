open Interpreter
open Utils
open Syntax.Combinator

type func = Unary of (int -> int) | Binary of (int -> int -> int)

(* Based on https://yshl.hatenadiary.com/entry/20081006/1223304302 *)

let b_based_churchnum_table =
  let converts =
    [
      (Unary (fun n -> n + 1), s2comb "(S S n)");
      (Binary (fun m n -> m + n), s2comb "(S (S m S) n)");
      (Binary (fun m n -> n * m), s2comb "(S (S I m) n)");
      (* (Unary (fun n -> n * n), s2comb "(S (S S J0) n)"); *)
      (Binary (fun m n -> pow m n), s2comb "(S n m)");
      (Unary (fun m -> pow m m), s2comb "(S S I m)");
      (Unary (fun n -> n * (n + 1)), s2comb "(S (S S S) n)");
    ]
  in

  Format.eprintf "Converters\n";
  let n = 1300 in
  let tst = Array.init n (fun _ -> []) in
  for i = 1 to n - 1 do
    List.iter
      (fun (f, m) ->
        match f with
        | Unary f ->
            let tf = f i in
            if 0 <= tf && tf < n then tst.(tf) <- (i, i, m) :: tst.(tf)
        | Binary _ -> ())
      converts;
    for j = 1 to n - 1 do
      List.iter
        (fun (f, m) ->
          match f with
          | Unary _ -> ()
          | Binary f ->
              let tf = f i j in
              if 0 <= tf && tf < n then tst.(tf) <- (i, j, m) :: tst.(tf))
        converts
    done
  done;
  Format.eprintf "Make etable\n";
  let btable = Array.make n (s2comb "I") in
  btable.(0) <- s2comb "K(KI)";
  btable.(1) <- s2comb "KI";
  for i = 2 to n - 1 do
    let tm =
      tst.(i)
      |> List.fold_left
           (fun res (a, b, m) ->
             if a >= i || b >= i then res
             else
               (* Format.eprintf "%d <- %d %d | %a\n" i a b (pp_option (pp_combinator pp_string)) res; *)
               let ta = btable.(a) in
               let tb = btable.(b) in
               let tm = subst (subst m "m" ta) "n" tb in
               let tl =
                 String.length
                   (Format.asprintf "%a" (pp pp_string) tm)
               in
               match res with
               | None -> Some (tm, tl)
               | Some (_, bl) -> if bl > tl then Some (tm, tl) else res)
           None
    in
    let tm, _ = Option.get tm in
    Format.eprintf "%d %a\n" i (pp pp_string) tm;
    btable.(i) <- tm
  done;
  btable

let shortest_churchnum_table =
  let bcom = s2comb "(S(KS)K)" in
  Array.map (fun d -> CApp (d, bcom)) b_based_churchnum_table

let n2charchnum n =
  if n < Array.length shortest_churchnum_table then
    Interpreter.scomb_to_lambda @@ shortest_churchnum_table.(n)
  else (
    let open Syntax.Lambda in
    Format.eprintf "Generate non-optimized church num: %d\n" n;
    let rec aux n = if n = 0 then Var "z" else App (Var "s", aux (n - 1)) in
    map (fun v -> `Str v) @@ Abs ("s", Abs ("z", aux n)))