open Interpreter
open Utils
open Syntax
open Syntax.Combinator
module _ = Optimize

type func = Unary of (int -> int) | Binary of (int -> int -> int)

(* Based on https://yshl.hatenadiary.com/entry/20081006/1223304302 *)

let b_based_churchnum_table () =
  assert false;
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
  List.iter
    (fun (f, m) ->
      match f with
      | Unary f ->
          let rec fori i =
            let tf = f i in
            if tf < n && i < n then (
              tst.(tf) <- (i, i, m) :: tst.(tf);
              fori (i + 1))
            else ()
          in
          fori 1
      | Binary f ->
          let rec fori i =
            (let rec forj j =
               let tf = f i j in
               if tf < n && j < n then (
                 tst.(tf) <- (i, j, m) :: tst.(tf);
                 forj (j + 1))
               else ()
             in
             forj 1);
            if i < n then fori (i + 1)
          in
          fori 1)
    converts;
  Format.eprintf "Make btable\n";
  flush_all ();
  (* assert false; *)
  let btable = Array.make n (s2comb "I") in
  btable.(0) <- s2comb "K(KI)";
  btable.(1) <- s2comb "KI";
  btable.(2) <- s2comb "SSJ0";
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
               let tm = subst (subst m (`Str "m") ta) (`Str "n") tb in
               (* let tm = Optimize.com_str_to_com tm in
                  let tm = Optimize.optimize tm in
                  let tm = Optimize.com_to_com_str tm in *)
               let tl =
                 String.length
                   (Format.asprintf "%a" (pp Combinators.pp_com_str) tm)
               in
               match res with
               | None -> Some (tm, tl)
               | Some (_, bl) -> if bl > tl then Some (tm, tl) else res)
           None
    in
    let tm, _ = Option.get tm in
    Format.eprintf "%d %a\n" i (pp Syntax.Combinators.pp_com_str) tm;
    flush_all ();
    btable.(i) <- tm
  done;
  btable

let b_based_churchnum_table = cached b_based_churchnum_table

let shortest_churchnum_table =
  cached @@ fun () ->
  let bcom = s2comb "(S(KS)K)" in
  Array.map (fun d -> CApp (d, bcom)) (b_based_churchnum_table ())

let n2charchnum n =
  if n < Array.length (shortest_churchnum_table ()) then
    Interpreter.com_to_lambda @@ (shortest_churchnum_table ()).(n)
  else
    let open Syntax.Lambda in
    Format.eprintf "Generate non-optimized church num: %d\n" n;
    let rec aux n = if n = 0 then Var "z" else App (Var "s", aux (n - 1)) in
    Lambda.map (fun v -> `Str v) @@ Abs ("s", Abs ("z", aux n))

(* To be implemented for Unlambda compiler *)

(* #2進数で実装したほうがいいらしい
   #32bit固定のほうが楽らしい
   #true,false の組でやって、末尾をvにする *)

(* def n2lam(n,bl=$binlength)
   	if $ischurchnum then
   		return s2lam('s. z.' + '(s' * n + ' z' + ')' * n)
   	end
   	#s = 's. z.' + '(s' * n + ' z' + ')' * n
   	def rec(n,d,bl)
   		if d == bl then 'V'
   		elsif n % 2 > 0 then
   			'($cons $t ' + rec(n/2,d+1,bl) + ')'
   		else
   			'($cons $f ' + rec(n/2,d+1,bl) + ')'
   		end
   	end
   	s2lam(rec(n,0,bl))
   end *)
