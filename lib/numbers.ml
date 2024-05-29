open Interpreter
open Utils
open Syntax
open Syntax.Combinator
module _ = Optimize

type func = Unary of (int -> int) | Binary of (int -> int -> int)

(* Based on https://yshl.hatenadiary.com/entry/20081006/1223304302 *)

let b_based_churchnum_table () =
  (* assert false; *)
  let converts =
    [
      (Unary (fun n -> n + 1), "(n + 1)", s2comb "(S S n)");
      (Binary (fun m n -> m + n), "(m + n)", s2comb "(S (S m S) n)");
      (Binary (fun m n -> n * m), "(n * m)", s2comb "(S (S I m) n)");
      (* (Unary (fun n -> n * n), s2comb "(S (S S J0) n)"); *)
      (Binary (fun m n -> pow m n), "(pow m n)", s2comb "(S n m)");
      (Unary (fun m -> pow m m), "((x. pow x x) m)", s2comb "(S S I m)");
      ( Unary (fun n -> n * (n + 1)),
        "((x. x * (x + 1)) m)",
        s2comb "(S (S S S) n)" );
    ]
  in

  (* Format.eprintf "Converters\n"; *)
  let n = 100 in
  let tst = Array.init n (fun _ -> []) in
  List.iter
    (fun (f, s, m) ->
      match f with
      | Unary f ->
          let rec fori i =
            let tf = f i in
            if tf < n && i < n then (
              let s = s ^ Format.sprintf " %d" i in
              tst.(tf) <- (i, i, s, m) :: tst.(tf);
              fori (i + 1))
            else ()
          in
          fori 1
      | Binary f ->
          let rec fori i =
            (let rec forj j =
               let tf = f i j in
               if tf < n && j < n then (
                 let s = s ^ Format.sprintf " %d %d" i j in
                 tst.(tf) <- (i, j, s, m) :: tst.(tf);
                 forj (j + 1))
               else ()
             in
             forj 1);
            if i < n then fori (i + 1)
          in
          fori 1)
    converts;
  (* Format.eprintf "Make btable\n"; *)
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
           (fun res (a, b, s, m) ->
             if a >= i || b >= i then res
             else
               (* Format.eprintf "%d <- %d %d | %a\n" i a b (pp_option (pp_combinator pp_string)) res; *)
               let ta = btable.(a) in
               let tb = btable.(b) in
               let tm = subst (subst m (`Str "m") ta) (`Str "n") tb in

               (* let tm = Optimize.com_str_to_com tm in
                  let tm = Optimize.optimize tm in
                  let tm = Optimize.com_to_com_str tm in *)
               let tl = ShortestPp.approx_size tm in
               (* let tl = ShortestPp.str_based_size tm in *)
               (* let tl =
                    String.length
                      (Format.asprintf "%a" (pp Combinators.pp_com_str) tm)
                  in *)
               match res with
               | None -> Some (tm, s, tl)
               | Some (_, _, bl) -> if bl > tl then Some (tm, s, tl) else res)
           None
    in
    let tm, s, _ = Option.get tm in
    Logs.info (fun a ->
        a "%d %a (%s)" i (pp Syntax.Combinators.pp_com_str) tm s);
    flush_all ();
    btable.(i) <- tm
  done;
  btable

let b_based_churchnum_table = cached b_based_churchnum_table

let shortest_churchnum_table =
  cached @@ fun () ~add_bcom ->
  let bcom = s2comb "(S(KS)K)" in
  Array.map
    (fun d -> if add_bcom then CApp (d, bcom) else d)
    (b_based_churchnum_table ())

let n2charchnum n ~add_bcom =
  if n < Array.length (shortest_churchnum_table () ~add_bcom) then
    Interpreter.com_to_lambda @@ (shortest_churchnum_table () ~add_bcom).(n)
  else
    let open Syntax.Lambda in
    Logs.info (fun a -> a "Generate non-optimized church num: %d" n);
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
