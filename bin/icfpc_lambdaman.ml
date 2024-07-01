open Lambda_esolang
module R = Random.State

let parse lexbuf = ParsericfpcHand.main LexericfpcHand.token lexbuf

module Args = struct
  let problem_idx = ref (-1)
  let speclist = [ ("-i", Arg.Int (fun i -> problem_idx := i), "ProblemIdx") ]
end

module Problem = struct
  type pos = {x: int; y: int}
  type t = {
    board: string array;
    h: int;
    w: int;
    pegnum: int;
    start: pos
  }

  let getc ss y x = String.get (ss.(y)) x

  let load ss =
    let ss = Array.map (fun s -> "#" ^ s ^ "#") ss in
    let ss =
      let s = Array.make 1 (String.make (String.length ss.(0)) '#') in
      Array.concat [s; ss; s]
    in
    let h = Array.length ss in
    let w = String.length (ss.(0)) in
    let pegnum = 
      Array.fold_left (fun r s ->
        String.fold_left (fun r c -> r + if c = '.' then 1 else 0) r s)
      1 ss
    in
    let start =
      let rec aux y x =
        if getc ss y x = 'L' then {y;x}
        else begin
          if x = 0 then
            if y = 0 then assert false
            else aux (y-1) (w-1)
          else aux y (x-1)
        end
      in
        aux (h-1) (w-1)
    in
    {board=ss;h;w;start;pegnum }
  
  let getc data y x = getc data.board y x

  let check_string data s =
    (* if String.length s > 1 then (0,0) else *)
    let np = data.start in
    let gone = Array.make_matrix data.h data.w false in
    gone.(data.start.y).(data.start.x) <- true;
    let cnt = ref 1 in
    let _tp = String.fold_left (fun p c ->
      let x = p.x + (match c with 'L' -> -1 | 'R' -> 1 | _ -> 0) in
      let y = p.y + (match c with 'U' -> -1 | 'D' -> 1 | _ -> 0) in
      let tp = {y;x} in
      (* Format.eprintf "%d,%d -> %d,%d / %d@." p.y p.x y x !cnt; *)
      if getc data y x = '#' then p
      else (
        (if gone.(y).(x) then () else begin
          gone.(y).(x) <- true;
          cnt := !cnt + 1;
        end);
        tp
      ) 
    ) np s in
    (!cnt,data.pegnum)

end

let load_problem i =
  let ic = open_in (Format.sprintf "testcases/lambdaman%d" i) in
  let rec aux acc = 
    try
      let s = input_line ic in
      aux (s :: acc)
    with
      | End_of_file -> acc
  in
  let d = aux [] |> List.rev |> List.filter (fun s -> s <> "") |> Array.of_list in
  close_in ic;
  Problem.load d

let _ =
  Logs.set_reporter
    (Logs.format_reporter
       ~pp_header:(fun fmt (level, _) ->
         Format.fprintf fmt "[%a] " Logs.pp_level level)
       ());
  Logs.set_level (Some Logs.Info);

  Arg.parse Args.speclist (fun _ -> ()) "";
  let problem = load_problem !Args.problem_idx in

  let _ = parse in
  
  let rand = R.make [| 31415 |] in
  let _random_walk ~rand =
    (* Your solution may consist of at most `1,000,000` characters. *)
    String.init 1_000_000 (fun _ -> 
      let ci = R.int rand 4 in
      String.get "LRUD" ci
    )
  in
  let search_size = 100 in
  let random_walk_w_seed ~rand =
    let mdiv, genfun = 
    match !Args.problem_idx with
      | 4 -> 
          (* 1, fun x -> (1, String.sub "LRUD" (x mod 4) 1) *)
          1, fun x -> (((x mod 8) + 1),String.sub "LURDDRUL" 0 ((x mod 8) + 1))
          (* 1, fun x -> ((x mod 8),String.sub "LURDDRU" 0 (x mod 8)) *)
      | 5 -> 
          1, fun x -> (((x mod 7) + 1),String.sub "LURDDRU" 0 ((x mod 7) + 1))
      | 13 ->
          2, fun x -> (4, String.sub "LLUURRDDLL" ((x mod 4) * 2) 4)
      | 20 ->
          2, fun x -> (4, String.sub "LLUULLDDRRUURRDDLL" ((x mod 8) * 2) 4)
      | _ -> Format.eprintf "Default: Lambda man setting@.";
          1, fun x -> (1, String.sub "LRUD" (x mod 4) 1)
    in
    (* Your solution may consist of at most `1,000,000` characters. *)
    (* let st = R.int rand 100 in *)
    let st = R.int rand (94/mdiv) in
    let a = R.int rand 94 in
    (* let a = R.int 100000 in *)
    let n = R.int rand 1_000_00_00 in
    (* let n = R.int rand ((94 * 94 * 94)/mdiv) in *)
    if n = 0 then ("","") else
    (* let gone = Array.make (n+10) false in *)

    let minb = ref (-1) in
    let minbl = ref (-1) in
    let minb2 = ref (-1) in
    let minbl2 = ref (-1) in
    let minb3 = ref (-1) in
    let minbl3 = ref (-1) in

    let rec aux x ls acc =
      let tx = (x * a) mod n in
      let is_gone =
        tx = st
          (* if gone.(tx) then true else begin
            gone.(tx) <- true;
            false
          end *)
      in
      let ns, s = genfun x in
      let ci = x mod 4 in
      let _ = ci in
      (* let ns, s = ci+1, String.sub "LURD" 0 (ci+1) in *)
      (* let s = String.sub "LRUD" ci (4-ci) in *)
      (* let ns,s = 1, String.sub "LRUD" ci 1 in *)
      (* let ns,s = 1, String.sub "LURD" (x mod 4) 1 in *)
      (* let ns,s = 2, String.sub "LLUURRDD" ((x mod 4) * 2) 2 in *)
      (* let ns,s = 4, (List.nth [
        "LLUU";
        "LLDD";
        "RRUU";
        "RRDD";
        "UULL";
        "UURR";
        "DDLL";
        "DDRR"
       ] (x mod 8))  in *)

      (* let ns,s = 4, String.sub "LLUULLDDRRUURRDDLL" ((x mod 8) * 2) 4 in *)
      (* let ns,s = 2, String.sub "LULDRURDL" (x mod 8) 2 in *)

      (* let ns,s = 4, String.sub "LLUULLDDRRUURR" ((x mod 6) * 2) 4 in *)
      (* let ns,s = 4, String.sub "LLUULLDDRRUURRDDLL" (x mod 15) 4 in *)
      (* let ns,s = 4, String.sub "LLLLUUUURRRRDDDDLLLL" (x mod 15) 4 in *)
      (* let ns,s = 4, String.sub "LLLLUUUURRRRDDDDLLLL" ((x mod 8) * 2) 2 in *)
      (* let ns,s = 2, String.sub "LLUURRDDLL" (x mod 8) 2 in *)
      (* let ns,s = 3, String.sub "LLLUUURRRDDDLLL" (x mod 12) 3 in *)
      (* let ns,s =
        if x mod 8 > 3 then
          1, String.sub "LURD" (x mod 4) 1
        else
          (* 2, String.sub "LLLUUURRRDDD" ((x mod 4) * 3) 3  *)
          2, String.sub "LLUURRDD" ((x mod 4) * 2) 2
      in *)
      (* let ns,s = 5, String.sub "LURD" (x mod 4) 1 ^ "LRUD" in *)
      (* let ns,s = 9, String.sub "LURD" (x mod 4) 1 ^ "LRRLUDDU" in *)
      (* let ns,s = 1, (x mod 4) in *)
      (* let ns,s = 2, String.sub "LLLUUURRRDDDLL" (x mod 12) 3 in *)
      (* let s = String.sub "RDLLUURRDDUL" 0 ((x mod 8)+2) in *)
      let ts = s :: acc in
      let tls = ls + ns in
      (* let tls = ls + 1 in *)
      if is_gone || tls >= 1_000_000 then begin
        (* Format.eprintf "Gen@."; *)
        (* let rs = String.concat "" (List.rev acc) in *)
        let rs = String.concat "" acc in
        (* let vs = Array.of_list (List.rev ts) in
        let rs = String.init (Array.length vs) 
          (fun i -> let ci = vs.(i) in 
            if ci = 0 then 'L'
            else if ci = 1 then 'R'
            else if ci = 2 then 'U'
            else 'D'
              ) in *)
        (* let un = String.fold_left (fun acc c -> acc + if c = 'U' then 1 else 0) 0 rs in
        let dn = String.fold_left (fun acc c -> acc + if c = 'D' then 1 else 0) 0 rs in
        let ln = String.fold_left (fun acc c -> acc + if c = 'L' then 1 else 0) 0 rs in
        let rn = String.fold_left (fun acc c -> acc + if c = 'R' then 1 else 0) 0 rs in
        Format.eprintf "Fin concat %d %d / %d %d@." un dn ln rn; *)

        let ml = (min (String.length rs) 10) in 
        (rs, Format.sprintf "Maze%d(%d,%d,%d -> %d / %s(len %d)(b %d / bl %d)(b2 %d / l2 %d)(b3 %d / bl3 %d))"
          mdiv
          (st*mdiv) a (n*mdiv) (x*mdiv) 
          (String.sub rs (String.length rs - ml) ml) ls 
          (!minb*mdiv) !minbl (!minb2*mdiv) !minbl2 (!minb3*mdiv) !minbl3)

        (* (rs, Format.sprintf "(%d,%d,%d -> %d / %s(len %d)(b %d / bl %d)(b2 %d / l2 %d)(b3 %d / bl3 %d))"
          st a n x (String.sub rs (String.length rs - ml) ml) ls !minb !minbl !minb2 !minbl2 !minb3 !minbl3) *)
      end else begin
        (
          (* if tx < 94 then begin  *)
          if tx < (94/mdiv) then begin
            minb := tx;
            minbl := tls
          end
        );
        (
          (* if tx < 94 then begin  *)
          if tx < (94*94/mdiv) then begin
            minb2 := tx;
            minbl2 := tls
          end
        );
        (
          (* if tx < 94 then begin  *)
          if tx < (94*94*94/mdiv) then begin
            minb3 := tx;
            minbl3 := tls
          end
        );
        aux tx tls ts
      end
    in
      aux st 0 []
  in
  let res = 
    let r = ref [] in
    for _ = 1 to search_size; do
      let s,ps = random_walk_w_seed ~rand in
      let ts = s in
      (* Format.eprintf "%s@." (String.sub ts 0 10); *)
      (* let ts = String.concat "" (List.init 1 (fun _ -> s)) in *)
      let (cnt,all) = Problem.check_string problem ts in
      let d = (ps,cnt,all) in 
      r := d :: !r
      (* Format.eprintf "%s => %d/%d@." s cnt all *)
    done;
    !r
  in
  
  (* Lam 19 *)
  (* let lam_19 () =
    let dup n d =
      String.make n (String.get "LURD" (d mod 4))
    in
    let rec gen n =
      if n = 0 then "" else
      let rec aux i =
        if i = 0 then "" else
        (dup n i) ^
        gen (n/2) ^
        (dup n (i+2)) ^
        aux (i-1)
      in
        aux 4
    in
      let res = gen 1024 in
      (res,"gen19")
  in
  let res = [
    let s,ps = lam_19 () in
    let ts = s in
    (* Format.eprintf "%s@." (String.sub ts 0 10); *)
    (* let ts = String.concat "" (List.init 1 (fun _ -> s)) in *)
    let (cnt,all) = Problem.check_string problem ts in
    let d = (ps,cnt,all) in
    d
  ] in *)
  
  (* Lam 20 *)
  (* let lam_20 () =
    let dup d n =
      String.make n (String.get "URDL" ((d + 10000) mod 4))
    in
    let rec gen i n =
      if n = 0 then "" else
      (dup i n) ^
      (gen (i+3) (n/4)) ^ 
      (dup i n) ^
      (gen (i-1) (n/2)) ^ 
      (dup (i+1) 1) ^
      (gen i (n/2)) ^ 
      (dup (i+2) 1) ^
      (gen (i+1) (n/2)) ^ 
      (dup (i+3) 1) ^

      (dup (i+2) (n-1)) ^
      (
        if n > 1 then begin
          (dup (i+1) 1) ^
          (gen (i+1) (n/4)) ^ 
          (dup (i+3) 1) ^
          ""
        end else ""
      ) ^

      (dup (i+2) n)
    in
      let res = begin
        (gen 0 32) ^
        dup 2 1 ^
        (gen 1 32) ^
        dup 3 1 ^
        (gen 2 32) ^
        dup 0 1 ^
        (gen 3 32) ^
        dup 1 1 ^
        ""
      end in
      (res,"gen20")
  in
  let res = [
    let s,ps = lam_20 () in
    let ts = s in
    (* Format.eprintf "%s@." (String.sub ts 0 10); *)
    (* let ts = String.concat "" (List.init 1 (fun _ -> s)) in *)
    let (cnt,all) = Problem.check_string problem ts in
    let d = (ps,cnt,all) in
    d
  ] in *)
  let res = List.sort (fun (_,c1,_) (_,c2,_) -> Int.compare c1 c2) res |> List.rev in
  let res = List.filteri (fun i _ -> i < 10) res in
  List.iter (fun (s,cnt,all) ->
    Format.eprintf "%s => %d/%d@." s cnt all
  ) res;
  ()
