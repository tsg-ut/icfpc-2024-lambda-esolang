(* 多分既存の実装を見つけたほうがよい *)

let rec pow x y =
  if y = 0 then 1
  else
    let d = pow x (y / 2) in
    d * d * if y mod 2 = 1 then x else 1

let pp_option pp_var fmt v =
  match v with
  | None -> Format.fprintf fmt "None"
  | Some v -> Format.fprintf fmt "Some(%a)" pp_var v

let pp_string fmt s = Format.fprintf fmt "%s" s

let cached gen =
  let v = ref None in
  fun () ->
    match !v with
    | None ->
        let tv = gen () in
        v := Some tv;
        tv
    | Some v -> v

let ( let* ) = Option.bind
let ( let+ ) v f = Option.map f v

(* 書いたけど誤読していて本家も十分早そうなのであった *)
(* module type FastHashtbl = sig
     type b = int
     type 'a t
     val create : b -> 'a t
     val length : 'a t -> int
     val find_opt : 'a t -> b -> 'a option
     val replace : 'a t -> b -> 'a -> unit
   end

   module FastHashtbl : FastHashtbl = struct
     type b = int
     type 'a t = {
       v: (int,'a) Hashtbl.t array;
       len: int
     }

     let create l =
       let res = Array.make l (Hashtbl.create 0) in
       for i = 0 to (l-1); do
         res.(i) <- Hashtbl.create 3
       done;
       { v=res; len=l }

     let length tbl =
       Array.fold_left (fun acc v -> Hashtbl.length v + acc) 0 tbl.v

     let i2hi tbl i = ((i mod tbl.len) + tbl.len) mod tbl.len

     let find_opt tbl i =
       let hi = i2hi tbl i in
       Hashtbl.find_opt tbl.v.(hi) i

     let replace tbl i x =
       let hi = i2hi tbl i in
       Hashtbl.replace tbl.v.(hi) i x
   end *)
