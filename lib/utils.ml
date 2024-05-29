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
