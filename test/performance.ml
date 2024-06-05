open Lambda_esolang
open Generator

let measure f v =
  let open Mtime_clock in
  let open Mtime in
  let st = now () in
  f v;
  let tt = now () in
  let ns = span st tt |> Span.to_float_ns in
  ns

let average v = List.fold_left ( +. ) 0.0 v /. (float_of_int @@ List.length v)

let measure_ntimes f v ~times =
  let res = List.init times (fun _ -> measure f v) in
  average res

let measure_list f vs ~times =
  let res = List.map (fun v -> measure_ntimes f v ~times) vs in
  average res

let measure_hash_time () =
  let rand = R.make [| 314 |] in
  let ms =
    List.init 10000 (fun _ ->
        random_comb ~rand ~random_var:(random_comb_fv ~neg_fv:true) ~maxsize:30)
  in

  let hash_ns f str =
    let ns =
      measure_list
        (fun m ->
          let _ = f m in
          ())
        ms ~times:10
    in
    Logs.info (fun a -> a "%s: %f" str ns)
  in

  hash_ns Optimize.generate_behavior_hash "Simple Hash";
  hash_ns Optimize.generate_beta_eta_xor_behavior_hash "Beta Eta Xor Hash";
  hash_ns Optimize.generate_beta_eta_behavior_hash "Beta Eta Hash";
  ()

let () =
  Logs.set_reporter
    (Logs.format_reporter
       ~pp_header:(fun fmt (level, _) ->
         Format.fprintf fmt "[%a] " Logs.pp_level level)
       ());
  Logs.set_level (Some Logs.Info);

  measure_hash_time ()
