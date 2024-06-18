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

  let msLen = 10000 in
  let times = 10 in
  let ms =
    List.init msLen (fun _ ->
        random_comb ~rand ~random_var:(random_comb_fv ~neg_fv:true) ~maxsize:30)
  in

  let hash_ns f str =
    let ns =
      measure_list
        (fun m ->
          let _ = f m in
          ())
        ms ~times
    in
    (* Logs.info (fun a -> a "%s: %f [ns]" str ns) *)
    Logs.info (fun a -> a "%s: %e [s]" str (ns /. 1000000000.))
  in

  hash_ns Optimize.generate_behavior_hash "Simple Hash";
  hash_ns Optimize.generate_beta_eta_xor_behavior_hash "Beta Eta Xor Hash";
  hash_ns Optimize.generate_beta_eta_behavior_hash "Beta Eta Hash";
  ()

let measure_enumerate_ski_time () =
  let measure f str =
    let ns =
      measure
        (fun _ ->
          let _ = f () in
          ())
        ()
    in
    (* Logs.info (fun a -> a "%s: %f [ns]" str ns) *)
    Logs.info (fun a -> a "%s: %f [s]" str (ns /. 1000000000.))
  in

  let open Optimize in
  let gen_old ~size =
    enumerate_ski_with_size_old ~hasher:generate_beta_eta_xor_behavior_hash
      ~size ~fvn:2
  in
  let gen ~size =
    enumerate_ski_with_size ~hasher:generate_beta_eta_xor_behavior_hash ~size
      ~fvn:2
  in
  let _ = (gen, gen_old) in
  for size = 6 to 13 do
    measure
      (fun () -> gen ~size)
      (Format.sprintf "Faster enumerate ski: size %d" size)
    (* measure (fun () -> gen_old ~size)
       (Format.sprintf "BaseLine enumerate ski: size %d" size); *)
  done;

  (let open Syntax.Combinator in
   let size = 260000 in
   let v = ref (CVar 3) in
   measure
     (fun () ->
       for _ = 1 to size do
         v := CApp (!v, CVar 3)
       done)
     (Format.sprintf "Compare: Cons loop: %d" size);

   Logs.info (fun a -> a "vsize: %d" (Syntax.Combinator.size !v)));
  ()

let () =
  Logs.set_reporter
    (Logs.format_reporter
       ~pp_header:(fun fmt (level, _) ->
         Format.fprintf fmt "[%a] " Logs.pp_level level)
       ());
  Logs.set_level (Some Logs.Info);

  let _ = (measure_hash_time, measure_enumerate_ski_time) in
  (* measure_hash_time (); *)
  measure_enumerate_ski_time ();
  ()
