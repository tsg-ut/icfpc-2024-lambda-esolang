open Lambda_esolang

let parse lexbuf = ParsericfpcHand.main LexericfpcHand.token lexbuf

module Args = struct
  let for_golf = ref false
  let speclist = [
    ("-g", Arg.Set for_golf, "Golfer");
  ]
end

let _ =
  Logs.set_reporter
    (Logs.format_reporter
       ~pp_header:(fun fmt (level, _) ->
         Format.fprintf fmt "[%a] " Logs.pp_level level)
       ());
  Logs.set_level (Some Logs.Info);

  Arg.parse Args.speclist (fun _ -> ()) "";
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf in

  Logs.info (fun a -> a "Inputted: %a" Syntax.(Lambda.pp Icfpc.pp) res);

  (* Logs.info (fun a -> a "Converted: %a" Syntax.icfpc_prog_pp res); *)
  Format.printf "%a" Syntax.icfpc_prog_pp res;
  ()
