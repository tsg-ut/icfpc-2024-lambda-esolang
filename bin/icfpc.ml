open Lambda_esolang

let parse lexbuf = Parsericfpc.main Lexericfpc.token lexbuf

module Args = struct

  let speclist =
    [    ]
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


  Logs.info (fun a ->
      a "Inputted: %a" Syntax.(Lambda.pp Icfpc.pp) res);
  ()
