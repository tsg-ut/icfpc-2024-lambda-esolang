open Lambda_esolang

let parse lexbuf = Parser.main Lexer.token lexbuf

module Args = struct
  let no_opt = ref false
  let partial_opt = ref false

  let speclist =
    [
      ("-noopt", Arg.Set no_opt, "no optimization");
      ( "-popt",
        Arg.Set partial_opt,
        "partial optimization. Expressions surrounded with {} will be optimized"
      );
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

  Library.load_library ();

  Logs.info (fun a ->
      a "Inputted: %a" Syntax.(Lambda.pp Combinators.pp_com_str) res);
  let res =
    if !Args.partial_opt then
      let res = Ski.ski_allow_str res in
      Optimize.optimize_only_annot res
    else if !Args.no_opt then
      let res = Ski.ski_allow_str res in
      let res =
        Syntax.Combinator.map
          (function
            | `Str "optimize" -> `I
            | `Str s -> failwith @@ "Not-converted free variable " ^ s
            | `Com c -> c)
          res
      in
      res
    else
      let res = Ski.ski res in
      Optimize.optimize res
  in
  (* let res = Syntax.Combinator.combinator_to_str res in *)
  Format.printf "%a\n" Syntax.Combinator.ShortestPp.pp res;
  ()
