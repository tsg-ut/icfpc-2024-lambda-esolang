open Lambda_esolang
module L = Library

let parse lexbuf = ParserLazyK.main LexerLazyK.token lexbuf

let _ =
  Logs.set_reporter
    (Logs.format_reporter
       ~pp_header:(fun fmt (level, _) ->
         Format.fprintf fmt "[%a] " Logs.pp_level level)
       ());
  Logs.set_level (Some Logs.Info);
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf in
  Logs.info (fun a ->
      a "Inputted: %a" Syntax.(Combinator.pp Combinators.pp_com_str) res);
  let _res = Decompiler.decompile res in
  ()
