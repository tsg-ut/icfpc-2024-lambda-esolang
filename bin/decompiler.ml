open Lambda_esolang
module L = Library

let parse lexbuf = ParserLazyK.main LexerLazyK.token lexbuf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf in
  Format.eprintf "Inputted: %a\n"
    Syntax.(Combinator.pp Combinators.pp_com_str)
    res;
  let res = Decompiler.decompile res in
  Format.printf "%s\n" res;
  ()
