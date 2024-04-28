open Lambda_esolang
module L = Library

let parse lexbuf = Parser.main Lexer.token lexbuf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf in
  Format.eprintf "Inputted: %a\n" Syntax.(Lambda.pp Combinators.pp_com_str) res;
  let res = Ski.ski res in
  let res = Optimize.optimize res in
  let res = Syntax.Combinator.combinator_to_str res in
  Format.printf "%s\n" res;
  ()
