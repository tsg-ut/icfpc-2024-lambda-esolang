open Interpreter
module L = Library

let parse lexbuf = Parser.main Lexer.token lexbuf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf in
  Format.eprintf "Inputted: %a\n" (pp_lambda pp_var) res;
  let res = combinator_to_str @@ simplify @@ ski res in
  Format.printf "%s\n" res;
  ()
