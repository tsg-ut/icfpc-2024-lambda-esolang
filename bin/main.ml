open Lambda_esolang
module L = Library

let parse lexbuf = Parser.main Lexer.token lexbuf

module Args = struct
  let no_opt = ref false
  let partial_opt = ref false

  let speclist = [
    ("-noopt", Arg.Set no_opt, "no optimization");
    ("-popt", Arg.Set partial_opt,
      "partial optimization. Expressions surrounded with {} will be optimized");
  ]
end


let _ =
  Arg.parse Args.speclist (fun _ -> ()) "";
  let lexbuf = Lexing.from_channel stdin in
  let res = parse lexbuf in
  Format.eprintf "Inputted: %a\n" Syntax.(Lambda.pp Combinators.pp_com_str) res;
  let res =
    if !Args.partial_opt then begin
      let res = Ski.ski_allow_str res in
      Optimize.optimize_only_annot res
    end else begin
      let res = Ski.ski res in
      if !Args.no_opt then res else Optimize.optimize res
    end
  in
  (* let res = Syntax.Combinator.combinator_to_str res in *)
  Format.printf "%a\n" Syntax.Combinator.ShortestPp.pp res;
  ()
