open Lambda_esolang
open Interpreter

let test_ski_length () =
  Alcotest.(check int) "" (String.length @@ combinators_to_str `K) 1

let () =
  let open Alcotest in
  run "Utils" [ ("ski", [ test_case "Lower case" `Quick test_ski_length ]) ]
