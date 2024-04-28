open Lambda_esolang

let test_ski_length () =
  Alcotest.(check int)
    ""
    (String.length @@ Syntax.Combinators.combinators_to_str `K)
    1

let () =
  let open Alcotest in
  run "Utils" [ ("ski", [ test_case "Lower case" `Quick test_ski_length ]) ]
