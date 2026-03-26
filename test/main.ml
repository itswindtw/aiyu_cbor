let () =
  let open Alcotest in
  run "aiyu_cbor"
    [
      Unint64_cases.(M.make_suite "Unint64" examples);
      Int_cases.(M.make_suite "Int" examples);
      Int64_cases.(M.make_suite "Int64" examples);
      Additional_cases.suite;
    ]
