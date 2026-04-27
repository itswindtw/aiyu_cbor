module M = Lokto_cbor_test.Make (Unint64_cases.Unint64)

let decode_ok s =
  match M.Cbor.decode s with
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok but got Error: %s" e

let decode_err s =
  match M.Cbor.decode s with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error but got Ok"

let suite =
  let open Alcotest in
  ( "Additional_cases",
    [
      test_case "text string in byte string" `Quick (fun () ->
          decode_err "0x5f6141ff");
      test_case "byte string in text string" `Quick (fun () ->
          decode_err "0x7f4141ff");
      test_case "nested indefinite" `Quick (fun () -> decode_err "0x5f5fffff");
      test_case "missing BREAK" `Quick (fun () -> decode_err "0x5f4141");
      test_case "trailing bytes" `Quick (fun () -> decode_err "0x0101");
      test_case "map with odd length" `Quick (fun () ->
          decode_err "0xbf010203ff");
      test_case "integer roundtrip" `Quick (fun () ->
          let values =
            let open M.Cbor in
            [
              Integer (U 0L);
              Integer (U 1L);
              Integer (U 10L);
              Integer (N 0L);
              Integer (N 1L);
              Integer (N 10L);
            ]
          in

          List.iter
            (fun v ->
              let encoded = M.Cbor.encode v |> Bytes.to_string in

              match M.Cbor.decode encoded with
              | Ok v' ->
                  if v <> v' then
                    Alcotest.failf "roundtrip mismatch: %s"
                      (M.Cbor_derived.show v)
              | Error e -> Alcotest.failf "unexpected decode error: %s" e)
            values);
    ] )
