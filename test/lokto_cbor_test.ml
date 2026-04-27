module Make (I : sig
  include Lokto_cbor.Integer

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end) =
struct
  module Cbor = Lokto_cbor.Make (I)

  module Cbor_derived = struct
    type t = Cbor.t =
      | Integer of I.t
      | Simple of int
      | Float of (float[@equal Float.equal])
      | Byte_string of string
      | Text_string of string
      | Array of t list
      | Map of (t * t) list
      | Tag of I.t * t
    [@@deriving show, eq]
  end

  let testable = Alcotest.testable Cbor_derived.pp Cbor_derived.equal

  let make_suite suite_name examples =
    let make_test (input_hex, expected_value, expected_hex) =
      let expected_hex = Option.value ~default:input_hex expected_hex in

      Alcotest.test_case input_hex `Quick (fun () ->
          let raw =
            `Hex (String.sub input_hex 2 (String.length input_hex - 2))
            |> Hex.to_string
          in

          let decoded =
            match Cbor.decode raw with
            | Ok actual ->
                Alcotest.check testable input_hex expected_value actual;
                actual
            | Error msg -> Alcotest.failf "decode %s error: %s" input_hex msg
          in

          let encoded = Cbor.encode decoded in
          let actual_hex = Hex.of_bytes encoded |> Hex.show in

          Alcotest.(check string) input_hex expected_hex ("0x" ^ actual_hex))
    in
    (suite_name, List.map make_test examples)
end
