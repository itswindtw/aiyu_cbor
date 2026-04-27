module Int64 = struct
  type t = int64 [@@deriving show, eq]

  let compare = Int64.compare

  (* decode *)
  let of_uint uint = Int64.of_int uint
  let of_uint32 uint32 = Some Int64.(of_int32 uint32 |> logand 0xffffffffL)

  let of_uint64 uint64 =
    if Int64.compare uint64 0L >= 0 then Some uint64 else None

  let of_nint nint = Int64.of_int (-1 - nint)

  let of_nint32 nint32 =
    let n = Int64.(of_int32 nint32 |> logand 0xffffffffL) in
    Some (Int64.sub (-1L) n)

  let of_nint64 nint64 =
    if Int64.compare nint64 0L >= 0 then Some (Int64.sub (-1L) nint64) else None

  (* encode *)
  let to_uint64 t = if Int64.compare t Int64.zero >= 0 then Some t else None

  let to_nint64 t =
    if Int64.compare t Int64.zero < 0 then Some Int64.(neg (add t one))
    else None
end

module M = Lokto_cbor_test.Make (Int64)

let examples =
  let open M.Cbor in
  [
    ("0x00", Integer 0L, None);
    ("0x01", Integer 1L, None);
    ("0x0a", Integer 10L, None);
    ("0x17", Integer 23L, None);
    ("0x1818", Integer 24L, None);
    ("0x1819", Integer 25L, None);
    ("0x1864", Integer 100L, None);
    ("0x1903e8", Integer 1000L, None);
    ("0x1a000f4240", Integer 1000000L, None);
    ("0x1b000000e8d4a51000", Integer 1000000000000L, None);
    (* (Integer 18446744073709551615), "0x1bffffffffffffffff", None); *)
    ( "0xc249010000000000000000",
      Tag (2L, Byte_string "\x01\x00\x00\x00\x00\x00\x00\x00\x00"),
      None );
    (* (Integer (-18446744073709551616), "0x3bffffffffffffffff", None); *)
    ( "0xc349010000000000000000",
      Tag (3L, Byte_string "\x01\x00\x00\x00\x00\x00\x00\x00\x00"),
      None );
    ("0x20", Integer (-1L), None);
    ("0x29", Integer (-10L), None);
    ("0x3863", Integer (-100L), None);
    ("0x3903e7", Integer (-1000L), None);
    ("0xf90000", Float 0.0, None);
    ("0xf98000", Float (-0.0), None);
    ("0xf93c00", Float 1.0, None);
    ("0xfb3ff199999999999a", Float 1.1, None);
    ("0xf93e00", Float 1.5, None);
    ("0xf97bff", Float 65504.0, None);
    ("0xfa47c35000", Float 100000.0, None);
    ("0xfa7f7fffff", Float 3.4028234663852886e+38, None);
    ("0xfb7e37e43c8800759c", Float 1.0e+300, None);
    ("0xf90001", Float 5.960464477539063e-8, None);
    ("0xf90400", Float 0.00006103515625, None);
    ("0xf9c400", Float (-4.0), None);
    ("0xfbc010666666666666", Float (-4.1), None);
    ("0xf97c00", Float Float.infinity, None);
    ("0xf97e00", Float Float.nan, None);
    ("0xf9fc00", Float Float.neg_infinity, None);
    ("0xfa7f800000", Float Float.infinity, Some "0xf97c00");
    ("0xfa7fc00000", Float Float.nan, Some "0xf97e00");
    ("0xfaff800000", Float Float.neg_infinity, Some "0xf9fc00");
    ("0xfb7ff0000000000000", Float Float.infinity, Some "0xf97c00");
    ("0xfb7ff8000000000000", Float Float.nan, Some "0xf97e00");
    ("0xfbfff0000000000000", Float Float.neg_infinity, Some "0xf9fc00");
    ("0xf4", Simple 20, None);
    ("0xf5", Simple 21, None);
    ("0xf6", Simple 22, None);
    ("0xf7", Simple 23, None);
    ("0xf0", Simple 16, None);
    ("0xf8ff", Simple 255, None);
    ( "0xc074323031332d30332d32315432303a30343a30305a",
      Tag (0L, Text_string "2013-03-21T20:04:00Z"),
      None );
    ("0xc11a514b67b0", Tag (1L, Integer 1363896240L), None);
    ("0xc1fb41d452d9ec200000", Tag (1L, Float 1363896240.5), None);
    ("0xd74401020304", Tag (23L, Byte_string "\x01\x02\x03\x04"), None);
    ("0xd818456449455446", Tag (24L, Byte_string "\x64\x49\x45\x54\x46"), None);
    ( "0xd82076687474703a2f2f7777772e6578616d706c652e636f6d",
      Tag (32L, Text_string "http://www.example.com"),
      None );
    ("0x40", Byte_string "", None);
    ("0x4401020304", Byte_string "\x01\x02\x03\x04", None);
    ("0x60", Text_string "", None);
    ("0x6161", Text_string "a", None);
    ("0x6449455446", Text_string "IETF", None);
    ("0x62225c", Text_string "\"\\", None);
    ("0x62c3bc", Text_string "ü", None);
    ("0x63e6b0b4", Text_string "水", None);
    ("0x64f0908591", Text_string "𐅑", None);
    ("0x80", Array [], None);
    ("0x83010203", Array [ Integer 1L; Integer 2L; Integer 3L ], None);
    ( "0x8301820203820405",
      Array
        [
          Integer 1L;
          Array [ Integer 2L; Integer 3L ];
          Array [ Integer 4L; Integer 5L ];
        ],
      None );
    ( "0x98190102030405060708090a0b0c0d0e0f101112131415161718181819",
      Array
        [
          Integer 1L;
          Integer 2L;
          Integer 3L;
          Integer 4L;
          Integer 5L;
          Integer 6L;
          Integer 7L;
          Integer 8L;
          Integer 9L;
          Integer 10L;
          Integer 11L;
          Integer 12L;
          Integer 13L;
          Integer 14L;
          Integer 15L;
          Integer 16L;
          Integer 17L;
          Integer 18L;
          Integer 19L;
          Integer 20L;
          Integer 21L;
          Integer 22L;
          Integer 23L;
          Integer 24L;
          Integer 25L;
        ],
      None );
    ("0xa0", Map [], None);
    ( "0xa201020304",
      Map [ (Integer 1L, Integer 2L); (Integer 3L, Integer 4L) ],
      None );
    ( "0xa26161016162820203",
      Map
        [
          (Text_string "a", Integer 1L);
          (Text_string "b", Array [ Integer 2L; Integer 3L ]);
        ],
      None );
    ( "0x826161a161626163",
      Array [ Text_string "a"; Map [ (Text_string "b", Text_string "c") ] ],
      None );
    ( "0xa56161614161626142616361436164614461656145",
      Map
        [
          (Text_string "a", Text_string "A");
          (Text_string "b", Text_string "B");
          (Text_string "c", Text_string "C");
          (Text_string "d", Text_string "D");
          (Text_string "e", Text_string "E");
        ],
      None );
    ( "0x5f42010243030405ff",
      Byte_string "\x01\x02\x03\x04\x05",
      Some "0x450102030405" );
    ( "0x7f657374726561646d696e67ff",
      Text_string "streaming",
      Some "0x6973747265616d696e67" );
    ("0x9fff", Array [], Some "0x80");
    ( "0x9f018202039f0405ffff",
      Array
        [
          Integer 1L;
          Array [ Integer 2L; Integer 3L ];
          Array [ Integer 4L; Integer 5L ];
        ],
      Some "0x8301820203820405" );
    ( "0x9f01820203820405ff",
      Array
        [
          Integer 1L;
          Array [ Integer 2L; Integer 3L ];
          Array [ Integer 4L; Integer 5L ];
        ],
      Some "0x8301820203820405" );
    ( "0x83018202039f0405ff",
      Array
        [
          Integer 1L;
          Array [ Integer 2L; Integer 3L ];
          Array [ Integer 4L; Integer 5L ];
        ],
      Some "0x8301820203820405" );
    ( "0x83019f0203ff820405",
      Array
        [
          Integer 1L;
          Array [ Integer 2L; Integer 3L ];
          Array [ Integer 4L; Integer 5L ];
        ],
      Some "0x8301820203820405" );
    ( "0x9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff",
      Array
        [
          Integer 1L;
          Integer 2L;
          Integer 3L;
          Integer 4L;
          Integer 5L;
          Integer 6L;
          Integer 7L;
          Integer 8L;
          Integer 9L;
          Integer 10L;
          Integer 11L;
          Integer 12L;
          Integer 13L;
          Integer 14L;
          Integer 15L;
          Integer 16L;
          Integer 17L;
          Integer 18L;
          Integer 19L;
          Integer 20L;
          Integer 21L;
          Integer 22L;
          Integer 23L;
          Integer 24L;
          Integer 25L;
        ],
      Some "0x98190102030405060708090a0b0c0d0e0f101112131415161718181819" );
    ( "0xbf61610161629f0203ffff",
      Map
        [
          (Text_string "a", Integer 1L);
          (Text_string "b", Array [ Integer 2L; Integer 3L ]);
        ],
      Some "0xa26161016162820203" );
    ( "0x826161bf61626163ff",
      Array [ Text_string "a"; Map [ (Text_string "b", Text_string "c") ] ],
      Some "0x826161a161626163" );
    ( "0xbf6346756ef563416d7421ff",
      Map [ (Text_string "Fun", Simple 21); (Text_string "Amt", Integer (-2L)) ],
      Some "0xa263416d74216346756ef5" );
  ]
