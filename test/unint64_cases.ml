module Unint64 = struct
  type t = U of Int64.t | N of Int64.t [@@deriving show, eq]

  let compare x y =
    match (x, y) with
    | U x, U y | N y, N x -> Int64.unsigned_compare x y
    | U _, N _ -> 1
    | N _, U _ -> -1

  (* decode *)
  let of_uint uint = U (Int64.of_int uint)

  let of_uint32 uint32 =
    Some (U (Int64.logand (Int64.of_int32 uint32) 0xffffffffL))

  let of_uint64 uint64 = Some (U uint64)
  let of_nint nint = N (Int64.of_int nint)

  let of_nint32 nint32 =
    Some (N (Int64.logand (Int64.of_int32 nint32) 0xffffffffL))

  let of_nint64 nint64 = Some (N nint64)

  (* encode *)
  let to_uint64 = function U uint64 -> Some uint64 | N _ -> None
  let to_nint64 = function U _ -> None | N nint64 -> Some nint64
end

module M = Lokto_cbor_test.Make (Unint64)

let examples =
  let open M.Cbor in
  [
    ("0x00", Integer (U 0L), None);
    ("0x01", Integer (U 1L), None);
    ("0x0a", Integer (U 10L), None);
    ("0x17", Integer (U 23L), None);
    ("0x1818", Integer (U 24L), None);
    ("0x1819", Integer (U 25L), None);
    ("0x1864", Integer (U 100L), None);
    ("0x1903e8", Integer (U 1000L), None);
    ("0x1a000f4240", Integer (U 1000000L), None);
    ("0x1b000000e8d4a51000", Integer (U 1000000000000L), None);
    ("0x1bffffffffffffffff", Integer (U (-1L)), None);
    ( "0xc249010000000000000000",
      Tag (U 2L, Byte_string "\x01\x00\x00\x00\x00\x00\x00\x00\x00"),
      None );
    ("0x3bffffffffffffffff", Integer (N (-1L)), None);
    ( "0xc349010000000000000000",
      Tag (U 3L, Byte_string "\x01\x00\x00\x00\x00\x00\x00\x00\x00"),
      None );
    ("0x20", Integer (N 0L), None);
    ("0x29", Integer (N 9L), None);
    ("0x3863", Integer (N 99L), None);
    ("0x3903e7", Integer (N 999L), None);
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
      Tag (U 0L, Text_string "2013-03-21T20:04:00Z"),
      None );
    ("0xc11a514b67b0", Tag (U 1L, Integer (U 1363896240L)), None);
    ("0xc1fb41d452d9ec200000", Tag (U 1L, Float 1363896240.5), None);
    ("0xd74401020304", Tag (U 23L, Byte_string "\x01\x02\x03\x04"), None);
    ("0xd818456449455446", Tag (U 24L, Byte_string "\x64\x49\x45\x54\x46"), None);
    ( "0xd82076687474703a2f2f7777772e6578616d706c652e636f6d",
      Tag (U 32L, Text_string "http://www.example.com"),
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
    ( "0x83010203",
      Array [ Integer (U 1L); Integer (U 2L); Integer (U 3L) ],
      None );
    ( "0x8301820203820405",
      Array
        [
          Integer (U 1L);
          Array [ Integer (U 2L); Integer (U 3L) ];
          Array [ Integer (U 4L); Integer (U 5L) ];
        ],
      None );
    ( "0x98190102030405060708090a0b0c0d0e0f101112131415161718181819",
      Array
        [
          Integer (U 1L);
          Integer (U 2L);
          Integer (U 3L);
          Integer (U 4L);
          Integer (U 5L);
          Integer (U 6L);
          Integer (U 7L);
          Integer (U 8L);
          Integer (U 9L);
          Integer (U 10L);
          Integer (U 11L);
          Integer (U 12L);
          Integer (U 13L);
          Integer (U 14L);
          Integer (U 15L);
          Integer (U 16L);
          Integer (U 17L);
          Integer (U 18L);
          Integer (U 19L);
          Integer (U 20L);
          Integer (U 21L);
          Integer (U 22L);
          Integer (U 23L);
          Integer (U 24L);
          Integer (U 25L);
        ],
      None );
    ("0xa0", Map [], None);
    ( "0xa201020304",
      Map [ (Integer (U 1L), Integer (U 2L)); (Integer (U 3L), Integer (U 4L)) ],
      None );
    ( "0xa26161016162820203",
      Map
        [
          (Text_string "a", Integer (U 1L));
          (Text_string "b", Array [ Integer (U 2L); Integer (U 3L) ]);
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
          Integer (U 1L);
          Array [ Integer (U 2L); Integer (U 3L) ];
          Array [ Integer (U 4L); Integer (U 5L) ];
        ],
      Some "0x8301820203820405" );
    ( "0x9f01820203820405ff",
      Array
        [
          Integer (U 1L);
          Array [ Integer (U 2L); Integer (U 3L) ];
          Array [ Integer (U 4L); Integer (U 5L) ];
        ],
      Some "0x8301820203820405" );
    ( "0x83018202039f0405ff",
      Array
        [
          Integer (U 1L);
          Array [ Integer (U 2L); Integer (U 3L) ];
          Array [ Integer (U 4L); Integer (U 5L) ];
        ],
      Some "0x8301820203820405" );
    ( "0x83019f0203ff820405",
      Array
        [
          Integer (U 1L);
          Array [ Integer (U 2L); Integer (U 3L) ];
          Array [ Integer (U 4L); Integer (U 5L) ];
        ],
      Some "0x8301820203820405" );
    ( "0x9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff",
      Array
        [
          Integer (U 1L);
          Integer (U 2L);
          Integer (U 3L);
          Integer (U 4L);
          Integer (U 5L);
          Integer (U 6L);
          Integer (U 7L);
          Integer (U 8L);
          Integer (U 9L);
          Integer (U 10L);
          Integer (U 11L);
          Integer (U 12L);
          Integer (U 13L);
          Integer (U 14L);
          Integer (U 15L);
          Integer (U 16L);
          Integer (U 17L);
          Integer (U 18L);
          Integer (U 19L);
          Integer (U 20L);
          Integer (U 21L);
          Integer (U 22L);
          Integer (U 23L);
          Integer (U 24L);
          Integer (U 25L);
        ],
      Some "0x98190102030405060708090a0b0c0d0e0f101112131415161718181819" );
    ( "0xbf61610161629f0203ffff",
      Map
        [
          (Text_string "a", Integer (U 1L));
          (Text_string "b", Array [ Integer (U 2L); Integer (U 3L) ]);
        ],
      Some "0xa26161016162820203" );
    ( "0x826161bf61626163ff",
      Array [ Text_string "a"; Map [ (Text_string "b", Text_string "c") ] ],
      Some "0x826161a161626163" );
    ( "0xbf6346756ef563416d7421ff",
      Map
        [ (Text_string "Fun", Simple 21); (Text_string "Amt", Integer (N 1L)) ],
      Some "0xa263416d74216346756ef5" );
  ]
