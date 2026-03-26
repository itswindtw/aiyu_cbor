(* Shared *)

type sign = Unsigned | Negative

module type Integer = sig
  type t

  val compare : t -> t -> int

  (* decode *)
  val of_uint : int -> t
  val of_uint32 : int32 -> t option
  val of_uint64 : int64 -> t option
  val of_nint : int -> t
  val of_nint32 : int32 -> t option
  val of_nint64 : int64 -> t option

  (* encode *)
  val to_uint64 : t -> int64 option
  val to_nint64 : t -> int64 option
end

module Int16 = struct
  let bits_of_float float =
    if Float.is_nan float then 0x7e00
    else if Float.is_infinite float then if float >= 0.0 then 0x7c00 else 0xfc00
    else
      let bits_of_float64 = Int64.bits_of_float float in
      let sign64 = Int64.(shift_right_logical bits_of_float64 63) in
      let exponent64 =
        Int64.(shift_right_logical bits_of_float64 52 |> logand 0x7ffL)
      in
      let significand64 = Int64.logand bits_of_float64 0xf_ffff_ffff_ffffL in
      let exponent16 = Int64.to_int exponent64 - 1023 + 15 in

      let sign16 = Int64.to_int sign64 lsl 15 in
      let bits16 =
        if exponent16 <= 0 then
          let shift = 1 - exponent16 in
          if shift > 10 then 0
          else
            Int64.(
              shift_right_logical
                (logor significand64
                   0x0010_0000_0000_0000L (* implicit leading bit *))
                (42 + shift)
              |> to_int)
        else if exponent16 >= 31 then 0x7c00
        else
          (exponent16 lsl 10)
          lor Int64.(shift_right_logical significand64 42 |> to_int)
      in

      sign16 lor bits16

  let float_of_bits bits =
    let exp = (bits lsr 10) land 0x1f in
    let mant = bits land 0x3ff in
    let val_ =
      match exp with
      | 0 -> Float.ldexp (Int.to_float mant) (-24)
      | 31 -> if mant = 0 then Float.infinity else Float.nan
      | _ -> Float.ldexp (Int.to_float (mant + 1024)) (exp - 25)
    in
    if bits land 0x8000 <> 0 then Float.neg val_ else val_
end

module Cursor = struct
  type t = { data : string; pos : int; len : int }

  let sub n cursor =
    if cursor.pos + n <= cursor.len then
      let string = String.sub cursor.data cursor.pos n in
      Ok (string, { cursor with pos = cursor.pos + n })
    else Error "unexpected end of input"

  let get_uint8 cursor =
    if cursor.pos + 1 <= cursor.len then
      Ok
        ( String.get_uint8 cursor.data cursor.pos,
          { cursor with pos = cursor.pos + 1 } )
    else Error "unexpected end of input"

  let get_uint16_be cursor =
    if cursor.pos + 2 <= cursor.len then
      Ok
        ( String.get_uint16_be cursor.data cursor.pos,
          { cursor with pos = cursor.pos + 2 } )
    else Error "unexpected end of input"

  let get_int32_be cursor =
    if cursor.pos + 4 <= cursor.len then
      Ok
        ( String.get_int32_be cursor.data cursor.pos,
          { cursor with pos = cursor.pos + 4 } )
    else Error "unexpected end of input"

  let get_int64_be cursor =
    if cursor.pos + 8 <= cursor.len then
      Ok
        ( String.get_int64_be cursor.data cursor.pos,
          { cursor with pos = cursor.pos + 8 } )
    else Error "unexpected end of input"
end

module Make (I : Integer) = struct
  type t =
    | Integer of I.t
    | Simple of int
    | Float of float
    | Byte_string of string
    | Text_string of string
    | Array of t list
    | Map of (t * t) list
    | Tag of I.t * t

  let rec to_pairs = function
    | [] -> Ok []
    | a :: b :: rest -> (
        match to_pairs rest with
        | Ok pairs -> Ok ((a, b) :: pairs)
        | Error e -> Error e)
    | _ -> Error "odd length of pairs"

  (* Decoder *)

  let do_decode_byte_string uint8 (cursor : Cursor.t) =
    let open Result.Syntax in
    match uint8 with
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4a
    | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55
    | 0x56 | 0x57 ->
        let n = uint8 - 0x40 in
        Cursor.sub n cursor
    | 0x58 ->
        let* n, cursor = Cursor.get_uint8 cursor in
        Cursor.sub n cursor
    | 0x59 ->
        let* n, cursor = Cursor.get_uint16_be cursor in
        Cursor.sub n cursor
    | 0x5a -> (
        let* int32, cursor = Cursor.get_int32_be cursor in

        match Int32.unsigned_to_int int32 with
        | Some n -> Cursor.sub n cursor
        | None -> Error "n can not fit into int")
    | 0x5b -> (
        let* int64, cursor = Cursor.get_int64_be cursor in

        match Int64.unsigned_to_int int64 with
        | Some n -> Cursor.sub n cursor
        | None -> Error "n can not fit into int")
    | _ -> Error "expect byte string"

  let do_decode_text_string uint8 (cursor : Cursor.t) =
    let open Result.Syntax in
    match uint8 with
    | 0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69 | 0x6a
    | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75
    | 0x76 | 0x77 ->
        let n = uint8 - 0x60 in
        Cursor.sub n cursor
    | 0x78 ->
        let* n, cursor = Cursor.get_uint8 cursor in
        Cursor.sub n cursor
    | 0x79 ->
        let* n, cursor = Cursor.get_uint16_be cursor in
        Cursor.sub n cursor
    | 0x7a -> (
        let* int32, cursor = Cursor.get_int32_be cursor in

        match Int32.unsigned_to_int int32 with
        | Some n -> Cursor.sub n cursor
        | None -> Error "n can not fit into int")
    | 0x7b -> (
        let* int64, cursor = Cursor.get_int64_be cursor in

        match Int64.unsigned_to_int int64 with
        | Some n -> Cursor.sub n cursor
        | None -> Error "n can not fit into int")
    | _ -> Error "expect text string"

  let do_decode_chunks_indefinitely chunk_decoder (cursor : Cursor.t) =
    let rec collect_chunks (cursor : Cursor.t) =
      let open Result.Syntax in
      let* uint8, cursor = Cursor.get_uint8 cursor in
      match uint8 with
      | 0xff -> Ok ([], cursor)
      | uint8 ->
          let* chunk, cursor = chunk_decoder uint8 cursor in
          let* chunks, cursor = collect_chunks cursor in
          Ok (chunk :: chunks, cursor)
    in
    collect_chunks cursor
    |> Result.map (fun (chunks, cursor) -> (String.concat "" chunks, cursor))

  let do_decode_byte_string_indefinitely =
    do_decode_chunks_indefinitely do_decode_byte_string

  let do_decode_text_string_indefinitely =
    do_decode_chunks_indefinitely do_decode_text_string

  let rec do_decode_item (cursor : Cursor.t) =
    let open Result.Syntax in
    let* uint8, cursor = Cursor.get_uint8 cursor in
    match uint8 with
    | 0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x08 | 0x09 | 0x0A
    | 0x0B | 0x0C | 0x0D | 0x0E | 0x0F | 0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15
    | 0x16 | 0x17 ->
        Ok (Integer (I.of_uint uint8), cursor)
    | 0x18 ->
        let* uint8, cursor = Cursor.get_uint8 cursor in
        Ok (Integer (I.of_uint uint8), cursor)
    | 0x19 ->
        let* uint16, cursor = Cursor.get_uint16_be cursor in
        Ok (Integer (I.of_uint uint16), cursor)
    | 0x1a -> (
        let* int32, cursor = Cursor.get_int32_be cursor in
        match I.of_uint32 int32 with
        | Some i -> Ok (Integer i, cursor)
        | None -> Error "of_uint32")
    | 0x1b -> (
        let* int64, cursor = Cursor.get_int64_be cursor in
        match I.of_uint64 int64 with
        | Some i -> Ok (Integer i, cursor)
        | None -> Error "of_uint64")
    | 0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 | 0x26 | 0x27 | 0x28 | 0x29 | 0x2a
    | 0x2b | 0x2c | 0x2d | 0x2e | 0x2f | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35
    | 0x36 | 0x37 ->
        Ok (Integer (I.of_nint (uint8 - 0x20)), cursor)
    | 0x38 ->
        let* uint8, cursor = Cursor.get_uint8 cursor in
        Ok (Integer (I.of_nint uint8), cursor)
    | 0x39 ->
        let* uint16, cursor = Cursor.get_uint16_be cursor in
        Ok (Integer (I.of_nint uint16), cursor)
    | 0x3a -> (
        let* int32, cursor = Cursor.get_int32_be cursor in
        match I.of_nint32 int32 with
        | Some i -> Ok (Integer i, cursor)
        | None -> Error "of_nint32")
    | 0x3b -> (
        let* int64, cursor = Cursor.get_int64_be cursor in
        match I.of_nint64 int64 with
        | Some i -> Ok (Integer i, cursor)
        | None -> Error "of_nint64")
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4a
    | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55
    | 0x56 | 0x57 | 0x58 | 0x59 | 0x5a | 0x5b ->
        do_decode_byte_string uint8 cursor
        |> Result.map (fun (s, cursor) -> (Byte_string s, cursor))
    | 0x5f ->
        do_decode_byte_string_indefinitely cursor
        |> Result.map (fun (s, cursor) -> (Byte_string s, cursor))
    | 0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69 | 0x6a
    | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75
    | 0x76 | 0x77 | 0x78 | 0x79 | 0x7a | 0x7b ->
        do_decode_text_string uint8 cursor
        |> Result.map (fun (s, cursor) -> (Text_string s, cursor))
    | 0x7f ->
        do_decode_text_string_indefinitely cursor
        |> Result.map (fun (s, cursor) -> (Text_string s, cursor))
    | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89 | 0x8a
    | 0x8b | 0x8c | 0x8d | 0x8e | 0x8f | 0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95
    | 0x96 | 0x97 ->
        let n = uint8 - 0x80 in
        do_decode_items_n n cursor
        |> Result.map (fun (list, cursor) -> (Array list, cursor))
    | 0x98 ->
        let* n, cursor = Cursor.get_uint8 cursor in
        do_decode_items_n n cursor
        |> Result.map (fun (list, cursor) -> (Array list, cursor))
    | 0x99 ->
        let* n, cursor = Cursor.get_uint16_be cursor in
        do_decode_items_n n cursor
        |> Result.map (fun (list, cursor) -> (Array list, cursor))
    | 0x9a -> (
        let* int32, cursor = Cursor.get_int32_be cursor in

        match Int32.unsigned_to_int int32 with
        | Some n ->
            do_decode_items_n n cursor
            |> Result.map (fun (list, cursor) -> (Array list, cursor))
        | None -> Error "n can not fit into int")
    | 0x9b -> (
        let* int64, cursor = Cursor.get_int64_be cursor in

        match Int64.unsigned_to_int int64 with
        | Some n ->
            do_decode_items_n n cursor
            |> Result.map (fun (list, cursor) -> (Array list, cursor))
        | None -> Error "n can not fit into int")
    | 0x9f ->
        do_decode_items_indefinitely cursor
        |> Result.map (fun (list, cursor) -> (Array list, cursor))
    | 0xa0 | 0xa1 | 0xa2 | 0xa3 | 0xa4 | 0xa5 | 0xa6 | 0xa7 | 0xa8 | 0xa9 | 0xaa
    | 0xab | 0xac | 0xad | 0xae | 0xaf | 0xb0 | 0xb1 | 0xb2 | 0xb3 | 0xb4 | 0xb5
    | 0xb6 | 0xb7 ->
        let n = uint8 - 0xa0 in

        do_decode_pairs_n n cursor
        |> Result.map (fun (pairs, cursor) -> (Map pairs, cursor))
    | 0xb8 ->
        let* n, cursor = Cursor.get_uint8 cursor in

        do_decode_pairs_n n cursor
        |> Result.map (fun (pairs, cursor) -> (Map pairs, cursor))
    | 0xb9 ->
        let* n, cursor = Cursor.get_uint16_be cursor in

        do_decode_pairs_n n cursor
        |> Result.map (fun (pairs, cursor) -> (Map pairs, cursor))
    | 0xba -> (
        let* int32, cursor = Cursor.get_int32_be cursor in

        match Int32.unsigned_to_int int32 with
        | Some n ->
            do_decode_pairs_n n cursor
            |> Result.map (fun (pairs, cursor) -> (Map pairs, cursor))
        | None -> Error "n can not fit into int")
    | 0xbb -> (
        let* int64, cursor = Cursor.get_int64_be cursor in

        match Int64.unsigned_to_int int64 with
        | Some n ->
            do_decode_pairs_n n cursor
            |> Result.map (fun (pairs, cursor) -> (Map pairs, cursor))
        | None -> Error "n can not fit into int")
    | 0xbf ->
        do_decode_pairs_indefinitely cursor
        |> Result.map (fun (pairs, cursor) -> (Map pairs, cursor))
    | 0xc0 | 0xc1 | 0xc2 | 0xc3 | 0xc4 | 0xc5 | 0xc6 | 0xc7 | 0xc8 | 0xc9 | 0xca
    | 0xcb | 0xcc | 0xcd | 0xce | 0xcf | 0xd0 | 0xd1 | 0xd2 | 0xd3 | 0xd4 | 0xd5
    | 0xd6 | 0xd7 ->
        let n = uint8 - 0xc0 in

        do_decode_item cursor
        |> Result.map (fun (item, cursor) -> (Tag (I.of_uint n, item), cursor))
    | 0xd8 ->
        let* n, cursor = Cursor.get_uint8 cursor in

        do_decode_item cursor
        |> Result.map (fun (item, cursor) -> (Tag (I.of_uint n, item), cursor))
    | 0xd9 ->
        let* n, cursor = Cursor.get_uint16_be cursor in

        do_decode_item cursor
        |> Result.map (fun (item, cursor) -> (Tag (I.of_uint n, item), cursor))
    | 0xda -> (
        let* n, cursor = Cursor.get_int32_be cursor in

        match I.of_uint32 n with
        | Some i ->
            do_decode_item cursor
            |> Result.map (fun (item, cursor) -> (Tag (i, item), cursor))
        | None -> Error "of_uint32")
    | 0xdb -> (
        let* n, cursor = Cursor.get_int64_be cursor in

        match I.of_uint64 n with
        | Some i ->
            do_decode_item cursor
            |> Result.map (fun (item, cursor) -> (Tag (i, item), cursor))
        | None -> Error "of_uint64")
    | 0xe0 | 0xe1 | 0xe2 | 0xe3 | 0xe4 | 0xe5 | 0xe6 | 0xe7 | 0xe8 | 0xe9 | 0xea
    | 0xeb | 0xec | 0xed | 0xee | 0xef | 0xf0 | 0xf1 | 0xf2 | 0xf3 | 0xf4 | 0xf5
    | 0xf6 | 0xf7 ->
        let value = uint8 - 0xe0 in
        Ok (Simple value, cursor)
    | 0xf8 ->
        let* value, cursor = Cursor.get_uint8 cursor in
        Ok (Simple value, cursor)
    | 0xf9 ->
        let* uint16, cursor = Cursor.get_uint16_be cursor in
        let float = Int16.float_of_bits uint16 in
        Ok (Float float, cursor)
    | 0xfa ->
        let* int32, cursor = Cursor.get_int32_be cursor in
        let float = Int32.float_of_bits int32 in
        Ok (Float float, cursor)
    | 0xfb ->
        let* int64, cursor = Cursor.get_int64_be cursor in
        let float = Int64.float_of_bits int64 in
        Ok (Float float, cursor)
    | _ -> Error "expect data item"

  and do_decode_items_n n cursor =
    let open Result.Syntax in
    match n with
    | 0 -> Ok ([], cursor)
    | n ->
        let* v, cursor = do_decode_item cursor in
        let* vs, cursor = do_decode_items_n (n - 1) cursor in
        Ok (v :: vs, cursor)

  and do_decode_pairs_n n cursor =
    let open Result.Syntax in
    let* items, cursor = do_decode_items_n (2 * n) cursor in
    let* pairs = to_pairs items in
    Ok (pairs, cursor)

  and do_decode_items_indefinitely cursor =
    let open Result.Syntax in
    let* uint8, cursor' = Cursor.get_uint8 cursor in
    match uint8 with
    | 0xff -> Ok ([], cursor')
    | _ ->
        let* item, cursor = do_decode_item cursor in
        let* items, cursor = do_decode_items_indefinitely cursor in
        Ok (item :: items, cursor)

  and do_decode_pairs_indefinitely cursor =
    let open Result.Syntax in
    let* items, cursor = do_decode_items_indefinitely cursor in
    let* pairs = to_pairs items in
    Ok (pairs, cursor)

  let decode_item data =
    match do_decode_item Cursor.{ data; pos = 0; len = String.length data } with
    | Ok (value, { data; pos }) ->
        Ok (value, String.sub data pos (String.length data - pos))
    | Error m -> Error m

  let decode data =
    match decode_item data with
    | Ok (value, "") -> Ok value
    | Ok (_, r) -> Error (Printf.sprintf "trailing %d bytes" (String.length r))
    | Error m -> Error m

  let encode_header major argument =
    let major = major lsl 5 in

    if Int64.unsigned_compare argument 23L <= 0 then
      Bytes.make 1 (Char.chr (major lor Int64.to_int argument))
    else if Int64.unsigned_compare argument 0xffL <= 0 then (
      let b = Bytes.create 2 in
      Bytes.set b 0 (Char.chr (major lor 24));
      Bytes.set_uint8 b 1 (Int64.to_int argument);
      b)
    else if Int64.unsigned_compare argument 0xffffL <= 0 then (
      let b = Bytes.create 3 in
      Bytes.set b 0 (Char.chr (major lor 25));
      Bytes.set_uint16_be b 1 (Int64.to_int argument);
      b)
    else if Int64.unsigned_compare argument 0xffffffffL <= 0 then (
      let b = Bytes.create 5 in
      Bytes.set b 0 (Char.chr (major lor 26));
      Bytes.set_int32_be b 1 (Int64.to_int32 argument);
      b)
    else
      let b = Bytes.create 9 in
      Bytes.set b 0 (Char.chr (major lor 27));
      Bytes.set_int64_be b 1 argument;
      b

  let rec encode = function
    | Integer i ->
        if I.compare i (I.of_uint 0) >= 0 then
          let uint64 = Option.get (I.to_uint64 i) in
          encode_header 0 uint64
        else
          let nint64 = Option.get (I.to_nint64 i) in
          encode_header 1 nint64
    | Simple int ->
        let major = 7 lsl 5 in

        if int < 24 then Bytes.make 1 (Char.chr (major lor int))
        else
          let b = Bytes.create 2 in
          Bytes.set b 0 (Char.chr (major lor 24));
          Bytes.set_uint8 b 1 int;
          b
    | Float float ->
        let major = 7 lsl 5 in
        let bits16 = Int16.bits_of_float float in
        if Float.equal (Int16.float_of_bits bits16) float then (
          let b = Bytes.create 3 in
          Bytes.set b 0 (Char.chr (major lor 25));
          Bytes.set_uint16_be b 1 bits16;
          b)
        else
          let bits32 = Int32.bits_of_float float in
          if Float.equal (Int32.float_of_bits bits32) float then (
            let b = Bytes.create 5 in
            Bytes.set b 0 (Char.chr (major lor 26));
            Bytes.set_int32_be b 1 bits32;
            b)
          else
            let bits64 = Int64.bits_of_float float in
            let b = Bytes.create 9 in
            Bytes.set b 0 (Char.chr (major lor 27));
            Bytes.set_int64_be b 1 bits64;
            b
    | Byte_string string ->
        let n = String.length string in
        let header = encode_header 2 (Int64.of_int n) in

        Bytes.concat Bytes.empty [ header; String.to_bytes string ]
    | Text_string string ->
        let n = String.length string in
        let header = encode_header 3 (Int64.of_int n) in

        Bytes.concat Bytes.empty [ header; String.to_bytes string ]
    | Array array ->
        let n = List.length array in
        let header = encode_header 4 (Int64.of_int n) in

        Bytes.concat Bytes.empty (header :: List.map encode array)
    | Map pairs ->
        let n = List.length pairs in
        let header = encode_header 5 (Int64.of_int n) in

        let encoded_items =
          pairs
          |> List.map (fun (k, v) -> (encode k, encode v))
          |> List.sort (fun (k1, _) (k2, _) -> Bytes.compare k1 k2)
          |> List.concat_map (fun (k, v) -> [ k; v ])
        in

        Bytes.concat Bytes.empty (header :: encoded_items)
    | Tag (n, t) ->
        let uint64 = Option.get (I.to_uint64 n) in
        let header = encode_header 6 uint64 in

        let item = encode t in

        Bytes.concat Bytes.empty [ header; item ]
end
