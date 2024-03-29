defmodule Aiyu.CBOR.EncodableTest do
  @moduledoc """
  Table is from https://www.rfc-editor.org/rfc/rfc8949.html#section-appendix.a
  """

  use ExUnit.Case
  alias Aiyu.CBOR

  test "encode/1" do
    table = [
      {0, <<0x00>>},
      {1, <<0x01>>},
      {10, <<0x0A>>},
      {23, <<0x17>>},
      {24, <<0x18, 0x18>>},
      {25, <<0x18, 0x19>>},
      {100, <<0x18, 0x64>>},
      {1000, <<0x19, 0x03, 0xE8>>},
      {1_000_000, <<0x1A, 0x00, 0x0F, 0x42, 0x40>>},
      {1_000_000_000_000, <<0x1B, 0x00, 0x00, 0x00, 0xE8, 0xD4, 0xA5, 0x10, 0x00>>},
      {18_446_744_073_709_551_615, <<0x1B, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>},
      {18_446_744_073_709_551_616,
       <<0xC2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>},
      {-18_446_744_073_709_551_616, <<0x3B, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>},
      {-18_446_744_073_709_551_617,
       <<0xC3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>},
      {-1, <<0x20>>},
      {-10, <<0x29>>},
      {-100, <<0x38, 0x63>>},
      {-1000, <<0x39, 0x03, 0xE7>>},
      {0.0, <<0xF9, 0x00, 0x00>>},
      {-0.0, <<0xF9, 0x80, 0x00>>},
      {1.0, <<0xF9, 0x3C, 0x00>>},
      {1.1, <<0xFB, 0x3F, 0xF1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9A>>},
      {1.5, <<0xF9, 0x3E, 0x00>>},
      {65504.0, <<0xF9, 0x7B, 0xFF>>},
      {100_000.0, <<0xFA, 0x47, 0xC3, 0x50, 0x00>>},
      {3.4028234663852886e+38, <<0xFA, 0x7F, 0x7F, 0xFF, 0xFF>>},
      {1.0e+300, <<0xFB, 0x7E, 0x37, 0xE4, 0x3C, 0x88, 0x00, 0x75, 0x9C>>},
      {5.960464477539063e-8, <<0xF9, 0x00, 0x01>>},
      {0.00006103515625, <<0xF9, 0x04, 0x00>>},
      {-4.0, <<0xF9, 0xC4, 0x00>>},
      {-4.1, <<0xFB, 0xC0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66>>},
      {false, <<0xF4>>},
      {true, <<0xF5>>},
      {nil, <<0xF6>>},
      {%CBOR.SimpleValue{value: 23}, <<0xF7>>},
      {%CBOR.SimpleValue{value: 16}, <<0xF0>>},
      {%CBOR.SimpleValue{value: 255}, <<0xF8, 0xFF>>},
      {%CBOR.Tag{number: 0, content: "2013-03-21T20:04:00Z"},
       <<0xC0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2D, 0x30, 0x33, 0x2D, 0x32, 0x31, 0x54, 0x32, 0x30,
         0x3A, 0x30, 0x34, 0x3A, 0x30, 0x30, 0x5A>>},
      {%CBOR.Tag{number: 1, content: 1_363_896_240}, <<0xC1, 0x1A, 0x51, 0x4B, 0x67, 0xB0>>},
      {%CBOR.Tag{number: 1, content: 1_363_896_240.5},
       <<0xC1, 0xFB, 0x41, 0xD4, 0x52, 0xD9, 0xEC, 0x20, 0x00, 0x00>>},
      {%CBOR.Tag{
         number: 23,
         content: %CBOR.Encodable.ByteString{value: <<0x01, 0x02, 0x03, 0x04>>}
       }, <<0xD7, 0x44, 0x01, 0x02, 0x03, 0x04>>},
      {%CBOR.Tag{
         number: 24,
         content: %CBOR.Encodable.ByteString{value: <<0x64, 0x49, 0x45, 0x54, 0x46>>}
       }, <<0xD8, 0x18, 0x45, 0x64, 0x49, 0x45, 0x54, 0x46>>},
      {%CBOR.Tag{number: 32, content: "http://www.example.com"},
       <<0xD8, 0x20, 0x76, 0x68, 0x74, 0x74, 0x70, 0x3A, 0x2F, 0x2F, 0x77, 0x77, 0x77, 0x2E, 0x65,
         0x78, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x63, 0x6F, 0x6D>>},
      {%CBOR.Encodable.ByteString{value: <<>>}, <<0x40>>},
      {%CBOR.Encodable.ByteString{value: <<0x01, 0x02, 0x03, 0x04>>},
       <<0x44, 0x01, 0x02, 0x03, 0x04>>},
      {"", <<0x60>>},
      {"a", <<0x61, 0x61>>},
      {"IETF", <<0x64, 0x49, 0x45, 0x54, 0x46>>},
      {"\"\\", <<0x62, 0x22, 0x5C>>},
      {"ü", <<0x62, 0xC3, 0xBC>>},
      {"水", <<0x63, 0xE6, 0xB0, 0xB4>>},
      {"𐅑", <<0x64, 0xF0, 0x90, 0x85, 0x91>>},
      {[], <<0x80>>},
      {[1, 2, 3], <<0x83, 0x01, 0x02, 0x03>>},
      {[1, [2, 3], [4, 5]], <<0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05>>},
      {[
         1,
         2,
         3,
         4,
         5,
         6,
         7,
         8,
         9,
         10,
         11,
         12,
         13,
         14,
         15,
         16,
         17,
         18,
         19,
         20,
         21,
         22,
         23,
         24,
         25
       ],
       <<0x98, 0x19, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D,
         0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18, 0x18, 0x19>>},
      {%{}, <<0xA0>>},
      {%{1 => 2, 3 => 4}, <<0xA2, 0x01, 0x02, 0x03, 0x04>>},
      {%{"a" => 1, "b" => [2, 3]}, <<0xA2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03>>},
      {["a", %{"b" => "c"}], <<0x82, 0x61, 0x61, 0xA1, 0x61, 0x62, 0x61, 0x63>>},
      {%{"a" => "A", "b" => "B", "c" => "C", "d" => "D", "e" => "E"},
       <<0xA5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61, 0x42, 0x61, 0x63, 0x61, 0x43, 0x61, 0x64,
         0x61, 0x44, 0x61, 0x65, 0x61, 0x45>>}
    ]

    for {data, expected} <- table do
      assert {:ok, ^expected} = CBOR.Encodable.encode(data)
    end
  end
end
