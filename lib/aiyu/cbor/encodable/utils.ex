defmodule Aiyu.CBOR.Encodable.Utils do
  def header(major_type, argument) do
    cond do
      argument < 24 ->
        <<
          major_type::unsigned-integer-size(3),
          argument::unsigned-integer-size(5)
        >>

      argument < 0x100 ->
        <<
          major_type::unsigned-integer-size(3),
          24::unsigned-integer-size(5),
          argument::unsigned-integer-size(8)
        >>

      argument < 0x10000 ->
        <<
          major_type::unsigned-integer-size(3),
          25::unsigned-integer-size(5),
          argument::unsigned-integer-size(16)
        >>

      argument < 0x100000000 ->
        <<
          major_type::unsigned-integer-size(3),
          26::unsigned-integer-size(5),
          argument::unsigned-integer-size(32)
        >>

      argument < 0x10000000000000000 ->
        <<
          major_type::unsigned-integer-size(3),
          27::unsigned-integer-size(5),
          argument::unsigned-integer-size(64)
        >>
    end
  end
end
