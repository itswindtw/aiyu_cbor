defmodule Aiyu.CBOR do
  @moduledoc false

  alias Aiyu.CBOR

  def decode(encoded) do
    CBOR.Decoder.extended()
    |> CBOR.Decoder.decode(encoded)
  end

  def encode(data) do
    CBOR.Encodable.encode(data)
  end
end
