defmodule Aiyu.CBOR.Encodable.ByteString do
  defstruct [:value]
end

defimpl Aiyu.CBOR.Encodable, for: Aiyu.CBOR.Encodable.ByteString do
  alias Aiyu.CBOR.Encodable.Utils

  def encode(%Aiyu.CBOR.Encodable.ByteString{value: value}) do
    {:ok,
     <<
       Utils.header(2, byte_size(value))::binary,
       value::binary
     >>}
  end
end
