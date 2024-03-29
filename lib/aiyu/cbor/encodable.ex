defprotocol Aiyu.CBOR.Encodable do
  def encode(value)
end

defimpl Aiyu.CBOR.Encodable, for: Aiyu.CBOR.SimpleValue do
  alias Aiyu.CBOR.Encodable.Utils

  def encode(%Aiyu.CBOR.SimpleValue{value: value}) do
    {:ok, Utils.header(7, value)}
  end
end

defimpl Aiyu.CBOR.Encodable, for: Aiyu.CBOR.Tag do
  alias Aiyu.CBOR.Encodable.Utils

  def encode(%Aiyu.CBOR.Tag{number: number, content: content}) do
    with {:ok, encoded_content} <- Aiyu.CBOR.Encodable.encode(content) do
      {:ok,
       <<
         Utils.header(6, number)::binary,
         encoded_content::binary
       >>}
    end
  end
end

defimpl Aiyu.CBOR.Encodable, for: Atom do
  alias Aiyu.CBOR

  def encode(atom) do
    case atom do
      false ->
        %CBOR.SimpleValue{value: 20}
        |> CBOR.Encodable.encode()

      true ->
        %CBOR.SimpleValue{value: 21}
        |> CBOR.Encodable.encode()

      nil ->
        %CBOR.SimpleValue{value: 22}
        |> CBOR.Encodable.encode()

      _ ->
        :error
    end
  end
end

defimpl Aiyu.CBOR.Encodable, for: Integer do
  alias Aiyu.CBOR
  alias Aiyu.CBOR.Encodable.Utils

  def encode(integer) do
    cond do
      integer >= 0 and integer < 0x10000000000000000 ->
        {:ok, Utils.header(0, integer)}

      integer < 0 and integer >= -0x10000000000000000 ->
        {:ok, Utils.header(1, -1 - integer)}

      integer >= 0 ->
        %CBOR.Tag{
          number: 2,
          content: %CBOR.Encodable.ByteString{value: :binary.encode_unsigned(integer)}
        }
        |> Aiyu.CBOR.Encodable.encode()

      integer < 0 ->
        %CBOR.Tag{
          number: 3,
          content: %CBOR.Encodable.ByteString{value: :binary.encode_unsigned(-1 - integer)}
        }
        |> Aiyu.CBOR.Encodable.encode()
    end
  end
end

defimpl Aiyu.CBOR.Encodable, for: Float do
  alias Aiyu.CBOR.Encodable.Utils

  def encode(float) do
    Enum.find_value([{16, 0xF9}, {32, 0xFA}, {64, 0xFB}], fn {size, initial_byte} ->
      encoded = <<float::big-float-size(size)>>

      case encoded do
        <<value::big-float-size(size)>> ->
          if value == float, do: <<initial_byte, encoded::binary>>

        _ ->
          nil
      end
    end)
    |> case do
      nil -> :error
      encoded -> {:ok, encoded}
    end
  end
end

defimpl Aiyu.CBOR.Encodable, for: BitString do
  alias Aiyu.CBOR.Encodable.Utils

  def encode(bitstring) do
    {:ok,
     <<
       Utils.header(3, byte_size(bitstring))::binary,
       bitstring::binary
     >>}
  end
end

defimpl Aiyu.CBOR.Encodable, for: List do
  alias Aiyu.CBOR
  alias Aiyu.CBOR.Encodable.Utils

  def encode(list) do
    result =
      Enum.reduce_while(list, {:ok, <<>>}, fn item, {:ok, content} ->
        case CBOR.Encodable.encode(item) do
          {:ok, encoded} -> {:cont, {:ok, <<content::binary, encoded::binary>>}}
          :error -> {:halt, :error}
        end
      end)

    with {:ok, content} <- result do
      {:ok, <<Utils.header(4, length(list))::binary, content::binary>>}
    end
  end
end

defimpl Aiyu.CBOR.Encodable, for: Map do
  alias Aiyu.CBOR
  alias Aiyu.CBOR.Encodable.Utils

  def encode(map) do
    result =
      Enum.reduce_while(map, {:ok, <<>>}, fn {key, value}, {:ok, content} ->
        with {:ok, encoded_key} <- CBOR.Encodable.encode(key),
             {:ok, encoded_value} <- CBOR.Encodable.encode(value) do
          {:cont, {:ok, <<content::binary, encoded_key::binary, encoded_value::binary>>}}
        else
          :error -> {:halt, :error}
        end
      end)

    with {:ok, content} <- result do
      {:ok, <<Utils.header(5, map_size(map))::binary, content::binary>>}
    end
  end
end
