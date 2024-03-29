defmodule Aiyu.CBOR.Decoder do
  @moduledoc """
  ref: https://www.rfc-editor.org/rfc/rfc8949.html
  """

  alias Aiyu.CBOR

  defstruct [
    :simple_values,
    :tags
  ]

  def basic do
    %__MODULE__{
      simple_values: %{},
      tags: %{}
    }
  end

  def extended do
    %__MODULE__{
      simple_values: %{
        20 => false,
        21 => true,
        22 => nil
      },
      tags: %{
        0 => &parse_standard_datetime_string/1,
        1 => &parse_epoch_based_datetime/1,
        2 => &parse_unsigned_bignum/1,
        3 => &parse_negative_bignum/1,
        32 => &parse_uri/1
      }
    }
  end

  def parse_standard_datetime_string(content) do
    case DateTime.from_iso8601(content) do
      {:ok, datetime, _offset} -> {:ok, datetime}
      {:error, _reason} -> :error
    end
  end

  def parse_epoch_based_datetime(content) do
    content =
      (content * 1_000_000)
      |> round()

    case DateTime.from_unix(content, :microsecond) do
      {:ok, datetime} -> {:ok, datetime}
      {:error, _reason} -> :error
    end
  end

  def parse_unsigned_bignum(content) do
    case content do
      <<n::unsigned-big-integer-size(bit_size(content))>> ->
        {:ok, n}

      _ ->
        :error
    end
  end

  def parse_negative_bignum(content) do
    with {:ok, n} <- parse_unsigned_bignum(content) do
      {:ok, -1 - n}
    end
  end

  def parse_uri(content) do
    case URI.new(content) do
      {:ok, uri} -> {:ok, uri}
      {:error, _part} -> :error
    end
  end

  def decode(
        decoder,
        <<
          major_type::unsigned-integer-size(3),
          _additional_information::unsigned-integer-size(5),
          _rest::binary
        >> = bytes
      ) do
    case major_type do
      0 ->
        decode_unsigned_integer(bytes)

      1 ->
        decode_negative_integer(bytes)

      2 ->
        decode_byte_string(bytes)

      3 ->
        decode_utf8_string(bytes)

      4 ->
        decode_array(decoder, bytes)

      5 ->
        decode_map(decoder, bytes)

      6 ->
        with {:ok, %CBOR.Tag{number: number, content: content}, rest} <-
               decode_tag(decoder, bytes) do
          case Map.fetch(decoder.tags, number) do
            {:ok, parser} ->
              with {:ok, value} <- parser.(content) do
                {:ok, value, rest}
              end

            :error ->
              {:ok, %CBOR.Tag{number: number, content: content}, rest}
          end
        end

      7 ->
        case decode_floating_point_number_and_simple_value(bytes) do
          {:ok, %CBOR.SimpleValue{value: value}, rest} ->
            case Map.fetch(decoder.simple_values, value) do
              {:ok, mapped} ->
                {:ok, mapped, rest}

              :error ->
                {:ok, %CBOR.SimpleValue{value: value}, rest}
            end

          result ->
            result
        end
    end
  end

  def decode(_, _), do: :error

  # decode_unsigned_integer

  def decode_unsigned_integer(<<
        0::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> {:ok, x, rest}
      24 -> do_decode_unsigned_integer(rest, 1)
      25 -> do_decode_unsigned_integer(rest, 2)
      26 -> do_decode_unsigned_integer(rest, 4)
      27 -> do_decode_unsigned_integer(rest, 8)
      _ -> :error
    end
  end

  defp do_decode_unsigned_integer(bytes, size) do
    case bytes do
      <<value::unsigned-big-integer-size(8 * size), rest::binary>> ->
        {:ok, value, rest}

      _ ->
        :error
    end
  end

  # decode_negative_integer

  def decode_negative_integer(<<
        1::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> {:ok, -1 - x, rest}
      24 -> do_decode_negative_integer(rest, 1)
      25 -> do_decode_negative_integer(rest, 2)
      26 -> do_decode_negative_integer(rest, 4)
      27 -> do_decode_negative_integer(rest, 8)
      _ -> :error
    end
  end

  defp do_decode_negative_integer(bytes, size) do
    with {:ok, value, bytes} <- do_decode_unsigned_integer(bytes, size) do
      {:ok, -1 - value, bytes}
    end
  end

  # decode_byte_string

  def decode_byte_string(
        <<
          2::unsigned-integer-size(3),
          additional_information::unsigned-integer-size(5),
          rest::binary
        >> = bytes
      ) do
    case additional_information do
      31 -> do_decode_byte_string_indefinite(rest)
      _ -> decode_byte_string_definite(bytes)
    end
  end

  def decode_byte_string_definite(<<
        2::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> do_decode_byte_string(rest, x)
      24 -> do_decode_byte_string_n(rest, 1)
      25 -> do_decode_byte_string_n(rest, 2)
      26 -> do_decode_byte_string_n(rest, 4)
      27 -> do_decode_byte_string_n(rest, 8)
      _ -> :error
    end
  end

  defp do_decode_byte_string_indefinite(<<0xFF, rest::binary>>), do: {:ok, <<>>, rest}

  defp do_decode_byte_string_indefinite(bytes) do
    with {:ok, byte_string, bytes} <- decode_byte_string_definite(bytes),
         {:ok, byte_strings, bytes} <- do_decode_byte_string_indefinite(bytes) do
      {:ok, <<byte_string::binary, byte_strings::binary>>, bytes}
    end
  end

  defp do_decode_byte_string(bytes, size) do
    case bytes do
      <<value::binary-size(size), bytes::binary>> ->
        {:ok, value, bytes}

      _ ->
        :error
    end
  end

  defp do_decode_byte_string_n(bytes, n_size) do
    with {:ok, n, bytes} <- do_decode_unsigned_integer(bytes, n_size) do
      do_decode_byte_string(bytes, n)
    end
  end

  # decode_utf8_string

  def decode_utf8_string(
        <<
          3::unsigned-integer-size(3),
          additional_information::unsigned-integer-size(5),
          rest::binary
        >> = bytes
      ) do
    case additional_information do
      31 -> do_decode_utf8_string_indefinite(rest)
      _ -> decode_utf8_string_definite(bytes)
    end
  end

  def decode_utf8_string_definite(<<
        3::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> do_decode_utf8_string(rest, x)
      24 -> do_decode_utf8_string_n(rest, 1)
      25 -> do_decode_utf8_string_n(rest, 2)
      26 -> do_decode_utf8_string_n(rest, 4)
      27 -> do_decode_utf8_string_n(rest, 8)
      _ -> :error
    end
  end

  defp do_decode_utf8_string_indefinite(<<0xFF, rest::binary>>), do: {:ok, <<>>, rest}

  defp do_decode_utf8_string_indefinite(bytes) do
    with {:ok, utf8_string, bytes} <- decode_utf8_string_definite(bytes),
         {:ok, utf8_strings, bytes} <- do_decode_utf8_string_indefinite(bytes) do
      {:ok, <<utf8_string::binary, utf8_strings::binary>>, bytes}
    end
  end

  defp do_decode_utf8_string(bytes, size) do
    do_decode_byte_string(bytes, size)
  end

  defp do_decode_utf8_string_n(bytes, n_size) do
    do_decode_byte_string_n(bytes, n_size)
  end

  # decode_array

  def decode_array(decoder, <<
        4::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> do_decode_array(decoder, rest, x)
      24 -> do_decode_array_n(decoder, rest, 1)
      25 -> do_decode_array_n(decoder, rest, 2)
      26 -> do_decode_array_n(decoder, rest, 4)
      27 -> do_decode_array_n(decoder, rest, 8)
      31 -> do_decode_array_indefinite(decoder, rest)
      _ -> :error
    end
  end

  defp do_decode_array(decoder, bytes, size) do
    case size do
      0 ->
        {:ok, [], bytes}

      size ->
        with {:ok, value, bytes} <- decode(decoder, bytes),
             {:ok, array, bytes} <- do_decode_array(decoder, bytes, size - 1) do
          {:ok, [value | array], bytes}
        end
    end
  end

  defp do_decode_array_n(decoder, bytes, n_size) do
    with {:ok, n, bytes} <- do_decode_unsigned_integer(bytes, n_size) do
      do_decode_array(decoder, bytes, n)
    end
  end

  defp do_decode_array_indefinite(decoder, bytes) do
    case bytes do
      <<0xFF, rest::binary>> ->
        {:ok, [], rest}

      bytes ->
        with {:ok, value, bytes} <- decode(decoder, bytes),
             {:ok, array, bytes} <- do_decode_array_indefinite(decoder, bytes) do
          {:ok, [value | array], bytes}
        end
    end
  end

  # decode_map

  def decode_map(decoder, <<
        5::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> do_decode_map(decoder, rest, x)
      24 -> do_decode_map_n(decoder, rest, 1)
      25 -> do_decode_map_n(decoder, rest, 2)
      26 -> do_decode_map_n(decoder, rest, 4)
      27 -> do_decode_map_n(decoder, rest, 8)
      31 -> do_decode_map_indefinite(decoder, rest)
      _ -> :error
    end
  end

  defp do_decode_map(decoder, bytes, size) do
    case size do
      0 ->
        {:ok, %{}, bytes}

      size ->
        with {:ok, key, bytes} <- decode(decoder, bytes),
             {:ok, value, bytes} <- decode(decoder, bytes),
             {:ok, map, bytes} <- do_decode_map(decoder, bytes, size - 1) do
          {:ok, Map.put(map, key, value), bytes}
        end
    end
  end

  defp do_decode_map_n(decoder, bytes, n_size) do
    with {:ok, n, bytes} <- do_decode_unsigned_integer(bytes, n_size) do
      do_decode_map(decoder, bytes, n)
    end
  end

  defp do_decode_map_indefinite(decoder, bytes) do
    case bytes do
      <<0xFF, rest::binary>> ->
        {:ok, %{}, rest}

      bytes ->
        with {:ok, key, bytes} <- decode(decoder, bytes),
             {:ok, value, bytes} <- decode(decoder, bytes),
             {:ok, map, bytes} <- do_decode_map_indefinite(decoder, bytes) do
          {:ok, Map.put(map, key, value), bytes}
        end
    end
  end

  # decode_tag

  def decode_tag(decoder, <<
        6::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> do_decode_tag(decoder, rest, x)
      24 -> do_decode_tag_n(decoder, rest, 1)
      25 -> do_decode_tag_n(decoder, rest, 2)
      26 -> do_decode_tag_n(decoder, rest, 4)
      27 -> do_decode_tag_n(decoder, rest, 8)
      _ -> :error
    end
  end

  defp do_decode_tag(decoder, bytes, number) do
    with {:ok, content, bytes} <- decode(decoder, bytes) do
      {:ok, %CBOR.Tag{number: number, content: content}, bytes}
    end
  end

  defp do_decode_tag_n(decoder, bytes, number_size) do
    with {:ok, number, bytes} <- do_decode_unsigned_integer(bytes, number_size) do
      do_decode_tag(decoder, bytes, number)
    end
  end

  # decode_floating_point_number_and_simple_value

  def decode_floating_point_number_and_simple_value(<<
        7::unsigned-integer-size(3),
        additional_information::unsigned-integer-size(5),
        rest::binary
      >>) do
    case additional_information do
      x when x < 24 -> do_decode_simple_value(rest, x)
      24 -> do_decode_simple_value_n(rest, 1)
      25 -> do_decode_float(rest, 2)
      26 -> do_decode_float(rest, 4)
      27 -> do_decode_float(rest, 8)
    end
  end

  defp do_decode_simple_value(bytes, value) do
    {:ok, %CBOR.SimpleValue{value: value}, bytes}
  end

  defp do_decode_simple_value_n(bytes, size) do
    with {:ok, value, bytes} <- do_decode_unsigned_integer(bytes, size) do
      do_decode_simple_value(bytes, value)
    end
  end

  defp do_decode_float(bytes, size) do
    case bytes do
      <<value::big-float-size(8 * size), rest::binary>> ->
        {:ok, value, rest}

      _ ->
        {exponent_size, fraction_size} =
          case size do
            2 -> {5, 10}
            4 -> {8, 23}
            8 -> {11, 52}
          end

        exponent = Integer.pow(2, exponent_size) - 1

        case bytes do
          <<
            sign::size(1),
            ^exponent::size(exponent_size),
            fraction::size(fraction_size),
            rest::binary
          >> ->
            value =
              case {sign, fraction} do
                {0, 0} -> :inf
                {1, 0} -> :"-inf"
                _ -> :NaN
              end

            {:ok, value, rest}

          _ ->
            :error
        end
    end
  end
end
