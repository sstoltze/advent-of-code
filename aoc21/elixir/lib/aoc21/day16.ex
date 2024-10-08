defmodule Aoc21.Day16 do
  # @related [test](aoc21/elixir/test/aoc21/day16_test.exs)

  defmodule Packet do
    def parse([packet]) do
      parse(packet)
    end

    def parse(packet) do
      packet |> Base.decode16!() |> parse_bitstring()
    end

    defp parse_bitstring(packet) do
      <<version::3, type_id::3, body::bitstring>> = packet

      type =
        case type_id do
          4 -> :literal
          _ -> :operator
        end

      body
      |> parse_body(type)
      |> Map.merge(%{version: version, type: type})
    end

    defp parse_body(<<0::1, length_in_bits::15, subpackets::bitstring>>, :operator) do
      <<packets::size(length_in_bits), _rest::bitstring>> = subpackets
      parse_subpackets(<<packets::size(length_in_bits)>>)
    end

    defp parse_body(<<1::1, number_of_subpackets::11, subpackets::bitstring>>, :operator) do
      parse_subpackets(subpackets, number_of_subpackets)
    end

    defp parse_body(body, :literal, current_value \\ 0) do
      <<last::1, number::4, rest::bitstring>> = body

      new_value = 16 * current_value + number

      case last do
        1 -> parse_body(rest, :literal, new_value)
        0 -> %{value: new_value, rest: rest}
      end
    end

    defp parse_subpackets(packets, n \\ 0) do
      packet = parse_bitstring(packets)

      case {packet.rest, n} do
        {"", _} ->
          %{subpackets: [packet], rest: packet.rest}

        {_, 1} ->
          %{subpackets: [packet], rest: packet.rest}

        _ ->
          Map.merge(%{subpackets: [packet]}, parse_subpackets(packet.rest, n - 1), fn _key,
                                                                                      a,
                                                                                      b ->
            a ++ b
          end)
      end
    end
  end

  def input do
    Aoc21.input_day(16, 1)
  end

  def day16(input \\ input()) do
    packet = Packet.parse(input)

    {sum_versions(packet), 0}
  end

  defp sum_versions(packet) do
    packet.version +
      case packet.type do
        :literal ->
          0

        :operator ->
          Enum.reduce(packet.subpackets, 0, fn packet, acc -> acc + sum_versions(packet) end)
      end
  end
end
