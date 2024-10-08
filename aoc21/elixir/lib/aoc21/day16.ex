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
          0 -> :sum
          1 -> :product
          2 -> :minimum
          3 -> :maximum
          5 -> :greater_than
          6 -> :less_than
          7 -> :equal_to
        end

      body
      |> parse_body(type)
      |> Map.merge(%{version: version, type: type})
    end

    defp parse_body(body, :literal) do
      parse_literal(body, 0)
    end

    defp parse_body(<<0::1, length_in_bits::15, subpackets::bitstring>>, _) do
      <<packets::size(length_in_bits), rest::bitstring>> = subpackets

      <<packets::size(length_in_bits)>> |> parse_subpackets() |> Map.put(:rest, rest)
    end

    defp parse_body(<<1::1, number_of_subpackets::11, subpackets::bitstring>>, _) do
      parse_subpackets(subpackets, number_of_subpackets)
    end

    defp parse_literal(body, current_value) do
      <<last::1, number::4, rest::bitstring>> = body

      new_value = 16 * current_value + number

      case last do
        1 -> parse_literal(rest, new_value)
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

    {sum_versions(packet), packet_value(packet)}
  end

  def sum_versions(packet) do
    packet.version +
      case packet.type do
        :literal ->
          0

        _ ->
          Enum.reduce(packet.subpackets, 0, fn packet, acc -> acc + sum_versions(packet) end)
      end
  end

  def packet_value(%{value: value, type: :literal}) do
    value
  end

  def packet_value(%{subpackets: subpackets, type: type}) do
    packet_values = Enum.map(subpackets, &packet_value/1)

    case type do
      :sum ->
        Enum.sum(packet_values)

      :product ->
        Enum.product(packet_values)

      :minimum ->
        Enum.min(packet_values)

      :maximum ->
        Enum.max(packet_values)

      :greater_than ->
        if Enum.at(packet_values, 0) > Enum.at(packet_values, 1) do
          1
        else
          0
        end

      :less_than ->
        if Enum.at(packet_values, 0) < Enum.at(packet_values, 1) do
          1
        else
          0
        end

      :equal_to ->
        if Enum.at(packet_values, 0) == Enum.at(packet_values, 1) do
          1
        else
          0
        end
    end
  end
end
