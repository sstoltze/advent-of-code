defmodule Aoc21.Day16 do
  # @related [test](aoc21/elixir/test/aoc21/day16_test.exs)

  defmodule Packet do
    def parse(input) when is_list(input) do
      Enum.map(input, &parse/1)
    end

    def parse(packet) do
      <<version::3, type_id::3, body::bitstring>> = Base.decode16!(packet)

      type =
        case type_id do
          4 -> :literal
          _ -> :unknown
        end

      data = parse_body(body, type)

      %{version: version, type: type_id, value: data}
    end

    def parse_body(_body, :unknown) do
      nil
    end

    def parse_body(body, :literal, current_value \\ 0) do
      <<last::1, number::4, rest::bitstring>> = body

      new_value = 16 * current_value + number

      case last do
        1 -> parse_body(rest, :literal, new_value)
        0 -> new_value
      end
    end
  end

  def input do
    Aoc21.input_day(16, 1)
  end

  def day16(input \\ input()) do
    _packet = Packet.parse(input)

    {0, 0}
  end
end
