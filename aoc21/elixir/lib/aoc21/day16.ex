defmodule Aoc21.Day16 do
  # @related [test](aoc21/elixir/test/aoc21/day16_test.exs)

  defmodule Packet do
    def parse(input) do
      [packet] = input

      <<version::3, type_id::3, body::bitstring>> = Base.decode16!(packet)

      type =
        case type_id do
          4 -> :literal
          _ -> :unknown
        end

      _data = parse_body(body, type)

      %{version: version, type: type_id}
    end

    def parse_body(_body, :literal) do
      nil
    end

    def parse_body(_body, :unknown) do
      nil
    end
  end

  def input do
    Aoc21.input_day(16, 1)
  end

  def day16(input \\ input()) do
    packet = Packet.parse(input)
    dbg(packet)
    {0, 0}
  end
end
