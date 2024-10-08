defmodule Aoc21.Day16Test do
  # @related [subject](aoc21/elixir/lib/aoc21/day16.ex)
  use ExUnit.Case

  test "parsing example" do
    assert %{value: 2021, type: 4, version: 6} = Aoc21.Day16.Packet.parse("D2FE28")
  end

  test "day16-1 example" do
    input = ~S<8A004A801A8002F478
> |> String.trim() |> String.split("\n")
    assert {16, _} = Aoc21.Day16.day16(input)
  end
end
