defmodule Aoc21.Day16Test do
  # @related [subject](aoc21/elixir/lib/aoc21/day16.ex)
  use ExUnit.Case

  test "parsing example" do
    assert %{value: 2021, type: :literal, version: 6} = Aoc21.Day16.Packet.parse("D2FE28")
  end

  test "operator example, type 0" do
    assert %{
             subpackets: [%{type: :literal, value: 10}, %{type: :literal, value: 20}],
             type: :operator,
             version: 1
           } = Aoc21.Day16.Packet.parse("38006F45291200")
  end

  test "operator example, type 1" do
    assert %{
             subpackets: [
               %{type: :literal, value: 1},
               %{type: :literal, value: 2},
               %{type: :literal, value: 3}
             ],
             type: :operator,
             version: 7
           } = Aoc21.Day16.Packet.parse("EE00D40C823060")
  end

  test "day16-1 example" do
    input = ~S<8A004A801A8002F478
> |> String.trim() |> String.split("\n")
    assert {16, _} = Aoc21.Day16.day16(input)
  end
end
