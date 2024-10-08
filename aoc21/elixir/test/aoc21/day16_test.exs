defmodule Aoc21.Day16Test do
  # @related [subject](aoc21/elixir/lib/aoc21/day16.ex)
  use ExUnit.Case

  test "parsing example" do
    assert %{value: 2021, type: :literal, version: 6} = Aoc21.Day16.Packet.parse("D2FE28")
  end

  test "operator example, type 0" do
    assert %{
             subpackets: [%{type: :literal, value: 10}, %{type: :literal, value: 20}],
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
             version: 7
           } = Aoc21.Day16.Packet.parse("EE00D40C823060")
  end

  test "version sum" do
    assert {16, _} = Aoc21.Day16.day16("8A004A801A8002F478")
    assert {12, _} = Aoc21.Day16.day16("620080001611562C8802118E34")
    assert {23, _} = Aoc21.Day16.day16("C0015000016115A2E0802F182340")
    assert {31, _} = Aoc21.Day16.day16("A0016C880162017C3686B18A3D4780")
  end

  test "day16-1 example" do
    input = ~S<8A004A801A8002F478
> |> String.trim() |> String.split("\n")
    assert {16, _} = Aoc21.Day16.day16(input)
  end

  test "day16-2 examples" do
    assert {_, 3} = Aoc21.Day16.day16("C200B40A82")
    assert {_, 54} = Aoc21.Day16.day16("04005AC33890")
    assert {_, 7} = Aoc21.Day16.day16("880086C3E88112")
    assert {_, 9} = Aoc21.Day16.day16("CE00C43D881120")
    assert {_, 1} = Aoc21.Day16.day16("D8005AC2A8F0")
    assert {_, 0} = Aoc21.Day16.day16("F600BC2D8F")
    assert {_, 0} = Aoc21.Day16.day16("9C005AC2F8F0")
    assert {_, 1} = Aoc21.Day16.day16("9C0141080250320F1802104A08")
  end
end
