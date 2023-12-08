defmodule Aoc23.Day8Test do
  use ExUnit.Case

  test "day 8 example" do
    input = ~S<
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
> |> String.trim() |> String.split("\n\n")
    assert {2, _} = Aoc23.Day8.day8(input)
  end

  test "day 8 example 2" do
    input = ~S<
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
> |> String.trim() |> String.split("\n\n")

    assert {6, _} = Aoc23.Day8.day8(input)
  end

  test "day 8 example 3" do
    input = ~S<
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
> |> String.trim() |> String.split("\n\n")
    assert {_, 6} = Aoc23.Day8.day8(input)
  end
end
