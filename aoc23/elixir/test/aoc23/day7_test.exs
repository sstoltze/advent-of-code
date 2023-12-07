defmodule Aoc23.Day7Test do
  use ExUnit.Case

  test "day 7 example" do
    input = ~S<
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
> |> String.trim() |> String.split("\n")

    assert {6440, 5905} = Aoc23.Day7.day7(input)
  end
end
