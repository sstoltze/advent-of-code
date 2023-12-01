defmodule Aoc23Test do
  # @related [subject](lib/aoc23.ex)
  use ExUnit.Case

  test "day1-1 example" do
    input = ~S<
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
> |> String.trim() |> String.split("\n")
    assert Aoc23.Day1.day1(input) |> elem(0) == 142
  end

  test "day1-2 example" do
    input = ~S<
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
> |> String.trim() |> String.split("\n")
    assert input |> Enum.map(&Aoc23.Day1.parse_input_two/1) |> Enum.sum() == 281
  end

  test "day1-2 edge case..." do
    assert Aoc23.Day1.parse_input_two("oneight") == 18
  end
end
