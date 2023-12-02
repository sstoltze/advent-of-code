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
    assert {142, _} = Aoc23.Day1.day1(input)
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

  test "day2-1 example" do
    input = ~S<
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
> |> String.trim() |> String.split("\n")
    assert {8, 2286} = Aoc23.Day2.day2(input)
  end
end
