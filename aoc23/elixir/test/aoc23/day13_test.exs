defmodule Aoc23.Day13Test do
  use ExUnit.Case

  test "day 12 example" do
    input = ~S<
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
> |> String.trim() |> String.split("\n\n")

    assert {405, 400} = Aoc23.Day13.day13(input)
  end
end
