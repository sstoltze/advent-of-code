defmodule Aoc23.Day11Test do
  use ExUnit.Case

  test "day 11 example" do
    input = ~S<
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
> |> String.trim() |> String.split()

    assert {374, 8410} = Aoc23.Day11.day11(input, 100)
  end
end
