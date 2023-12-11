defmodule Aoc23.Day10Test do
  use ExUnit.Case

  test "day 10 example" do
    input = ~S<
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
> |> String.trim() |> String.split("\n")

    assert {_, 4} = Aoc23.Day10.day10(input)
  end
end
