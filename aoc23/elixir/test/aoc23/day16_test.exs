defmodule Aoc23.Day16Test do
  use ExUnit.Case

  test "day 16 example" do
    input = ~S<
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
> |> String.trim() |> String.split("\n")
    assert {46, 51} = Aoc23.Day16.day16(input)
  end
end
