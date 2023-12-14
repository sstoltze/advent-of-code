defmodule Aoc23.Day14Test do
  use ExUnit.Case

  test "day 14 examples" do
    input = ~S<
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
> |> String.trim() |> String.split("\n")
    assert {136, _} = Aoc23.Day14.day14(input)
  end

  test "load calculation" do
    input = ~S<
OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....
> |> String.trim() |> String.split("\n")
    assert Aoc23.Day14.calculate_load(input) == 136
  end

  test "rolling north" do
    start = ~S<
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
> |> String.trim() |> String.split("\n")
    rolled = ~S<
OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....
> |> String.trim() |> String.split("\n")
    assert Aoc23.Day14.tilt_north(start) == rolled
  end
end
