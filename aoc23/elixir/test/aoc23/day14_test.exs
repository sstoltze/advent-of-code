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
    assert {136, 64} == Aoc23.Day14.day14(input)
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
    assert Aoc23.Day14.tilt(start, :north) == rolled
  end

  test "repeat spin cycle" do
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
    one = ~S<
.....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#....
> |> String.trim() |> String.split("\n")
    two = ~S<
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#..OO###..
#.OOO#...O
> |> String.trim() |> String.split("\n")
    three = ~S<
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O
> |> String.trim() |> String.split("\n")

    assert Enum.map(0..3, fn i -> Aoc23.Day14.repeat(&Aoc23.Day14.spin_cycle/1, i, start) end) ==
             [
               start,
               one,
               two,
               three
             ]
  end
end
