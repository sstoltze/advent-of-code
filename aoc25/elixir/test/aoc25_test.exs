defmodule Aoc25Test do
  use ExUnit.Case

  test "day 1" do
    input = ~S"
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
" |> Aoc25.Day1.parse()

    assert 3 == Aoc25.Day1.part_1(input)
  end
end
