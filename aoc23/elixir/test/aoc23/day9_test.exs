defmodule Aoc23.Day9Test do
  use ExUnit.Case

  test "day 9 example" do
    input = ~S<
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
> |> String.trim() |> String.split("\n")
    assert {114, 2} = Aoc23.Day9.day9(input)
  end
end
