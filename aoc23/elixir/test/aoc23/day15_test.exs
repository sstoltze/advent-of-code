defmodule Aoc23.Day15Test do
  use ExUnit.Case

  test "day 15 example" do
    input =
      "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |> String.trim() |> String.split("\n")

    assert {1320, 145} = Aoc23.Day15.day15(input)
  end
end
