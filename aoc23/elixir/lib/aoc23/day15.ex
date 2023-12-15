defmodule Aoc23.Day15 do
  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers
  end

  def input(), do: Aoc23.input_day(15)

  def day15(input \\ input()) do
    steps = input |> Enum.flat_map(&String.split(&1, ","))
    sum_of_hash = steps |> Enum.map(&hash/1) |> Enum.sum()
    {sum_of_hash, 0}
  end

  def hash(step) do
    step |> String.to_charlist() |> Enum.reduce(0, fn c, acc -> ((acc + c) * 17) |> rem(256) end)
  end
end
