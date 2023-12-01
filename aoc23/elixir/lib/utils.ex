defmodule Utils do
  def read_lines(f) do
    f |> File.read!() |> String.trim() |> String.split("\n")
  end

  def input_day(i, n \\ 1) do
    read_lines("../input/day#{i}-#{n}.txt")
  end
end
