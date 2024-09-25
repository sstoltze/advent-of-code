defmodule Aoc21 do
  # @related [test]
  @moduledoc """
   Advent of Code, 2021
  """
  # Utility functions
  def read_lines(f), do: read_with_separator(f, "\n")

  def read_with_separator(f, separator) do
    case separator do
      nil -> read(f)
      _ -> f |> read() |> String.split(separator)
    end
  end

  def read(f) do
    f |> File.read!() |> String.trim()
  end

  def input_day(i, n \\ 1, separator \\ "\n"),
    do: read_with_separator("../input/day#{i}-#{n}.txt", separator)
end
