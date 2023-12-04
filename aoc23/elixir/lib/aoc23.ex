defmodule Aoc23 do
  # @related [tests](test/aoc23_test.exs)
  @moduledoc """
  Advent of Code, 2023 - now in elixir
  """
  # Utility functions
  def read_lines(f) do
    f |> File.read!() |> String.trim() |> String.split("\n")
  end

  def input_day(i, n \\ 1), do: read_lines("../input/day#{i}-#{n}.txt")
end
