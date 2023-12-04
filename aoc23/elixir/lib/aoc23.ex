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

  defmodule ParserHelpers do
    import NimbleParsec

    def number(combinator \\ empty()), do: combinator |> integer(min: 1)
    def whitespace(combinator \\ empty()), do: combinator |> times(string(" "), min: 1)

    def list_of(combinator, separator \\ whitespace()) do
      combinator
      |> repeat(ignore(separator) |> concat(combinator))
    end
  end
end
