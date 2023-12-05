defmodule Aoc23 do
  # @related [tests](test/aoc23_test.exs)
  @moduledoc """
  Advent of Code, 2023 - now in elixir
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
