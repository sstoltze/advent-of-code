defmodule Aoc25.Day2 do
  def parse(string) do
    string
    |> String.trim()
    |> String.split(",")
    |> Enum.map(fn range ->
      [first, last] = String.split(range, "-") |> Enum.map(&String.to_integer/1)
      first..last
    end)
  end

  def part_1(ranges) do
    ranges
    |> parse()
    |> Enum.flat_map(fn range -> Enum.filter(range, &invalid?/1) end)
    |> Enum.sum()
  end

  def part_2(ranges) do
    ranges
    |> parse()
    |> Enum.flat_map(fn range -> Enum.filter(range, &really_invalid?/1) end)
    |> Enum.sum()
  end

  def invalid?(number) do
    "#{number}" =~ ~r/^(.+)(\1){1}$/
  end

  def really_invalid?(number) do
    "#{number}" =~ ~r/^(.+)(\1){1,}$/
  end
end
