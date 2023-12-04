defmodule Aoc23.Day1 do
  # @related [tests](test/aoc23_test.exs)
  def input do
    Aoc23.input_day(1, 1)
  end

  def calibration(list) do
    list
    |> (fn l -> {List.first(l, nil), List.last(l, nil)} end).()
    |> (fn {a, b} -> String.to_integer("#{a}#{b}") end).()
  end

  def parse_input(input) do
    input
    |> String.split(~r/[[:alpha:]]?/, trim: true)
    |> calibration
  end

  def find_all_matches(regex, input, offset \\ 0) do
    case Regex.run(regex, input, offset: offset, return: :index) do
      nil -> []
      [{i, l}] -> [String.slice(input, i, l) | find_all_matches(regex, input, i + 1)]
    end
  end

  def parse_input_two(input) do
    numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    regex = ~r/#{Enum.join(numbers, "|")}|[[:digit:]]/

    find_all_matches(regex, input)
    |> Enum.map(fn d ->
      case Enum.find_index(numbers, fn x -> x == d end) do
        nil -> d
        i -> i + 1
      end
    end)
    |> calibration()
  end

  def day1(input \\ input()) do
    input
    |> Enum.map(&{parse_input(&1), parse_input_two(&1)})
    |> Enum.reduce({0, 0}, fn {x, y}, {a, b} -> {x + a, y + b} end)
  end
end
