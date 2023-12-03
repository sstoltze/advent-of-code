defmodule Aoc23 do
  # @related [tests](test/aoc23_test.exs)
  @moduledoc """
  Advent of Code, 2023 - now in elixir
  """
  defmodule Day1 do
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

  defmodule Day2 do
    defmodule Parser do
      import NimbleParsec
      colour = choice([string("blue"), string("red"), string("green")])
      int = integer(min: 1)

      def parse_stones(args) do
        case args do
          [n, col] -> %{String.to_existing_atom(col) => n}
          _ -> %{}
        end
      end

      stones = int |> concat(ignore(string(" "))) |> concat(colour) |> reduce(:parse_stones)

      hand =
        stones
        |> concat(ignore(string(", ")))
        # A hand is a (possibly empty) list of stones
        |> repeat()
        # But with at least one
        |> concat(stones)
        # Followed by either a ; or the end of the string
        |> concat(ignore(choice([string("; "), eos()])))
        |> reduce({Enum, :reduce, [%{}, &Map.merge/2]})

      hands = hand |> repeat() |> eos()

      game =
        ignore(string("Game "))
        |> concat(int)
        |> unwrap_and_tag(:game)
        |> concat(ignore(string(": ")))
        |> concat(hands)

      defparsec(:colour, colour)
      defparsec(:game, game)
      defparsec(:hand, hand)
      defparsec(:hands, hands)

      def parse_game!(g) do
        {:ok, game, "", _, _, _} = game(g)
        game
      end
    end

    def input(), do: Aoc23.input_day(2)

    def day2(input \\ input(), constraint \\ %{red: 12, green: 13, blue: 14}) do
      games =
        input |> Enum.map(&Parser.parse_game!/1)

      sum_of_valid_games =
        games
        |> Enum.filter(&possible_game?(constraint, &1))
        |> Enum.reduce(0, fn [{:game, n} | _hands], acc -> n + acc end)

      power_sum = games |> Enum.map(fn g -> g |> minimum_required |> power end) |> Enum.sum()

      {sum_of_valid_games, power_sum}
    end

    def possible_game?(constraint, game) do
      [{:game, _n} | hands] = game
      Enum.all?(hands, fn h -> possible_hand?(constraint, h) end)
    end

    def possible_hand?(constraint, hand) do
      Enum.all?([:blue, :red, :green], fn col ->
        Map.get(hand, col, 0) <= Map.get(constraint, col, 0)
      end)
    end

    def minimum_required(game) do
      [{:game, _n} | hands] = game

      Enum.reduce(hands, %{blue: 0, red: 0, green: 0}, fn hand, acc ->
        Map.merge(acc, hand, fn _k, v1, v2 -> max(v1, v2) end)
      end)
    end

    def power(m) do
      m |> Map.values() |> Enum.reduce(&(&1 * &2))
    end
  end

  defmodule Day3 do
    def input(), do: Aoc23.input_day(3)

    def day3(input \\ input()) do
      schematic =
        (input ++ [""])
        |> Enum.zip(["" | input])
        |> Enum.with_index()
        |> Enum.reduce(%{parts: [], gears: []}, fn {{line, prev_line}, index}, acc ->
          parts = parts_in_line(line, index, input)

          gears = gears_in_line(prev_line, index - 1, input, acc[:parts] ++ parts)

          acc
          |> Map.update!(:parts, fn ps -> Enum.concat(ps, parts) end)
          |> Map.update!(:gears, fn gs -> Enum.concat(gs, gears) end)
        end)

      {schematic[:parts] |> Enum.map(fn p -> p[:part] end) |> Enum.sum(),
       schematic[:gears] |> Enum.map(fn g -> g[:ratio] end) |> Enum.sum()}
    end

    def find_number_offsets(line) do
      Regex.scan(~r/[[:digit:]]+/, line, return: :index) |> Enum.map(&List.first/1)
    end

    def parts_in_line(line, line_index, lines) do
      line
      |> find_number_offsets()
      |> Enum.flat_map(fn offset ->
        adjacents = get_adjacent(offset, line_index, lines)

        if Enum.any?(adjacents, &is_symbol?/1) do
          {index, length} = offset
          part = String.slice(line, index, length) |> String.to_integer()
          %{line_index: line_index, part: part, col_range: index..(index + length - 1)}
        end
        |> List.wrap()
      end)
    end

    def is_symbol?(c) do
      c != "." and
        not Regex.match?(~r/[[:digit:]]+/, c)
    end

    def get_adjacent({index, length}, line_index, lines) do
      Enum.flat_map(
        -1..length,
        fn row_offset ->
          Enum.flat_map(-1..1, fn col_offset ->
            lines
            |> Enum.at(line_index + col_offset, "")
            |> String.at(index + row_offset)
            |> List.wrap()
          end)
        end
      )
    end

    def gears_in_line(line, line_index, lines, parts) do
      Regex.scan(~r/\*/, line, return: :index)
      |> Enum.flat_map(fn [{index, 1}] ->
        adjacent_parts =
          Enum.flat_map(-1..1, fn row_offset ->
            Enum.flat_map(-1..1, fn col_offset ->
              parts
              |> Enum.filter(fn p ->
                p[:line_index] == line_index + row_offset and
                  Enum.any?(p[:col_range], fn c -> c == index + col_offset end)
              end)
            end)
          end)
          |> Enum.uniq()

        case adjacent_parts do
          [p1, p2] -> %{ratio: p1[:part] * p2[:part]}
          _ -> nil
        end
        |> List.wrap()
      end)
    end
  end

  # Utility functions
  def read_lines(f) do
    f |> File.read!() |> String.trim() |> String.split("\n")
  end

  def input_day(i, n \\ 1), do: read_lines("../input/day#{i}-#{n}.txt")
end
