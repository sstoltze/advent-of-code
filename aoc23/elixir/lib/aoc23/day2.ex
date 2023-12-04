defmodule Aoc23.Day2 do
  # @related [tests](test/aoc23_test.exs)
  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers

    def parse_stones(args) do
      case args do
        [n, col] -> %{String.to_existing_atom(col) => n}
        _ -> %{}
      end
    end

    colour = choice([string("blue"), string("red"), string("green")])

    stones = number() |> ignore(whitespace()) |> concat(colour) |> reduce(:parse_stones)

    hand =
      list_of(stones, string(",") |> whitespace())
      |> reduce({Enum, :reduce, [%{}, &Map.merge/2]})

    hands = list_of(hand, string(";") |> whitespace())

    game =
      ignore(string("Game") |> whitespace())
      |> number()
      |> unwrap_and_tag(:game)
      |> ignore(string(":") |> whitespace())
      |> concat(hands)
      |> eos

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
