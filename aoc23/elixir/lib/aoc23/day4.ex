defmodule Aoc23.Day4 do
  # @related [tests](test/aoc23/day4_test.exs)
  def input(), do: Aoc23.input_day(4)

  defmodule ScratchCard do
    defstruct [:card_number, :winning_numbers, :your_numbers]

    defmodule Parser do
      import NimbleParsec
      import Aoc23.ParserHelpers

      number_list = list_of(number())

      scratch_card =
        ignore(string("Card") |> whitespace())
        |> concat(number() |> unwrap_and_tag(:card_number))
        |> ignore(string(":") |> whitespace())
        |> concat(number_list |> tag(:winning_numbers))
        |> ignore(whitespace() |> string("|") |> whitespace())
        |> concat(number_list |> tag(:your_numbers))
        |> eos

      defparsec(:scratch_card, scratch_card)
    end

    def parse(s) do
      {:ok, card, "", _, _, _} = s |> Parser.scratch_card()
      struct(ScratchCard, card)
    end
  end

  def day4(input \\ input()) do
    cards = input |> Enum.map(&ScratchCard.parse/1)
    score = cards |> Enum.map(fn c -> c |> matches |> score_matches end) |> Enum.sum()

    won_cards =
      cards
      |> Enum.map(fn c -> {1, c} end)
      |> process_wins()
      |> Enum.reduce(0, fn {k, _}, acc -> acc + k end)

    {score, won_cards}
  end

  def matches(%ScratchCard{winning_numbers: winning, your_numbers: yours}) do
    MapSet.intersection(MapSet.new(winning), MapSet.new(yours)) |> MapSet.to_list()
  end

  def score_matches(l) do
    case l do
      [] -> 0
      _ -> 2 ** (Enum.count(l) - 1)
    end
  end

  def process_wins(cards, result \\ [])

  def process_wins([], result) do
    result
  end

  def process_wins([{n, card} | cards], result) do
    matches = card |> matches |> Enum.count()
    {to_dupe, rest} = Enum.split(cards, matches)
    process_wins(Enum.map(to_dupe, fn {k, c} -> {k + n, c} end) ++ rest, [{n, card} | result])
  end
end
