defmodule Aoc23.Day7 do
  def input(), do: Aoc23.input_day(7)

  @hand_types [
    "Five of a kind",
    "Four of a kind",
    "Full house",
    "Three of a kind",
    "Two pair",
    "One pair",
    "High card"
  ]

  def parse_hand(h, opts \\ []) do
    joker = Keyword.get(opts, :joker, false)
    [cards, bid] = h |> String.split(" ")

    cards =
      cards
      |> String.to_charlist()

    freq =
      cards |> Enum.frequencies()

    updated_freq =
      if joker do
        case Map.pop(freq, ?J) do
          {nil, f} ->
            f |> Map.values() |> Enum.sort(&(&2 <= &1))

          {5, %{}} ->
            [5]

          {k, f} ->
            [best | rest] =
              f
              |> Map.values()
              |> Enum.sort(&(&2 <= &1))

            [best + k | rest]
        end
      else
        freq |> Map.values() |> Enum.sort(&(&2 <= &1))
      end

    hand =
      case updated_freq do
        [5] -> @hand_types |> Enum.at(0)
        [4, 1] -> @hand_types |> Enum.at(1)
        [3, 2] -> @hand_types |> Enum.at(2)
        [3, 1, 1] -> @hand_types |> Enum.at(3)
        [2, 2, 1] -> @hand_types |> Enum.at(4)
        [2, 1, 1, 1] -> @hand_types |> Enum.at(5)
        [1, 1, 1, 1, 1] -> @hand_types |> Enum.at(6)
        _ -> nil
      end

    bid = String.to_integer(bid)

    %{cards: cards, hand: hand, bid: bid}
  end

  def reverse_index_lookup(l, e1, e2) do
    Enum.find_index(l, &(&1 == e1)) > Enum.find_index(l, &(&1 == e2))
  end

  def compare_hand_types(h1, h2) do
    reverse_index_lookup(@hand_types, h1, h2)
  end

  def card_order(opts \\ []) do
    if Keyword.get(opts, :joker, false) do
      "AKQT98765432J"
    else
      "AKQJT98765432"
    end
    |> String.to_charlist()
  end

  def compare_cards(c1, c2, opts \\ []) do
    indices =
      [c1, c2]
      |> Enum.map(fn cards ->
        Enum.map(cards, fn c -> Enum.find_index(card_order(opts), &(&1 == c)) end)
      end)
      |> Enum.zip()

    not (indices
         |> Enum.reduce_while(false, fn {a, b}, _ ->
           if a == b do
             {:cont, false}
           else
             {:halt, a < b}
           end
         end))
  end

  def compare_hands(h1, h2, opts \\ []) do
    (h1[:hand] == h2[:hand] and compare_cards(h1[:cards], h2[:cards], opts)) or
      compare_hand_types(h1[:hand], h2[:hand])
  end

  def day7(input \\ input()) do
    hands = input |> Enum.map(&parse_hand/1)
    sorted_hands = Enum.sort(hands, &compare_hands/2)

    winnings =
      sorted_hands
      |> Enum.with_index(1)
      |> Enum.reduce(0, fn {%{bid: bid}, rank}, acc -> acc + bid * rank end)

    winnings_with_jokers =
      input
      |> Enum.map(&parse_hand(&1, joker: true))
      |> Enum.sort(&compare_hands(&1, &2, joker: true))
      |> Enum.with_index(1)
      |> Enum.reduce(0, fn {%{bid: bid}, rank}, acc -> acc + bid * rank end)

    {winnings, winnings_with_jokers}
  end
end
