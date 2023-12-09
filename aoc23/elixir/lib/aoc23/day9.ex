defmodule Aoc23.Day9 do
  # @related [test](test/aoc23/day9_test.exs)
  def input(), do: Aoc23.input_day(9)

  def day9(input \\ input()) do
    point_history =
      input
      |> Enum.map(fn p -> p |> String.split(" ") |> Enum.map(&String.to_integer/1) end)

    extrapolated = point_history |> Enum.map(&next_in_seq/1)

    {prev, next} =
      Enum.reduce(extrapolated, fn {p, n}, {acc_p, acc_n} -> {p + acc_p, n + acc_n} end)

    {next, prev}
  end

  def next_in_seq(s, acc \\ {[], []}) do
    if Enum.any?(s, &(&1 != 0)) do
      diffs = difference_of_seq(s)
      {ps, ns} = acc
      next_in_seq(diffs, {[List.first(s) | ps], [List.last(s) | ns]})
    else
      {ps, ns} = acc
      {ps |> Enum.reduce(0, fn k, p -> k - p end), Enum.sum(ns)}
    end
  end

  def difference_of_seq(s) do
    Enum.zip_with([s, Enum.drop(s, 1)], fn elems -> Enum.reduce(elems, &-/2) end)
  end
end
