defmodule Aoc15 do
  @moduledoc """
  Documentation for Aoc15.
  """

  @doc """
  Hello world.

  ## Examples

  iex> Aoc15.hello()
  :world

  """
  def hello do
    :world
  end

  def day1() do
    {:ok, input} = File.read("../input/day-one-input")
    final_floor = input |> String.graphemes |> Enum.map(fn c -> if c == "(" do 1 else -1 end end) |> Enum.sum
    IO.puts("Day 1-1: The final floor is #{final_floor}.")
    indexed_list = input |> String.graphemes |> Enum.zip(1..String.length(input))
  {-1, answer} = Enum.reduce(indexed_list, {0,0}, fn x, acc ->
      {current_floor, result} = acc
      if current_floor == -1
        do {current_floor, result}
        else
          case x do
            {"(", index} -> {current_floor + 1, index}
            {")", index} -> {current_floor - 1, index}
          end
      end
    end)
  IO.puts("Day 1-2: The basement is reached after #{answer} steps.")
  end

  def day2() do
    {:ok, input} = File.read("../input/day-two-input")
    boxes = input
    |> String.split("\n", trim: true)
    |> Enum.map(fn b -> String.split(b, "x", trim: true) |> Enum.map(&String.to_integer/1) end)
  {total_paper, total_ribbon} = Enum.reduce(boxes, {0, 0}, fn b, acc ->
      {acc_paper, acc_ribbon} = acc
      [l , w , h] = b
      sides = [l * w, l * h, w * h]
      area = 2 * Enum.sum(sides)
      smallest_side = Enum.min(sides)
      paper = area + smallest_side
      [s1, s2 | _rest] = Enum.sort(b)
      perimeter = 2 * (s1 + s2)
      volume = Enum.reduce(b, 1, fn s, acc -> s * acc end)
      ribbon = perimeter + volume
      {acc_paper + paper, acc_ribbon + ribbon}
    end)
  IO.puts("Day 2-1: The total amount of wrapping paper is #{total_paper}")
  IO.puts("Day 2-2: The total amount of ribbon is #{total_ribbon}")
  end

  def day4 do
    {:ok, input} = File.read("../input/day-four-input")
    md5 = fn n ->
      :crypto.hash(:md5, "#{input}#{n}") |> Base.encode16(case: :lower)
    end

    md5.(3938038)
  end
end
