defmodule Aoc25.Day3 do
  def parse(string) do
    string
    |> String.split()
    |> Enum.map(fn battery ->
      battery |> String.split("", trim: true) |> Enum.map(&String.to_integer/1)
    end)
  end

  def part_1(batteries) do
    batteries |> parse() |> Enum.map(&largest_joltage/1) |> Enum.sum()
  end

  def part_2(batteries) do
    batteries |> parse() |> Enum.map(&largest_joltage(&1, 12)) |> Enum.sum()
  end

  def largest_joltage(battery, capacity \\ 2, seen \\ [], highest \\ 0, highest_seen \\ 0) do
    case battery do
      [] ->
        highest

      [dial | remaining] ->
        new_highest =
          Enum.map(seen, fn l ->
            setting = l ++ [dial]

            if Enum.count(setting) < capacity do
              0
            else
              Enum.join(setting) |> String.to_integer()
            end
          end)
          |> Enum.max(&>=/2, fn -> 0 end)

        largest_joltage(
          remaining,
          capacity,
          Enum.filter(
            if dial >= highest_seen do
              [[dial]]
            else
              []
            end ++ Enum.map(seen, &(&1 ++ [dial])),
            &(Enum.count(&1) < capacity)
          ) ++
            seen,
          max(highest, new_highest),
          max(dial, highest_seen)
        )
    end
  end
end
