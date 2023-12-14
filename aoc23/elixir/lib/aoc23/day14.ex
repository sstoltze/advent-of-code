defmodule Aoc23.Day14 do
  def input(), do: Aoc23.input_day(14)

  def day14(input \\ input()) do
    map = input |> tilt_north()
    load = map |> calculate_load()
    {load, 0}
  end

  def tilt_north(map) do
    rolling =
      map
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, i} ->
        row
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.flat_map(fn {c, j} ->
          case c do
            ?O -> [{i, j}]
            _ -> []
          end
        end)
      end)

    empty_map = map |> Enum.map(&String.replace(&1, "O", "."))

    Enum.reduce(rolling, empty_map, fn rock, updated_map ->
      {i, j} = roll_north(rock, updated_map)

      List.update_at(updated_map, i, fn row ->
        row |> String.to_charlist() |> List.update_at(j, fn _ -> ?O end) |> to_string()
      end)
    end)
  end

  def roll_north({0, j}, _), do: {0, j}

  def roll_north({i, j}, map) do
    case map |> Enum.at(i - 1) |> String.at(j) do
      "." -> roll_north({i - 1, j}, map)
      _ -> {i, j}
    end
  end

  def calculate_load(map) do
    len = Enum.count(map)

    map
    |> Enum.with_index()
    |> Enum.map(fn {row, i} ->
      (len - i) * (row |> String.to_charlist() |> Enum.filter(&(&1 == ?O)) |> Enum.count())
    end)
    |> Enum.sum()
  end
end
