defmodule Aoc23.Day14 do
  def input(), do: Aoc23.input_day(14)

  def day14(input \\ input()) do
    map = input |> tilt(:north)
    load = map |> calculate_load()

    # repeat(&spin_cycle/1, 1_000_000_000, map) |> calculate_load()
    cycled_load = 64
    {load, cycled_load}
  end

  def tilt(map, direction) do
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
      |> sort_by_direction(direction)

    empty_map = map |> Enum.map(&String.replace(&1, "O", "."))

    Enum.reduce(rolling, empty_map, fn rock, updated_map ->
      {i, j} = roll(rock, updated_map, direction)

      List.update_at(updated_map, i, fn row ->
        row |> String.to_charlist() |> List.update_at(j, fn _ -> ?O end) |> to_string()
      end)
    end)
  end

  def sort_by_direction(rocks, direction) do
    get_index =
      case direction do
        :north -> &elem(&1, 0)
        :south -> &elem(&1, 0)
        :east -> &elem(&1, 1)
        :west -> &elem(&1, 1)
      end

    compare =
      case direction do
        :north -> &<=/2
        :south -> &>=/2
        :east -> &>=/2
        :west -> &<=/2
      end

    Enum.sort(
      rocks,
      &compare.(get_index.(&1), get_index.(&2))
    )
  end

  def spin_cycle(map) do
    map |> tilt(:north) |> tilt(:west) |> tilt(:south) |> tilt(:east)
  end

  def roll({i, j}, map, direction) do
    if at_edge({i, j}, map, direction) do
      {i, j}
    else
      {a, b} = next({i, j}, direction)

      case map |> Enum.at(a) |> String.at(b) do
        "." -> roll({a, b}, map, direction)
        _ -> {i, j}
      end
    end
  end

  def at_edge({i, j}, map, direction) do
    case direction do
      :north -> i == 0
      :south -> i == Enum.count(map) - 1
      :east -> j == String.length(List.first(map)) - 1
      :west -> j == 0
    end
  end

  def next({i, j}, direction) do
    case direction do
      :north -> {i - 1, j}
      :south -> {i + 1, j}
      :east -> {i, j + 1}
      :west -> {i, j - 1}
    end
  end

  def repeat(_, 0, input), do: input

  def repeat(f, n, input) do
    res = f.(input)

    if res == input do
      res
    else
      repeat(f, n - 1, res)
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
