defmodule Aoc23.Day11 do
  def input(), do: Aoc23.input_day(11)

  def day11(input \\ input(), scale_distance \\ 1_000_000) do
    {_, empty_rows} =
      input
      |> Enum.with_index()
      |> Enum.filter(fn {row, _i} ->
        row |> String.to_charlist() |> Enum.all?(fn c -> c == ?. end)
      end)
      |> Enum.unzip()

    empty_cols =
      0..(String.length(List.first(input)) - 1)
      |> Enum.filter(fn j -> get_column(input, j) |> Enum.all?(fn c -> c == ?. end) end)

    galaxies =
      input
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, i} ->
        row
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.flat_map(fn {c, j} ->
          if c == ?\# do
            [{i, j}]
          else
            []
          end
        end)
      end)

    pairs_of_galaxies = make_pairs(galaxies)

    distances = pairs_of_galaxies |> Enum.map(&galaxy_distance(&1, empty_rows, empty_cols))

    old_distances =
      pairs_of_galaxies |> Enum.map(&galaxy_distance(&1, empty_rows, empty_cols, scale_distance))

    {Enum.sum(distances), Enum.sum(old_distances)}
  end

  def galaxy_distance({{a, b}, {c, d}}, empty_rows, empty_cols, scale_distance \\ 2) do
    [row_start, row_end] = Enum.sort([a, c])
    [col_start, col_end] = Enum.sort([b, d])

    row_end - row_start + col_end - col_start +
      (scale_distance - 1) *
        ((empty_rows |> Enum.filter(fn i -> i in row_start..row_end end) |> Enum.count()) +
           (empty_cols |> Enum.filter(fn j -> j in col_start..col_end end) |> Enum.count()))
  end

  def make_pairs([]), do: []

  def make_pairs([x | xs]) do
    Enum.concat(Enum.map(xs, &{x, &1}), make_pairs(xs))
  end

  def get_column(arr, j) do
    Enum.map(arr, fn r -> r |> String.to_charlist() |> Enum.at(j) end)
  end
end
