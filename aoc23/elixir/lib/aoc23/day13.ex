defmodule Aoc23.Day13 do
  def input(),
    do: Aoc23.input_day(13, 1, "\n\n")

  def day13(input \\ input()) do
    arrays =
      input |> Enum.map(fn a -> a |> String.split("\n") |> Enum.map(&String.to_charlist/1) end)

    reflections = arrays |> Enum.flat_map(&find_reflections/1)

    reflections_with_smudges = arrays |> Enum.flat_map(&find_reflections(&1, 1))

    {reflection_sum(reflections), reflection_sum(reflections_with_smudges)}
  end

  def reflection_sum(reflections) do
    Enum.reduce(reflections, 0, fn {type, index}, acc ->
      acc +
        case type do
          :vertical -> index
          :horizontal -> 100 * index
        end
    end)
  end

  def find_reflections(arr, smudges \\ 0) do
    Enum.concat(
      find_vertical_reflections(arr, smudges),
      find_horizontal_reflections(arr, smudges)
    )
  end

  def find_vertical_reflections(arr, smudges) do
    find_reflection(arr, &array_col/2, Enum.count(Enum.at(arr, 0)), smudges)
    |> Enum.map(&{:vertical, &1})
  end

  def find_horizontal_reflections(arr, smudges) do
    find_reflection(arr, &array_row/2, Enum.count(arr), smudges) |> Enum.map(&{:horizontal, &1})
  end

  def find_reflection(arr, refl_fn, max_index, smudges) do
    1..(max_index - 1)
    |> Enum.filter(fn i ->
      Enum.reduce(
        1..min(i, max_index - i),
        0,
        fn j, acc ->
          acc +
            (Enum.zip(refl_fn.(arr, i + j), refl_fn.(arr, i - j + 1))
             |> Enum.count(fn {a, b} -> a != b end))
        end
      ) == smudges
    end)
  end

  def array_row(arr, i) do
    Enum.at(arr, i - 1)
  end

  def array_col(arr, j) do
    arr |> Enum.map(&Enum.at(&1, j - 1))
  end
end
