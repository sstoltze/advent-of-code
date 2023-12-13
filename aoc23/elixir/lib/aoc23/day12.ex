defmodule Aoc23.Day12 do
  def input(), do: Aoc23.input_day(12)

  def day12(input \\ input()) do
    filled = input |> Enum.map(&parse_input/1) |> Enum.map(&fill_springs/1)

    {filled |> Enum.map(&Enum.count/1) |> Enum.sum(), 0}
  end

  def parse_input(record) do
    [damaged, springs] = String.split(record, " ")

    {remove_empties(damaged), springs |> String.split(",") |> Enum.map(&String.to_integer/1)}
  end

  def remove_empties(spring_record) do
    spring_record
    |> String.split(~r/\.+/)
    |> Enum.reject(&(String.length(&1) == 0))
  end

  def fill_springs({spring_record, springs}) do
    case spring_record do
      [] ->
        if Enum.empty?(springs) do
          [[]]
        else
          []
        end

      [group | rest] ->
        potential = group |> fill_unknowns() |> Enum.map(&remove_empties/1)

        Enum.flat_map(potential, fn g ->
          {g_lengths, rest_lengths} = Enum.split(springs, Enum.count(g))

          if Enum.map(g, &String.length/1) == g_lengths do
            fill_springs({rest, rest_lengths}) |> Enum.map(&Enum.concat(g, &1))
          else
            []
          end
        end)
    end
  end

  def fill_unknowns(s) do
    s
    |> String.to_charlist()
    |> Enum.reverse()
    |> Enum.reduce([[]], fn c, acc ->
      case c do
        ?? -> Enum.flat_map(acc, &[[?. | &1], [?\# | &1]])
        _ -> Enum.map(acc, &[c | &1])
      end
    end)
    |> Enum.map(&to_string/1)
  end
end
