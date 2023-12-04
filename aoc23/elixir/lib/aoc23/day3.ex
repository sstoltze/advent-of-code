defmodule Aoc23.Day3 do
  def input(), do: Aoc23.input_day(3)

  def day3(input \\ input()) do
    schematic =
      (input ++ [""])
      |> Enum.zip(["" | input])
      |> Enum.with_index()
      |> Enum.reduce(%{parts: [], gears: []}, fn {{line, prev_line}, index}, acc ->
        parts = parts_in_line(line, index, input)

        gears = gears_in_line(prev_line, index - 1, acc[:parts] ++ parts)

        acc
        |> Map.update!(:parts, fn ps -> Enum.concat(ps, parts) end)
        |> Map.update!(:gears, fn gs -> Enum.concat(gs, gears) end)
      end)

    {schematic[:parts] |> Enum.map(fn p -> p[:part] end) |> Enum.sum(),
     schematic[:gears] |> Enum.map(fn g -> g[:ratio] end) |> Enum.sum()}
  end

  def find_number_offsets(line) do
    Regex.scan(~r/[[:digit:]]+/, line, return: :index) |> Enum.map(&List.first/1)
  end

  def parts_in_line(line, line_index, lines) do
    line
    |> find_number_offsets()
    |> Enum.flat_map(fn offset ->
      adjacents = get_adjacent(offset, line_index, lines)

      if Enum.any?(adjacents, &is_symbol?/1) do
        {index, length} = offset
        part = String.slice(line, index, length) |> String.to_integer()
        %{line_index: line_index, part: part, col_range: index..(index + length - 1)}
      end
      |> List.wrap()
    end)
  end

  def is_symbol?(c) do
    c != "." and
      not Regex.match?(~r/[[:digit:]]+/, c)
  end

  def get_adjacent({index, length}, line_index, lines) do
    Enum.flat_map(
      -1..length,
      fn row_offset ->
        Enum.flat_map(-1..1, fn col_offset ->
          lines
          |> Enum.at(line_index + col_offset, "")
          |> String.at(index + row_offset)
          |> List.wrap()
        end)
      end
    )
  end

  def gears_in_line(line, line_index, parts) do
    Regex.scan(~r/\*/, line, return: :index)
    |> Enum.flat_map(fn [{index, 1}] ->
      adjacent_parts =
        Enum.flat_map(-1..1, fn row_offset ->
          Enum.flat_map(-1..1, fn col_offset ->
            parts
            |> Enum.filter(fn p ->
              p[:line_index] == line_index + row_offset and
                Enum.any?(p[:col_range], fn c -> c == index + col_offset end)
            end)
          end)
        end)
        |> Enum.uniq()

      case adjacent_parts do
        [p1, p2] -> %{ratio: p1[:part] * p2[:part]}
        _ -> nil
      end
      |> List.wrap()
    end)
  end
end
