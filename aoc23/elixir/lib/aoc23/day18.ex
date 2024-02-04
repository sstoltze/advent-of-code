defmodule Aoc23.Day18 do
  def input(), do: Aoc23.input_day(18)

  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers
    def to_direction(0), do: :R
    def to_direction(1), do: :D
    def to_direction(2), do: :L
    def to_direction(3), do: :U

    direction =
      choice([string("U"), string("D"), string("L"), string("R")])
      |> map({String, :to_atom, []})
      |> unwrap_and_tag(:direction)

    meters = integer(min: 1) |> unwrap_and_tag(:meters)

    colour =
      ignore(string("#"))
      |> concat(
        choice([integer(1), ascii_string([?a..?f], 1)])
        |> times(5)
        |> reduce({Enum, :join, [""]})
        |> map({String, :to_integer, [16]})
        |> unwrap_and_tag(:meters)
      )
      |> concat(integer(1) |> map(:to_direction) |> unwrap_and_tag(:direction))
      |> tag(:colour)

    defparsec(
      :instruction,
      direction
      |> concat(ignore(whitespace()))
      |> concat(meters)
      |> ignore(whitespace())
      |> ignore(string("("))
      |> concat(colour)
      |> ignore(string(")"))
      |> eos
    )
  end

  def parse_instruction(i) do
    {:ok, instruction, "", %{}, _, _} = Parser.instruction(i)
    instruction
  end

  def day18(input \\ input()) do
    instructions = input |> Enum.map(&parse_instruction/1)
    loop = dig_loop(instructions)
    loop_area = calculate_loop_area(loop)

    colour_loop = instructions |> Enum.map(&Keyword.get(&1, :colour)) |> dig_loop()
    colour_loop_area = calculate_loop_area(colour_loop)

    {
      loop_area,
      # 0
      colour_loop_area
    }
  end

  def calculate_loop_area(loop) do
    crossings =
      calculate_crossings(loop)

    Enum.reduce(crossings, 0, fn {k, {_, a, _, _}}, acc ->
      acc + a * Enum.count(k)
    end)
  end

  def next({i, j}, instruction) do
    dir = Keyword.get(instruction, :direction)
    m = Keyword.get(instruction, :meters)

    {non_corners, {ending_row, ending_column}} =
      case dir do
        :U -> {[row: (i - m + 1)..(i - 1), col: j..j], {i - m, j}}
        :D -> {[row: (i + 1)..(i + m - 1), col: j..j], {i + m, j}}
        :R -> {[row: i..i, col: (j + 1)..(j + m - 1)], {i, j + m}}
        :L -> {[row: i..i, col: (j - m + 1)..(j - 1)], {i, j - m}}
      end

    path =
      if m == 1 do
        []
      else
        [non_corners |> Keyword.put(:direction, dir) |> Keyword.put(:corner, false)]
      end

    corner = [
      row: ending_row..ending_row,
      col: ending_column..ending_column,
      previous_direction: dir,
      corner: true,
      coordinates: {ending_row, ending_column}
    ]

    {path, corner}
  end

  def update_start(loop) do
    [origin | paths] = loop |> Enum.drop(-1)
    path_from_origin = List.last(paths)

    updated_origin =
      if origin |> Keyword.get(:previous_direction) == path_from_origin |> Keyword.get(:direction) do
        # Entering the same direction that we are leaving, so this is not a corner
        [row: 0..0, col: 0..0, corner: false, direction: Keyword.get(origin, :previous_direction)]
      else
        origin |> Keyword.put(:next_direction, path_from_origin |> Keyword.get(:direction))
      end

    [updated_origin | paths]
  end

  def dig_loop(instructions) do
    Enum.reduce(
      instructions,
      [[coordinates: {0, 0}]],
      fn i, path ->
        [old_corner | old_path] = path
        current_point = Keyword.get(old_corner, :coordinates)

        {next_part, next_corner} =
          next(current_point, i)

        [next_corner] ++
          next_part ++
          [Keyword.put(old_corner, :next_direction, i |> Keyword.get(:direction))] ++
          old_path
      end
    )
    |> update_start()
  end

  def mapset_to_ranges(ms) do
    if Enum.empty?(ms) do
      []
    else
      sorted = ms |> MapSet.to_list() |> Enum.sort()
      [first | rest] = sorted

      {_, _, last, rest} =
        Enum.reduce(rest, {first, first, first..first, []}, fn j,
                                                               {current_start, last_seen,
                                                                _in_progress, final} ->
          if j > last_seen + 1 do
            {j, j, j..j, [current_start..last_seen | final]}
          else
            {current_start, j, current_start..j, final}
          end
        end)

      [last | rest]
    end
  end

  def calculate_intersection_range(range, other) do
    ms = MapSet.new(range)
    other_ms = MapSet.new(other)

    {
      mapset_to_ranges(MapSet.difference(ms, other_ms)),
      mapset_to_ranges(MapSet.intersection(ms, other_ms)),
      mapset_to_ranges(MapSet.difference(other_ms, ms))
    }
  end

  def crossing_update(
        part,
        {current_crossing, loop_or_inside_count, previous_corner, last_loop_part}
      ) do
    {col_min, col_max} = part |> Keyword.get(:col) |> Enum.min_max()
    from_loop_count = col_max - col_min + 1

    crossing_number_from_loop =
      case Keyword.get(part, :corner) do
        false ->
          case Keyword.get(part, :direction) do
            :U -> 1
            :D -> 1
            _ -> 0
          end

        true ->
          case previous_corner do
            nil ->
              0

            _ ->
              calculate_new_crossing(previous_corner, part)
          end
      end

    count_from_inside =
      if rem(current_crossing, 2) == 1 do
        prev_loop_col_max = last_loop_part |> Keyword.get(:col) |> Enum.max()
        col_min - prev_loop_col_max - 1
      else
        0
      end

    new_corner =
      case Keyword.get(part, :corner) do
        true ->
          if is_nil(previous_corner) do
            part
          else
            nil
          end

        false ->
          previous_corner
      end

    {current_crossing + crossing_number_from_loop,
     loop_or_inside_count + from_loop_count + count_from_inside, new_corner, part}
  end

  def calculate_crossings(loop) do
    loop_parts_by_start = loop |> Enum.sort_by(&Enum.min(Keyword.get(&1, :col)))

    Enum.reduce(loop_parts_by_start, %{}, fn part, acc ->
      part_row = Keyword.get(part, :row)

      case Map.keys(acc) do
        [] ->
          %{part_row => crossing_update(part, {0, 0, nil, nil})}

        keys ->
          {new, new_acc} =
            Enum.reduce(keys, {[], %{}}, fn in_progress, {new, updated} ->
              val = Map.get(acc, in_progress)

              {new_ranges, intersections, unaltered_ranges} =
                calculate_intersection_range(part_row, in_progress)

              {case {new, new_ranges} do
                 {nil, _} -> nil
                 {_, []} -> nil
                 _ -> new_ranges ++ new
               end,
               updated
               |> Map.merge(
                 intersections
                 |> Enum.map(fn r -> %{r => crossing_update(part, val)} end)
                 |> Enum.reduce(%{}, &Map.merge/2)
               )
               |> Map.merge(
                 unaltered_ranges
                 |> Enum.map(fn r -> %{r => val} end)
                 |> Enum.reduce(%{}, &Map.merge/2)
               )}
            end)

          not_in_progress =
            case new do
              nil ->
                []

              _ ->
                Enum.reduce(
                  new |> List.wrap(),
                  &(calculate_intersection_range(&1, &2) |> elem(1) |> List.first([]))
                )
            end

          case not_in_progress do
            [] ->
              new_acc

            range ->
              new_acc
              |> Map.put(
                range,
                crossing_update(part, {0, 0, nil, nil})
              )
          end
      end
    end)
  end

  def calculate_new_crossing(previous_corner, next_corner) do
    first_start_end =
      [
        Keyword.get(previous_corner, :previous_direction),
        Keyword.get(previous_corner, :next_direction)
      ]

    last_start_end =
      [Keyword.get(next_corner, :previous_direction), Keyword.get(next_corner, :next_direction)]

    if (:U in first_start_end and :U in last_start_end) or
         (:D in first_start_end and :D in last_start_end) do
      1
    else
      0
    end
  end
end
