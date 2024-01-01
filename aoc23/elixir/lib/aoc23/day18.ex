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

    # colour_loop = instructions |> Enum.map(&Keyword.get(&1, :colour)) |> dig_loop()
    # colour_loop_area = calculate_loop_area(colour_loop)

    {
      loop_area,
      0
      # colour_loop_area
    }
  end

  def calculate_loop_area(loop) do
    map = build_map(loop)

    crossings =
      calculate_crossings(map, loop)

    Enum.sum(crossings)
  end

  def build_map(loop) do
    start = List.first(loop)
    row = Keyword.get(start, :row)
    col = Keyword.get(start, :col)

    {row_min, row_max, col_min, col_max} =
      Enum.reduce(loop, {Enum.min(row), Enum.max(row), Enum.min(col), Enum.max(col)}, fn
        part, {row_min, row_max, col_min, col_max} ->
          part_row = Keyword.get(part, :row)
          part_col = Keyword.get(part, :col)

          {min(row_min, Enum.min(part_row)), max(row_max, Enum.max(part_row)),
           min(col_min, Enum.min(part_col)), max(col_max, Enum.max(part_col))}
      end)

    Enum.map(row_min..row_max, fn i -> Enum.map(col_min..col_max, fn j -> {i, j} end) end)
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

  def calculate_crossings(map, loop) do
    Enum.map(map, fn row ->
      row_index = row |> Enum.at(0) |> elem(0)

      loop_parts_in_row =
        loop
        |> Enum.filter(fn p -> row_index in Keyword.get(p, :row) end)
        |> Enum.sort_by(&Enum.min(Keyword.get(&1, :col)))

      Enum.reduce(loop_parts_in_row, {0, 0, nil, nil}, fn part,
                                                          {current_crossing, loop_or_inside_count,
                                                           previous_corner, last_loop_part} ->
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
      end)
      |> elem(1)
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
