defmodule Aoc23.Day18 do
  def input(), do: Aoc23.input_day(18)

  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers

    direction =
      choice([string("U"), string("D"), string("L"), string("R")])
      |> map({String, :to_atom, []})
      |> unwrap_and_tag(:direction)

    meters = integer(min: 1) |> unwrap_and_tag(:meters)

    colour =
      ignore(string("#"))
      |> concat(
        choice([integer(1), ascii_string([?a..?f], 1)])
        |> times(6)
        |> reduce({Enum, :join, [""]})
        |> unwrap_and_tag(:colour)
      )

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
    {rows, cols} = Enum.unzip(loop) |> elem(0) |> Enum.unzip()
    max_row = rows |> Enum.map(&abs/1) |> Enum.max()
    max_col = cols |> Enum.map(&abs/1) |> Enum.max()
    map = Enum.map(-max_row..max_row, fn i -> Enum.map(-max_col..max_col, fn j -> {i, j} end) end)

    crossings =
      calculate_crossings(map, loop)

    map_with_crossings =
      crossings
      |> Enum.map(fn row -> row |> Enum.map(&to_string/1) |> Enum.join("") end)
      |> Enum.join("\n")

    File.write!("/tmp/map", map_with_crossings)

    loop_and_inside =
      Enum.flat_map(
        crossings,
        &Enum.filter(&1, fn p -> is_atom(p) or is_binary(p) or rem(p, 2) == 1 end)
      )

    {Enum.count(loop_and_inside), 0}
  end

  def next({i, j}, direction: dir, meters: m, colour: _) do
    case dir do
      :U -> Enum.map(m..1, fn k -> {i - k, j} end)
      :D -> Enum.map(m..1, fn k -> {i + k, j} end)
      :R -> Enum.map(m..1, fn k -> {i, j + k} end)
      :L -> Enum.map(m..1, fn k -> {i, j - k} end)
    end
  end

  def dig_loop(instructions) do
    l =
      Enum.reduce(
        instructions,
        [{{0, 0}, [previous_instruction: [], current_instruction: []]}],
        fn i, path ->
          [{current_point, point_instructions} | old_path] = path
          next_part = next(current_point, i)

          Enum.map(next_part, &{&1, [previous_instruction: i, current_instruction: i]}) ++
            [
              {current_point,
               [
                 previous_instruction: Keyword.get(point_instructions, :previous_instruction),
                 current_instruction: i
               ]}
              | old_path
            ]
        end
      )

    {path, [{{0, 0}, [previous_instruction: [], current_instruction: i]}]} =
      Enum.split(l, Enum.count(l) - 1)

    path |> List.update_at(0, &{{0, 0}, Keyword.put(elem(&1, 1), :current_instruction, i)})
  end

  def in_loop({i, j}, loop) do
    Enum.find(loop, nil, &(elem(&1, 0) == {i, j}))
  end

  def calculate_crossings(map, loop) do
    Enum.map(0..(Enum.count(map) - 1), fn i ->
      Enum.reduce(0..(Enum.count(Enum.at(map, i)) - 1), {0, [], nil, nil}, fn j,
                                                                              {current_crossing,
                                                                               acc,
                                                                               entering_instructions,
                                                                               last_point_instructions} ->
        case in_loop({i, j}, loop) do
          {_, instructions} ->
            if jumped_loop_path?(last_point_instructions, instructions) do
              new_crossing =
                current_crossing +
                  calculate_new_crossing(entering_instructions, last_point_instructions)

              {new_crossing, ["#" | acc], instructions, instructions}
            else
              {current_crossing, ["#" | acc],
               case entering_instructions do
                 nil -> instructions
                 _ -> entering_instructions
               end, instructions}
            end

          nil ->
            case last_point_instructions do
              nil ->
                {current_crossing, [current_crossing | acc], nil, nil}

              _ ->
                new_crossing =
                  current_crossing +
                    calculate_new_crossing(entering_instructions, last_point_instructions)

                {new_crossing, [new_crossing | acc], nil, nil}
            end
        end
      end)
      |> elem(1)
    end)
  end

  def jumped_loop_path?(nil, _), do: false

  def jumped_loop_path?(prev, current) do
    !(Keyword.get(current, :previous_direction) == Keyword.get(prev, :current_direction) or
        Keyword.get(current, :current_direction) == Keyword.get(prev, :previous_direction))
  end

  def calculate_new_crossing(entering_instructions, leaving_instructions) do
    first_start_end =
      entering_instructions
      |> Keyword.values()
      |> Enum.map(&Keyword.get(&1, :direction))

    last_start_end =
      leaving_instructions
      |> Keyword.values()
      |> Enum.map(&Keyword.get(&1, :direction))

    if (:U in first_start_end and :U in last_start_end) or
         (:D in first_start_end and :D in last_start_end) do
      1
    else
      0
    end
  end
end
