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
    loop_area = calculate_loop_area(loop) |> floor()

    colour_loop = instructions |> Enum.map(&Keyword.get(&1, :colour)) |> dig_loop()
    colour_loop_area = calculate_loop_area(colour_loop) |> floor()

    {
      loop_area,
      colour_loop_area
    }
  end

  def calculate_loop_area(corners) do
    double_corners = corners |> Enum.zip(Enum.drop(corners ++ corners, 1))

    {trapezoid_area, perimeter} =
      Enum.reduce(double_corners, {0, 0}, fn {{x, y}, {next_x, next_y}}, {area, perimeter} ->
        {area + (y + next_y) * (x - next_x), perimeter + abs(next_y - y) + abs(next_x - x)}
      end)

    abs(trapezoid_area / 2) + perimeter / 2 + 1
  end

  def next_corner({i, j}, instruction) do
    dir = Keyword.get(instruction, :direction)
    m = Keyword.get(instruction, :meters)

    {ending_row, ending_column} =
      case dir do
        :U -> {i - m, j}
        :D -> {i + m, j}
        :R -> {i, j + m}
        :L -> {i, j - m}
      end

    {ending_row, ending_column}
  end

  def dig_loop(instructions) do
    instructions
    |> Enum.reduce(
      [{0, 0}],
      fn i, path ->
        [old_corner | _] = path

        next_corner =
          next_corner(old_corner, i)

        [next_corner | path]
      end
    )
    |> Enum.drop(1)
  end
end
