defmodule Aoc23.Day20 do
  def input(), do: Aoc23.input_day(20, 1, nil)

  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers

    name = utf8_string([?a..?z], min: 1) |> unwrap_and_tag(:name)

    flip_flop = string("%") |> replace(:flip_flop) |> unwrap_and_tag(:type) |> concat(name)
    conjunction = string("&") |> replace(:conjunction) |> unwrap_and_tag(:type) |> concat(name)

    broadcaster =
      string("broadcaster")
      |> replace(:broadcaster)
      |> unwrap_and_tag(:type)

    type = choice([flip_flop, conjunction, broadcaster])

    config_line =
      type
      |> ignore(string(" -> "))
      |> concat(list_of(name, string(", ")) |> tag(:targets))
      |> eos

    defparsec(:config_line, config_line)
  end

  def parse_config_line(l) do
    {:ok, line, "", _, _, _} = Parser.config_line(l)
    line
  end

  def parse_config(input) do
    Enum.map(input, &parse_config_line/1)
  end

  def day20(input \\ input()) do
    config = parse_config(input)

    {0, 0}
  end
end
