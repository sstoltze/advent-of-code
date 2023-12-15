defmodule Aoc23.Day15 do
  # @related [tests](test/aoc23/day15_test.exs)
  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers

    label =
      ascii_string([?a..?z], min: 1)
      |> unwrap_and_tag(:label)

    focal_length = number() |> unwrap_and_tag(:focal_length)

    operation =
      choice([
        string("-") |> replace(:remove),
        string("=") |> replace(:put)
      ])
      |> unwrap_and_tag(:operation)
      |> concat(optional(focal_length))

    step = label |> concat(operation) |> choice([string(","), eos()])

    defparsec(:step, step)
  end

  def input(), do: Aoc23.input_day(15)

  def day15(input \\ input()) do
    steps = input |> Enum.flat_map(&String.split(&1, ","))
    sum_of_hash = steps |> Enum.map(&hash_step/1) |> Enum.sum()
    parsed_steps = Enum.map(steps, &parse_step/1)
    boxes = run_steps(parsed_steps)
    power = total_focusing_power(boxes)
    {sum_of_hash, power}
  end

  def hash_step(step) do
    step |> String.to_charlist() |> Enum.reduce(0, fn c, acc -> ((acc + c) * 17) |> rem(256) end)
  end

  def parse_step(s) do
    {:ok, step, "", _, _, _} = Parser.step(s)
    step
  end

  def total_focusing_power(boxes) do
    boxes
    |> Map.to_list()
    |> Enum.reduce(0, fn {box_number, lenses}, total_power ->
      total_power +
        (lenses
         |> Enum.with_index()
         |> Enum.reduce(0, fn {{_, focal_length}, position}, box_power ->
           box_power + (1 + box_number) * (1 + position) * focal_length
         end))
    end)
  end

  def run_steps(steps) do
    Enum.reduce(steps, %{}, fn step, boxes ->
      label = Keyword.get(step, :label)
      hash = hash_step(label)

      run_operation = fn box ->
        kw_label = String.to_atom(label)

        case Keyword.get(step, :operation) do
          :put ->
            focal_length = Keyword.get(step, :focal_length)
            box |> Keyword.update(kw_label, focal_length, fn _ -> focal_length end)

          :remove ->
            box |> Keyword.delete(kw_label)
        end
      end

      Map.update(boxes, hash, run_operation.([]), run_operation)
    end)
  end
end
