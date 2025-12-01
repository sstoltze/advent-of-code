defmodule Aoc25.Day1 do
  def parse(string) do
    String.split(string)
  end

  def part_1(instructions) do
    Enum.reduce(instructions, {50, 0}, fn
      <<direction::utf8, number::binary>>, {current_step, passed_zero} ->
        {rotation, ""} = Integer.parse(number)

        new_step =
          case direction do
            ?L -> Integer.mod(current_step - rotation, 100)
            ?R -> Integer.mod(current_step + rotation, 100)
          end

        new_passed_zero =
          case new_step do
            0 -> 1 + passed_zero
            _ -> passed_zero
          end

        {new_step, new_passed_zero}
    end)
    |> elem(1)
  end
end
