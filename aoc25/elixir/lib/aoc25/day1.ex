defmodule Aoc25.Day1 do
  def parse(string) do
    String.split(string)
  end

  def part_1(instructions) do
    instructions |> parse() |> spin() |> elem(1)
  end

  def part_2(instructions) do
    instructions |> parse() |> spin() |> elem(2)
  end

  def spin(instructions) do
    Enum.reduce(instructions, {50, 0, 0}, fn
      <<direction::utf8, number::binary>>, {current_step, shown_zero, passed_zero} ->
        {rotation, ""} = Integer.parse(number)

        {new_step, range} =
          case direction do
            ?L ->
              {Integer.mod(current_step - rotation, 100), (current_step - rotation)..current_step}

            ?R ->
              {Integer.mod(current_step + rotation, 100), current_step..(current_step + rotation)}
          end

        update_shown_zero =
          case new_step do
            0 -> 1
            _ -> 0
          end

        new_passed_zero =
          Enum.count(range, fn n -> Integer.mod(n, 100) == 0 end) + passed_zero -
            update_shown_zero

        {new_step, shown_zero + update_shown_zero, new_passed_zero}
    end)
  end
end
