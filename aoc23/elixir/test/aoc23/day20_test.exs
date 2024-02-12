defmodule Aoc23.Day20Test do
  use ExUnit.Case

  test "day 18 example" do
    first_input =
      """
      broadcaster -> a, b, c
      %a -> b
      %b -> c
      %c -> inv
      &inv -> a
      """
      |> String.trim()
      |> String.split("\n")

    assert {32_000_000, _} = Aoc23.Day20.day20(first_input)

    second_input =
      """
      broadcaster -> a
      %a -> inv, con
      &inv -> b
      %b -> con
      &con -> output
      """
      |> String.trim()
      |> String.split("\n")

    assert {11_687_500, _} = Aoc23.Day20.day20(second_input)
  end
end
