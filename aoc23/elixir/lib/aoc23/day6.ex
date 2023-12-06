defmodule Aoc23.Day6 do
  def input(), do: Aoc23.input_day(6)

  def day6(input \\ input()) do
    [time, distance] = input
    times = Regex.scan(~r/\d+/, time) |> Enum.concat() |> Enum.map(&String.to_integer/1)
    distances = Regex.scan(~r/\d+/, distance) |> Enum.concat() |> Enum.map(&String.to_integer/1)
    races = Enum.zip(times, distances)
    ways_to_win = races |> Enum.map(&wins/1)
    time_with_kerning = times |> Enum.join("") |> String.to_integer()
    distance_with_kerning = distances |> Enum.join("") |> String.to_integer()
    {Enum.product(ways_to_win), wins({time_with_kerning, distance_with_kerning})}
  end

  def wins(race) do
    # i * time - i ^ 2 = i * (time - i) >= distance
    # -i^2 + i * time - distance >= 0
    # D = time^2 - 4 distance (pos)
    # x = (-time +- sqrt(D))/(-2)
    {time, distance} = race
    disc = time * time - 4 * distance
    i = ceil(-((-time + :math.sqrt(disc)) / 2))
    2 * (floor(time / 2) - i + 1) - rem(rem(time, 2) + 1, 2)
  end

  def distance_for_ms(ms) do
    half_distance = 0..floor(ms / 2) |> Enum.map(&(&1 * (ms - &1)))

    other_half =
      if rem(ms, 2) == 0 do
        half_distance |> Enum.reverse() |> Enum.drop(1)
      else
        half_distance |> Enum.reverse()
      end

    half_distance ++ other_half
  end
end
