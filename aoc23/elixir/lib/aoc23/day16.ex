defmodule Aoc23.Day16 do
  def input(), do: Aoc23.input_day(16)

  def day16(input \\ input()) do
    map = input |> Enum.map(&String.to_charlist/1)
    starting_beams = [{{0, 0}, :right}]

    energized = follow_beams(starting_beams, map) |> Enum.count()

    max_energized =
      Enum.concat(
        Enum.flat_map(0..(Enum.count(map) - 1), fn i ->
          [
            follow_beams([{{i, 0}, :right}], map),
            follow_beams([{{i, Enum.count(List.first(map)) - 1}, :left}], map)
          ]
        end),
        Enum.flat_map(0..(Enum.count(List.first(map)) - 1), fn j ->
          [
            follow_beams([{{0, j}, :down}], map),
            follow_beams([{{Enum.count(map) - 1, j}, :up}], map)
          ]
        end)
      )
      |> Enum.map(&Enum.count/1)
      |> Enum.max()

    {energized, max_energized}
  end

  def follow_beams(beams, map) do
    {_, energized, _} =
      1..(Enum.count(map) * Enum.count(List.first(map)))
      |> Enum.reduce_while({beams, [], []}, fn _, {beams, energized, seen_beams} ->
        case Enum.reduce(beams, {[], energized, seen_beams}, fn beam,
                                                                {new_beams, new_energized,
                                                                 new_seen} ->
               {coords, _} = beam
               next_beams = follow_beam(beam, map) |> Enum.reject(&(&1 in new_seen))
               {next_beams ++ new_beams, [coords | new_energized], [beam | new_seen]}
             end) do
          {[], _, _} = no_beams -> {:halt, no_beams}
          _ = beams -> {:cont, beams}
        end
      end)

    Enum.uniq(energized)
  end

  def valid_beam({{i, j}, _}, map) do
    i in 0..(Enum.count(map) - 1) and j in 0..(Enum.count(List.first(map)) - 1)
  end

  def next({{i, j}, direction}) do
    coords =
      case direction do
        :up -> {i - 1, j}
        :down -> {i + 1, j}
        :right -> {i, j + 1}
        :left -> {i, j - 1}
      end

    {coords, direction}
  end

  def get_coordinates({i, j}, map) do
    map |> Enum.at(i) |> Enum.at(j)
  end

  def follow_beam({coords, _} = beam, map) do
    current_position = get_coordinates(coords, map)

    cond do
      current_position == ?. -> [next(beam)]
      current_position in [?|, ?-] -> handle_splitter(beam, current_position)
      current_position in [?/, ?\\] -> handle_mirror(beam, current_position)
    end
    |> Enum.filter(&valid_beam(&1, map))
  end

  def handle_splitter({coords, direction} = beam, ?|) do
    cond do
      direction in [:up, :down] -> [next(beam)]
      true -> [next({coords, :up}), next({coords, :down})]
    end
  end

  def handle_splitter({coords, direction} = beam, ?-) do
    cond do
      direction in [:left, :right] -> [next(beam)]
      true -> [next({coords, :left}), next({coords, :right})]
    end
  end

  def handle_mirror({coords, direction}, ?/) do
    new_direction =
      case direction do
        :up -> :right
        :down -> :left
        :right -> :up
        :left -> :down
      end

    [next({coords, new_direction})]
  end

  def handle_mirror({coords, direction}, ?\\) do
    new_direction =
      case direction do
        :up -> :left
        :down -> :right
        :right -> :down
        :left -> :up
      end

    [next({coords, new_direction})]
  end
end
