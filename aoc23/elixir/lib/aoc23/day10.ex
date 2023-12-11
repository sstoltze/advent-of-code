defmodule Aoc23.Day10 do
  def input(), do: Aoc23.input_day(10)

  def day10(input \\ input()) do
    pipes = input |> Enum.map(&String.to_charlist/1)
    start = pipes |> find_start()
    distances = walk_pipe(pipes, start)

    largest_distance = distances |> Map.values() |> Enum.sort(&>=/2) |> Enum.at(0)
    loop = Map.keys(distances)

    crossing_numbers =
      pipes
      |> Enum.with_index()
      |> Enum.flat_map(fn {r, i} ->
        r
        |> Enum.with_index()
        |> Enum.flat_map(fn {_, j} ->
          if {i, j} in loop do
            []
          else
            [crossing_number(pipes, loop, {i, j})]
          end
        end)
      end)

    {largest_distance, crossing_numbers |> Enum.filter(&(rem(&1, 2) == 1)) |> Enum.count()}
  end

  def find_start(pipes) do
    pipes
    |> Enum.with_index()
    |> Enum.reduce_while(nil, fn {row, row_index}, _ ->
      case Enum.find_index(row, &(&1 == ?S)) do
        nil -> {:cont, nil}
        col_index -> {:halt, {row_index, col_index}}
      end
    end)
  end

  def walk_pipe(pipes, start) when is_tuple(start) do
    walk_pipe(pipes, [start], %{start => 0})
  end

  def walk_pipe(_, [], distances) do
    distances
  end

  def walk_pipe(pipes, points, distances) when is_list(points) do
    next_points =
      points
      |> Enum.flat_map(fn p ->
        d = Map.get(distances, p)

        case get_coordinates(pipes, p) do
          ?S -> find_connecting_points(pipes, p)
          _ -> directions(pipes, p) |> Enum.map(move_from(p))
        end
        |> Enum.filter(fn new -> not Map.has_key?(distances, new) end)
        |> Enum.zip([d + 1, d + 1])
      end)

    new_distances = Map.new(next_points)
    walk_pipe(pipes, Map.keys(new_distances), Map.merge(distances, new_distances))
  end

  def get_coordinates(pipes, {row, col}) do
    pipes |> Enum.at(row) |> Enum.at(col)
  end

  def directions(pipes, p) do
    case get_coordinates(pipes, p) do
      ?| -> [:above, :below]
      ?- -> [:left, :right]
      ?L -> [:above, :right]
      ?J -> [:above, :left]
      ?7 -> [:below, :left]
      ?F -> [:below, :right]
      ?S -> directions_from_surrounding(pipes, p)
      _ -> []
    end
  end

  def move_from(p) do
    fn dir ->
      case dir do
        :above -> above(p)
        :below -> below(p)
        :left -> left(p)
        :right -> right(p)
      end
    end
  end

  def above({row, col}), do: {row - 1, col}
  def below({row, col}), do: {row + 1, col}
  def left({row, col}), do: {row, col - 1}
  def right({row, col}), do: {row, col + 1}

  def opposite(:above), do: :below
  def opposite(:below), do: :above
  def opposite(:left), do: :right
  def opposite(:right), do: :left

  def directions_from_surrounding(pipes, p) do
    [:above, :below, :left, :right]
    |> Enum.filter(fn d -> opposite(d) in directions(pipes, move_from(p).(d)) end)
  end

  def find_connecting_points(pipes, p) do
    directions_from_surrounding(pipes, p)
    |> Enum.map(move_from(p))
  end

  def crossing_number(pipes, loop, p) do
    {row, col} = p
    ray = 0..col |> Enum.map(fn c -> {row, c} end)
    crossings = ray |> Enum.filter(fn p -> p in loop end)

    segments =
      crossings
      |> Enum.reduce([], fn ray_p, acc ->
        case acc do
          [] ->
            [[ray_p]]

          [current_segment | rest] ->
            last_point = List.first(current_segment)

            if last_point == left(ray_p) and :left in directions(pipes, ray_p) do
              [[ray_p | current_segment] | rest]
            else
              [[ray_p] | [current_segment | rest]]
            end
        end
      end)

    Enum.reduce(segments, 0, fn s, acc ->
      if Enum.count(s) == 1 do
        1 + acc
      else
        first = List.first(s)
        last = List.last(s)
        directions_from_segment = directions(pipes, first) ++ directions(pipes, last)

        if :above in directions_from_segment and :below in directions_from_segment do
          1 + acc
        else
          acc
        end
      end
    end)
  end
end
