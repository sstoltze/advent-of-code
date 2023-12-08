defmodule Aoc23.Day8 do
  def input(), do: Aoc23.input_day(8, 1, "\n\n")

  def day8(input \\ input()) do
    [instructions, n] = input
    nodes = parse_nodes(n)

    a_nodes = nodes |> Map.keys() |> Enum.filter(&(String.last(&1) == "A"))

    walk_nodes_length =
      if "AAA" in a_nodes do
        walk(instructions, nodes, "AAA", &(&1 == "ZZZ"))
      else
        []
      end

    walk_simultaneous_length =
      walk(instructions, nodes, a_nodes, &(String.last(&1) == "Z"))

    {walk_nodes_length, walk_simultaneous_length}
  end

  def parse_nodes(n) do
    n
    |> String.split("\n")
    |> Enum.map(fn l -> Regex.scan(~r/\w+/, l) end)
    |> Enum.reduce(%{}, fn [[node], [left], [right]], acc ->
      Map.put(acc, node, %{left: left, right: right})
    end)
  end

  def walk_node(instructions, nodes, start, dest_fn) do
    {last, trip} =
      instructions
      |> String.to_charlist()
      |> Enum.reduce_while({start, 1}, fn i, acc ->
        {current, visited} = acc

        next =
          nodes
          |> Map.get(current)
          |> Map.get(
            if i == ?L do
              :left
            else
              :right
            end
          )

        if dest_fn.(next) do
          {:halt, {next, visited}}
        else
          {:cont, {next, visited + 1}}
        end
      end)

    if dest_fn.(last) do
      trip
    else
      trip + walk_node(instructions, nodes, last, dest_fn) - 1
    end
  end

  def walk(instructions, nodes, start, dest_fn) when is_bitstring(start) do
    walk_node(instructions, nodes, start, dest_fn)
  end

  def walk(instructions, nodes, start, dest_fn) do
    walks = Enum.map(start, &walk_node(instructions, nodes, &1, dest_fn))

    Enum.reduce(walks, 1, &Aoc23.Maths.lcm/2)
  end
end
