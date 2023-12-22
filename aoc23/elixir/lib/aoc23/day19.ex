defmodule Aoc23.Day19 do
  def input(), do: Aoc23.input_day(19, 1, nil)

  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers

    category =
      choice([string("x"), string("m"), string("a"), string("s")])
      |> map({String, :to_atom, []})
      |> unwrap_and_tag(:category)

    name = ascii_string([?a..?z], min: 1) |> unwrap_and_tag(:name)
    final = choice([string("A"), string("R")]) |> unwrap_and_tag(:final)
    destination = choice([name, final]) |> unwrap_and_tag(:destination)

    comparison =
      choice([string("<"), string(">")])
      |> map({String, :to_atom, []})
      |> unwrap_and_tag(:comparison)

    value = integer(min: 1) |> unwrap_and_tag(:value)
    check = category |> concat(comparison) |> concat(value)
    rule = check |> ignore(string(":")) |> concat(destination) |> tag(:rule)

    workflow =
      name
      |> ignore(string("{"))
      |> concat(
        list_of(rule, string(","))
        |> ignore(string(","))
        |> concat(destination)
        |> tag(:rules)
      )
      |> ignore(string("}"))
      |> tag(:workflow)

    part =
      ignore(string("{"))
      |> concat(
        list_of(
          category |> ignore(string("=")) |> concat(value),
          string(",")
        )
      )
      |> ignore(string("}"))
      |> tag(:part)

    system =
      list_of(workflow, string("\n"))
      |> tag(:workflows)
      |> ignore(string("\n\n"))
      |> concat(list_of(part, string("\n")) |> tag(:parts))
      |> eos()

    defparsec(:rule, rule)
    defparsec(:workflow, workflow)
    defparsec(:part, part)
    defparsec(:system, system)
  end

  def parse_system(s) do
    {:ok, system, "", _, _, _} = Parser.system(s)

    system
    |> Keyword.update!(
      :workflows,
      &Enum.reduce(&1, %{}, fn {:workflow, workflow}, acc ->
        Map.put(acc, Keyword.get(workflow, :name), Keyword.get(workflow, :rules))
      end)
    )
    |> Keyword.update!(
      :parts,
      fn ps -> Enum.map(ps, &(&1 |> elem(1) |> part_to_keyword_list)) end
    )
  end

  def day19(input \\ input()) do
    [workflows: workflows, parts: parts] = parse_system(input)

    accepted_values =
      parts
      |> Enum.map(&{run_system(&1, workflows), part_value(&1)})
      |> Enum.flat_map(
        &case elem(&1, 0) do
          "A" -> [elem(&1, 1)]
          _ -> []
        end
      )

    requirements = to_accepted(workflows, {:name, "in"})

    possibilities =
      requirements
      |> Enum.map(fn r ->
        r
        |> Keyword.values()
        |> Enum.reduce(1, fn [min: min, max: max], acc -> acc * (max - min + 1) end)
      end)
      |> Enum.sum()

    {Enum.sum(accepted_values), possibilities}
  end

  def part_value(p) do
    Enum.reduce(p, 0, fn {_, v}, acc -> v + acc end)
  end

  def part_to_keyword_list(p) do
    Enum.chunk_every(p, 2) |> Enum.map(&{Keyword.get(&1, :category), Keyword.get(&1, :value)})
  end

  def follow_rule(part, rule) do
    comparison =
      case Keyword.get(rule, :comparison) do
        :< -> &</2
        :> -> &>/2
      end

    if comparison.(Keyword.get(part, Keyword.get(rule, :category)), Keyword.get(rule, :value)) do
      Keyword.get(rule, :destination)
    else
      nil
    end
  end

  def follow_workflow(part, workflow) do
    Enum.reduce_while(workflow, nil, fn rule, _ ->
      case rule do
        {:destination, d} ->
          {:halt, d}

        {:rule, rule} ->
          case follow_rule(part, rule) do
            nil -> {:cont, nil}
            d -> {:halt, d}
          end
      end
    end)
  end

  def all_values do
    [min: 1, max: 4000]
  end

  def all do
    [x: all_values(), m: all_values(), a: all_values(), s: all_values()]
  end

  def run_system(part, workflows, start \\ "in") do
    case follow_workflow(part, Map.get(workflows, start)) do
      {:final, final} -> final
      {:name, n} -> run_system(part, workflows, n)
    end
  end

  def to_accepted(_workflows, {:final, "A"}) do
    [all()]
  end

  def to_accepted(_workflows, {:final, "R"}) do
    []
  end

  def to_accepted(workflows, {:name, name}) do
    workflow = Map.get(workflows, name)

    Enum.reduce(workflow, {all(), []}, fn workflow_step, {current, res} ->
      case workflow_step do
        {:rule, rule} ->
          next = Keyword.get(rule, :destination)
          rule_requirement = requirement_from_rule(rule)
          current_requirement = requirement_intersect(rule_requirement, current)
          from_next = to_accepted(workflows, next)
          from_here = Enum.map(from_next, &requirement_intersect(&1, current_requirement))
          next_current = requirement_intersect(invert_requirement(rule), current)
          {next_current, from_here ++ res}

        {:destination, d} ->
          Enum.map(to_accepted(workflows, d), &requirement_intersect(&1, current)) ++ res
      end
    end)
  end

  def requirement_intersect(r1, r2) do
    Enum.reduce([:x, :m, :a, :s], r1, fn c, new_r ->
      Keyword.update!(new_r, c, fn values ->
        values
        |> Keyword.update!(:min, &max(&1, Keyword.get(Keyword.get(r2, c), :min)))
        |> Keyword.update!(:max, &min(&1, Keyword.get(Keyword.get(r2, c), :max)))
      end)
    end)
  end

  def requirement_from_rule(rule) do
    all()
    |> Keyword.update!(Keyword.get(rule, :category), fn [min: minimum, max: maximum] ->
      case Keyword.get(rule, :comparison) do
        :< -> [min: minimum, max: min(maximum, Keyword.get(rule, :value) - 1)]
        :> -> [min: max(minimum, Keyword.get(rule, :value) + 1), max: maximum]
      end
    end)
  end

  def invert_requirement(rule) do
    all()
    |> Keyword.update!(Keyword.get(rule, :category), fn [min: minimum, max: maximum] ->
      case Keyword.get(rule, :comparison) do
        :< -> [min: max(minimum, Keyword.get(rule, :value)), max: maximum]
        :> -> [min: minimum, max: min(maximum, Keyword.get(rule, :value))]
      end
    end)
  end
end
