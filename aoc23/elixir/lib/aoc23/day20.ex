defmodule Aoc23.Day20 do
  def input(), do: Aoc23.input_day(20, 1, nil)

  defmodule Parser do
    import NimbleParsec
    import Aoc23.ParserHelpers

    name = utf8_string([?a..?z], min: 1) |> map({String, :to_atom, []}) |> unwrap_and_tag(:name)

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
    {Keyword.get(line, :name, :broadcaster), line}
  end

  def parse_config(input) do
    [
      modules: input |> Enum.map(&parse_config_line/1),
      pulses: [],
      done_pulses: []
    ]
  end

  def day20(input \\ input()) do
    system = parse_config(input)

    final_state =
      run_system(
        system
        |> Keyword.put(:pulses, [{:broadcaster, :low, nil}] |> Stream.cycle() |> Enum.take(1000))
      )

    score = calculate_score(Keyword.get(final_state, :done_pulses))

    {score, 0}
  end

  def calculate_score(pulses) do
    {p1, p2} = Enum.split_with(pulses, &(elem(&1, 1) == :high))
    Enum.count(p1) * Enum.count(p2)
  end

  def run_system(system) do
    case Keyword.get(system, :pulses) do
      [] -> system
      _ -> run_system(update_system(system))
    end
  end

  def update_system(system) do
    [pulse | remaining_pulses] = Keyword.get(system, :pulses)
    {target, pulse_type, from} = pulse
    modules = Keyword.get(system, :modules)

    module = Keyword.get(modules, target)

    {updated_module, new_pulses} =
      handle_pulse(Keyword.get(module, :type), pulse_type, module,
        from: from,
        inputs: system_inputs(system)
      )

    new_modules = modules |> Keyword.put(target, updated_module)

    system
    |> Keyword.put(:modules, new_modules)
    |> Keyword.put(:pulses, remaining_pulses ++ new_pulses)
    |> Keyword.update!(:done_pulses, &[pulse | &1])
  end

  def handle_pulse(:broadcaster, pulse, module, _opts) do
    {module, target_pulses(module, pulse)}
  end

  def handle_pulse(:flip_flop, :high, module, _opts) do
    {module, []}
  end

  def handle_pulse(:flip_flop, :low, module, _opts) do
    {new_state, new_pulse} =
      case Keyword.get(module, :state, :off) do
        :off -> {:on, :high}
        :on -> {:off, :low}
      end

    {module |> Keyword.put(:state, new_state), target_pulses(module, new_pulse)}
  end

  def handle_pulse(:conjunction, pulse, module, opts) do
    from = Keyword.get(opts, :from)
    inputs = Keyword.get(opts, :inputs)

    new_module =
      Keyword.update(module, :seen, Keyword.new([{from, pulse}]), &Keyword.put(&1, from, pulse))

    new_pulse =
      if all_high?(inputs, Keyword.get(new_module, :seen)) do
        :low
      else
        :high
      end

    {new_module, target_pulses(new_module, new_pulse)}
  end

  def all_high?(inputs, seen) do
    Enum.all?(inputs, &(Keyword.get(seen, &1, :low) == :high))
  end

  def target_pulses(module, pulse) do
    module
    |> Keyword.get(:targets)
    |> Keyword.values()
    |> Enum.map(&{&1, pulse, Keyword.get(module, :name)})
  end

  def system_inputs(system) do
    system |> Keyword.get(:modules) |> Keyword.keys()
  end
end
