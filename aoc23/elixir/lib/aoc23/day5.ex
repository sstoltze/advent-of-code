defmodule Aoc23.Day5 do
  # @related [test](test/aoc23/day5_test.exs)
  defmodule Almanac do
    defmodule Parser do
      import NimbleParsec
      import Aoc23.ParserHelpers
      identifier = ascii_string([?a..?z, ?A..?Z], min: 1)
      seeds = ignore(string("seeds:") |> whitespace) |> concat(list_of(number()))

      seed_range =
        ignore(string("seeds:") |> whitespace)
        |> concat(
          list_of(
            number()
            |> unwrap_and_tag(:start)
            |> ignore(whitespace())
            |> concat(number() |> unwrap_and_tag(:length))
            |> reduce({Enum, :into, [%{}]})
          )
        )

      almanac_entry =
        number()
        |> unwrap_and_tag(:destination_start)
        |> ignore(whitespace())
        |> concat(number() |> unwrap_and_tag(:source_start))
        |> ignore(whitespace())
        |> concat(number() |> unwrap_and_tag(:length))
        |> reduce({Enum, :into, [%{}]})

      almanac_map =
        identifier
        |> unwrap_and_tag(:source)
        |> ignore(string("-to-"))
        |> concat(identifier |> unwrap_and_tag(:destination))
        |> ignore(whitespace() |> string("map:\n"))
        |> concat(list_of(almanac_entry, string("\n")) |> tag(:maps))
        |> reduce({Enum, :into, [%{}]})

      almanac =
        seeds
        |> tag(:seeds)
        |> ignore(string("\n\n"))
        |> concat(list_of(almanac_map, string("\n\n")) |> tag(:maps))
        |> eos()

      range_almanac =
        seed_range
        |> tag(:seeds)
        |> ignore(string("\n\n"))
        |> concat(list_of(almanac_map, string("\n\n")) |> tag(:maps))
        |> eos()

      defparsec(:seeds, seeds)
      defparsec(:entry, almanac_entry)
      defparsec(:map, almanac_map)
      defparsec(:almanac, almanac)
      defparsec(:range_almanac, range_almanac)
    end

    def parse(a) do
      {:ok, a, "", _, _, _} = Parser.almanac(a)
      a
    end

    def parse_range(a) do
      {:ok, a, "", _, _, _} = Parser.range_almanac(a)
      a
    end

    defmodule Range do
      def from_almanac(m) do
        %{start: m[:source_start], length: m[:length]}
      end

      defp s(r) do
        r[:start]
      end

      defp e(r) do
        # Not included in the range!
        r[:start] + r[:length]
      end

      def calc(r1, r2) do
        [first, last] = Enum.sort([r1, r2], &(s(&1) <= s(&2)))

        if e(first) <= s(last) do
          %{
            intersection: [],
            difference:
              [
                first,
                last
              ]
              |> Enum.filter(&valid_range/1)
          }
        else
          if e(last) <= e(first) do
            %{
              intersection:
                [
                  %{start: s(last), length: e(last) - s(last)}
                ]
                |> Enum.filter(&valid_range/1),
              difference:
                [
                  %{start: s(first), length: s(last) - s(first)},
                  %{start: e(last), length: e(first) - e(last)}
                ]
                |> Enum.filter(&valid_range/1)
            }
          else
            %{
              intersection:
                [
                  %{start: s(last), length: e(first) - s(last)}
                ]
                |> Enum.filter(&valid_range/1),
              difference:
                [
                  %{start: s(first), length: s(last) - s(first)},
                  %{start: e(first), length: e(last) - e(first)}
                ]
                |> Enum.filter(&valid_range/1)
            }
          end
        end
      end

      def valid_range(r) do
        r[:length] >= 1
      end
    end

    def translate(almanac, {source, entry}) when is_integer(entry) do
      map = almanac[:maps] |> Enum.find(fn m -> m[:source] == source end)

      translation_map =
        map[:maps]
        |> Enum.find(fn m -> entry in m[:source_start]..(m[:source_start] + m[:length]) end)

      case translation_map do
        nil ->
          {map[:destination], entry}

        _ ->
          {map[:destination],
           translation_map[:destination_start] + (entry - translation_map[:source_start])}
      end
    end

    def translate(almanac, {source, entry}) when is_list(entry) do
      source_map = almanac[:maps] |> Enum.find(fn m -> m[:source] == source end)

      ranges =
        entry
        |> Enum.flat_map(fn r ->
          # We translate r with each map, keeping track of what has not been moved yet and only trying the next maps on the un-moved elements
          {a, b} =
            Enum.reduce(source_map[:maps], {[r], []}, fn m, {untranslated, translated} ->
              {untranslated_with_m, translated_with_m} =
                untranslated
                |> Enum.reduce({[], []}, fn range, {intersect, new_ranges} ->
                  {stay, move} = translate_range(range, m)

                  new_intersect =
                    case {intersect, stay} do
                      {[], _} ->
                        stay

                      {_, []} ->
                        intersect

                      {[i], [s]} ->
                        Range.calc(i, s) |> Map.get(:intersection, [])

                      _ ->
                        dbg({intersect, stay})
                        []
                    end

                  {new_intersect, Enum.concat(move, new_ranges)}
                end)

              {untranslated_with_m, Enum.concat(translated_with_m, translated)}
            end)

          a ++ b
        end)

      {source_map[:destination], ranges}
    end

    def translate(almanac, {s, e}) do
      translate(almanac, {s, [e]})
    end

    def translation_keys(almanac) do
      almanac[:maps] |> Enum.map(fn m -> m[:source] end)
    end

    defp translate_range(range, m) do
      %{intersection: i, difference: d} =
        range |> Range.calc(Range.from_almanac(m))

      no_translation =
        d
        |> Enum.flat_map(fn r ->
          r |> Range.calc(range) |> Map.get(:intersection, [])
        end)

      translation =
        i
        |> Enum.map(fn r ->
          r |> Map.put(:start, m[:destination_start] + (r[:start] - m[:source_start]))
        end)

      {no_translation, translation}
    end
  end

  # We handle the newlines in the parser
  def input(), do: Aoc23.input_day(5, 1, nil)

  def day5(input \\ input()) do
    almanac = Almanac.parse(input)
    locations = locations_from_seeds(almanac)

    range_almanac = Almanac.parse_range(input)

    more_locations =
      range_almanac |> locations_from_seeds() |> Enum.concat()

    {Enum.min(locations), more_locations |> Enum.map(fn a -> a[:start] end) |> Enum.min()}
  end

  def locations_from_seeds(almanac) do
    seeds = almanac[:seeds] |> Enum.map(fn s -> {"seed", s} end)

    results =
      Enum.reduce(Almanac.translation_keys(almanac), [seeds], fn _, acc ->
        curr = List.first(acc)
        [Enum.map(curr, &Almanac.translate(almanac, &1)) | acc]
      end)
      |> Enum.map(fn entry ->
        {[type | _], values} = Enum.unzip(entry)

        {type, values}
      end)

    {"location", locations} = List.keyfind(results, "location", 0)
    locations
  end
end
