import gleam/int
import gleam/list
import gleam/string

pub fn part_1(input: String) -> Int {
  let #(ranges, ingredients) = input |> parse

  list.filter(ingredients, fresh(_, ranges)) |> list.length
}

pub fn part_2(input: String) -> Int {
  let #(ranges, _ingredients) = input |> parse

  list.fold(ranges, 0, fn(acc, range) { acc + range.high - range.low + 1 })
}

fn parse(input: String) -> #(List(FreshRange), List(Ingredient)) {
  let assert [ranges, ingredients] = string.split(input, "\n\n")
  #(
    ranges
      |> string.split("\n")
      |> list.map(fn(r) {
        let assert [low, high] =
          string.split(r, "-") |> list.filter_map(int.parse)
        FreshRange(low, high)
      })
      |> list.sort(by: fn(r1, r2) { int.compare(r1.low, r2.low) })
      |> list.fold([], fn(acc: List(FreshRange), range: FreshRange) {
        case acc {
          [] -> {
            [range]
          }
          [prev_range, ..rest] -> {
            case
              contained_in(prev_range.high, range),
              contained_in(range.low, prev_range)
            {
              False, False -> {
                [range, prev_range, ..rest]
              }
              _, _ -> {
                [
                  FreshRange(
                    int.min(range.low, prev_range.low),
                    int.max(range.high, prev_range.high),
                  ),
                  ..rest
                ]
              }
            }
          }
        }
      }),
    ingredients |> string.split("\n") |> list.filter_map(int.parse),
  )
}

fn fresh(ingredient: Ingredient, ranges: List(FreshRange)) {
  list.any(ranges, contained_in(ingredient, _))
}

fn contained_in(ingredient: Ingredient, range: FreshRange) {
  range.low <= ingredient && ingredient <= range.high
}

type Ingredient =
  Int

type FreshRange {
  FreshRange(low: Ingredient, high: Ingredient)
}
