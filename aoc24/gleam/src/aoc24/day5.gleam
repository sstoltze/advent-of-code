import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/string

type Restrictions =
  Dict(Int, List(Int))

type Update =
  List(Int)

pub fn run(input: List(String)) -> #(Int, Int) {
  let #(restrictions, updates) = parse_input(input)
  let grouped =
    list.group(updates, fn(update) { is_valid_update(update, restrictions) })

  let assert Ok(valid_updates) = dict.get(grouped, True)
  let assert Ok(invalid_updates) = dict.get(grouped, False)

  #(
    valid_updates
      |> list.map(middle_element)
      |> list.fold(0, int.add),
    invalid_updates
      |> list.map(middle_element)
      |> list.fold(0, int.add),
  )
}

pub fn is_valid_update(update: Update, restrictions: Restrictions) -> Bool {
  update
  |> list.fold_until(#(True, []), fn(acc, page) {
    let #(_, seen) = acc

    case
      list.any(seen, fn(before_this) {
        case dict.get(restrictions, before_this) {
          Error(_) -> False
          Ok(before_this_must_be_after_these) ->
            list.contains(before_this_must_be_after_these, page)
        }
      })
    {
      False -> list.Continue(#(True, [page, ..seen]))
      True -> list.Stop(#(False, []))
    }
  })
  |> pair.first()
}

pub fn middle_element(update: Update) -> Int {
  let length = list.length(update)
  let assert Ok(half) = int.divide(length - 1, 2)
  let assert [middle, ..] = list.drop(update, half)
  middle
}

pub fn parse_input(input: List(String)) -> #(Restrictions, List(Update)) {
  let assert #(restrictions, ["", ..updates]) =
    list.split_while(input, fn(line) { line != "" })
  #(parse_restrictions(restrictions), list.map(updates, parse_update))
}

fn parse_update(update: String) -> List(Int) {
  update
  |> string.split(",")
  |> list.map(int.parse)
  |> result.values()
}

fn parse_restrictions(restrictions: List(String)) -> Restrictions {
  list.fold(restrictions, dict.new(), fn(acc, restriction) {
    let assert [before, after] =
      string.split(restriction, "|") |> list.map(int.parse) |> result.values()
    dict.upsert(acc, after, fn(existing) {
      case existing {
        Some(value) -> [before, ..value]
        None -> [before]
      }
    })
  })
}
