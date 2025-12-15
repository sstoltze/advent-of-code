import gleam/dict.{type Dict}
import gleam/list
import gleam/string

pub fn part_1(input: String) -> Int {
  input
  |> parse
  |> ignore_empty()
  |> count_surrounding()
  |> accessible()
  |> dict.size()
}

pub fn part_2(input: String) -> Int {
  input |> parse() |> remove_all(0)
}

fn remove_all(map: Map, removed: Int) -> Int {
  let #(updated_map, removed_rolls) =
    map
    |> ignore_empty()
    |> count_surrounding()
    |> accessible()
    |> dict.keys()
    |> remove_rolls(from: map)
  case map == updated_map {
    True -> removed
    False -> remove_all(updated_map, removed + removed_rolls)
  }
}

fn remove_rolls(to_remove: List(Position), from from: Map) -> #(Map, Int) {
  let updated_map =
    list.fold(to_remove, from, fn(acc, pos) {
      dict.upsert(acc, pos, fn(_) { "." })
    })
  #(updated_map, list.length(to_remove))
}

type Map =
  Dict(Position, String)

type Position =
  #(Int, Int)

fn parse(input: String) -> Map {
  string.split(input, "\n")
  |> list.index_fold(dict.new(), fn(acc, row, i) {
    list.index_fold(string.split(row, ""), acc, fn(better_acc, value, j) {
      dict.insert(better_acc, #(i, j), value)
    })
  })
}

fn ignore_empty(map: Map) -> Map {
  dict.filter(map, fn(_key, value) { value == "@" })
}

fn count_surrounding(map: Map) -> Dict(Position, Int) {
  map
  |> dict.keys()
  |> list.map(fn(key) { #(key, surrounding(map, key)) })
  |> list.map(fn(elem) {
    let #(key, surrounding) = elem
    #(key, surrounding |> list.filter(fn(s) { s == "@" }) |> list.length())
  })
  |> dict.from_list()
}

fn surrounding(map: Map, pos: Position) -> List(String) {
  let #(x, y) = pos
  [
    #(x - 1, y - 1),
    #(x - 1, y),
    #(x - 1, y + 1),
    #(x, y - 1),
    #(x, y + 1),
    #(x + 1, y - 1),
    #(x + 1, y),
    #(x + 1, y + 1),
  ]
  |> list.filter_map(dict.get(map, _))
}

fn accessible(map: Dict(Position, Int)) -> Dict(Position, Int) {
  map |> dict.filter(fn(_key, value) { value < 4 })
}
