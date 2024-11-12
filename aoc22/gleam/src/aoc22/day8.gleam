import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string

pub type TreeInfo {
  TreeInfo(visible: Bool, height: Int)
}

pub type Map =
  Dict(#(Int, Int), TreeInfo)

pub fn count_visible(trees: Map) -> Int {
  trees
  |> dict.values()
  |> list.filter(fn(tree) { tree.visible })
  |> list.length()
}

pub fn parse_map(input: List(String)) -> Map {
  list.index_fold(input, dict.new(), fn(map, line, i) {
    let line_map =
      line
      |> string.to_graphemes()
      |> list.index_fold(dict.new(), fn(acc, height, j) {
        let assert Ok(height) = int.parse(height)
        dict.insert(acc, #(i, j), TreeInfo(height: height, visible: False))
      })

    map
    |> dict.merge(line_map)
  })
}

pub fn calculate_visible(trees: Map) -> Map {
  todo
}

pub fn main(input: List(String)) -> #(Int, Int) {
  let map = input |> parse_map() |> calculate_visible()
  #(count_visible(map), 0)
}
