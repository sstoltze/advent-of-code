import gleam/dict
import gleam/function
import gleam/int
import gleam/list
import gleam/string

pub fn similarity_score(first_list: List(Int), second_list: List(Int)) -> Int {
  let grouped_first = list.group(first_list, function.identity)
  let grouped_second = list.group(second_list, function.identity)
  dict.fold(grouped_first, 0, fn(score, key, value) {
    let appears_in_second_list = case dict.get(grouped_second, key) {
      Ok(value) -> list.length(value)
      Error(Nil) -> 0
    }
    score + list.length(value) * key * appears_in_second_list
  })
}

pub fn total_distance(first_list: List(Int), second_list: List(Int)) -> Int {
  list.map2(
    list.sort(first_list, by: int.compare),
    list.sort(second_list, by: int.compare),
    fn(a, b) { int.absolute_value(a - b) },
  )
  |> list.fold(0, int.add)
}

pub fn run(input: List(String)) -> #(Int, Int) {
  let #(first_list, second_list) =
    input
    |> list.fold(#([], []), fn(acc, row) {
      let #(first_list, second_list) = acc
      case string.split(row, " ") {
        [first_string, "", "", second_string] -> {
          let assert Ok(first) = int.parse(first_string)
          let assert Ok(second) = int.parse(second_string)
          #([first, ..first_list], [second, ..second_list])
        }
        _ -> acc
      }
    })
  let total_distance = total_distance(first_list, second_list)
  let similarity_score = similarity_score(first_list, second_list)

  #(total_distance, similarity_score)
}
