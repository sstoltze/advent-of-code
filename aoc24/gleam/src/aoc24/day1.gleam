import gleam/int
import gleam/list
import gleam/string

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
  let total_distance =
    list.map2(
      list.sort(first_list, by: int.compare),
      list.sort(second_list, by: int.compare),
      fn(a, b) { int.absolute_value(a - b) },
    )
    |> list.fold(0, int.add)

  #(total_distance, 0)
}
