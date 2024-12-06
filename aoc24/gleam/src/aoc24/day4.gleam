import gleam/dict
import gleam/int
import gleam/list
import gleam/pair
import gleam/string

type Array =
  dict.Dict(#(Int, Int), String)

type Letter =
  #(Int, Int, String)

type Word =
  List(Letter)

fn parse_array(rows: List(String)) -> Array {
  list.index_fold(rows, dict.new(), fn(acc, row, first_index) {
    row
    |> string.split("")
    |> list.index_map(fn(letter, second_index) {
      #(#(first_index, second_index), letter)
    })
    |> dict.from_list()
    |> dict.merge(acc)
  })
}

fn find(array: Array, word: String) -> List(Word) {
  find_results(array, string.split(word, ""), [])
}

fn find_results(
  array: Array,
  to_be_found: List(String),
  results: List(Word),
) -> List(Word) {
  case to_be_found {
    [] -> results |> list.map(list.reverse)
    [letter, ..next] -> {
      let found_letters = find_in_array(array, letter)

      let new_results = case results {
        [] -> list.map(found_letters, list.wrap)
        _ -> {
          list.flat_map(results, fn(word) {
            let assert [#(x, y, _char), ..] = word
            found_letters
            |> list.filter(fn(letter) {
              let #(a, b, _char) = letter
              int.absolute_value(x - a) <= 1 && int.absolute_value(y - b) <= 1
            })
            |> list.map(fn(letter) { [letter, ..word] })
          })
        }
      }
      find_results(array, next, new_results)
    }
  }
}

fn find_in_array(array: Array, letter: String) -> List(Letter) {
  dict.fold(array, [], fn(acc, key, value) {
    case letter == value {
      True -> {
        let #(x, y) = key
        [#(x, y, letter), ..acc]
      }
      False -> acc
    }
  })
}

fn is_straight_line(word: Word) -> Bool {
  case word {
    [#(x1, y1, _), #(x2, y2, _), ..rest] ->
      list.fold_until(rest, #(True, #(x2, y2)), fn(acc, letter) {
        let #(_, #(x, y)) = acc
        let #(a, b, _) = letter
        case { a - x == x2 - x1 } && { b - y == y2 - y1 } {
          True -> list.Continue(#(True, #(a, b)))
          False -> list.Stop(#(False, #(a, b)))
        }
      })
      |> pair.first()
    _ -> True
  }
}

fn is_diagonal(word: Word) -> Bool {
  let assert [#(mx, my, "M"), _, #(sx, sy, "S")] = word
  int.absolute_value(sx - mx) == 2 && int.absolute_value(sy - my) == 2
}

fn gather_xes(mases: List(Word)) -> List(Word) {
  list.fold(mases, #([], []), fn(acc, mas) {
    let #(result, seen_a_coordinates) = acc
    let assert [_, #(x, y, "A"), _] = mas
    case list.find(seen_a_coordinates, fn(a) { a == #(x, y) }) {
      Ok(_) -> #([mas, ..result], seen_a_coordinates)
      Error(_) -> #(result, [#(x, y), ..seen_a_coordinates])
    }
  })
  |> pair.first()
}

pub fn run(input: List(String)) -> #(Int, Int) {
  let array = parse_array(input)
  let xmases = find(array, "XMAS") |> list.filter(is_straight_line)
  let x_mases =
    find(array, "MAS")
    |> list.filter(is_straight_line)
    |> list.filter(is_diagonal)
    |> gather_xes

  #(list.length(xmases), list.length(x_mases))
}
