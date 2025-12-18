import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn part_1(input: String) -> Int {
  input |> parse |> list.map(solve) |> list.fold(0, int.add)
}

pub fn part_2(input: String) -> Int {
  input |> parse_v2 |> list.map(solve) |> list.fold(0, int.add)
}

type Operator {
  Add
  Mult
}

type Problem {
  Problem(operator: Operator, numbers: List(Int))
}

fn parse(input: String) -> List(Problem) {
  string.split(input, "\n")
  |> list.map(fn(line) {
    string.split(line, " ") |> list.filter(fn(s) { s != "" })
  })
  |> list.transpose()
  |> list.map(fn(column) {
    let assert [operator, ..numbers] = list.reverse(column)
    Problem(
      case operator {
        "+" -> Add
        "*" -> Mult
        _ -> panic
      },
      list.filter_map(numbers, int.parse),
    )
  })
}

fn parse_v2(input: String) -> List(Problem) {
  string.split(input, "\n")
  |> list.map(fn(line) { string.split(line, "") })
  |> list.transpose()
  |> list.fold([], fn(acc, column) {
    let assert [operator, ..numbers] = list.reverse(column)
    case list.all([operator, ..numbers], fn(s) { s == " " }), operator {
      True, _ -> {
        acc
      }
      _, "+" -> [Problem(Add, [parse_number(numbers)]), ..acc]
      _, "*" -> [Problem(Mult, [parse_number(numbers)]), ..acc]

      _, _ -> {
        let assert [current_problem, ..rest] = acc
        [
          Problem(..current_problem, numbers: [
            parse_number([operator, ..numbers]),
            ..current_problem.numbers
          ]),
          ..rest
        ]
      }
    }
  })
}

fn parse_number(numbers: List(String)) -> Int {
  list.reverse(numbers)
  |> string.join("")
  |> string.trim()
  |> int.parse()
  |> result.unwrap(0)
}

fn solve(problem: Problem) -> Int {
  let #(operator, start) = case problem.operator {
    Add -> #(int.add, 0)
    Mult -> #(int.multiply, 1)
  }
  list.fold(problem.numbers, start, operator)
}
