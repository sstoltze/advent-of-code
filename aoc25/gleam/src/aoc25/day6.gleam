import gleam/int
import gleam/list
import gleam/string

pub fn part_1(input: String) -> Int {
  input |> parse |> list.map(solve) |> list.fold(0, int.add)
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

fn solve(problem: Problem) -> Int {
  let #(operator, start) = case problem.operator {
    Add -> #(int.add, 0)
    Mult -> #(int.multiply, 1)
  }
  list.fold(problem.numbers, start, operator)
}
