import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/regexp
import gleam/string

type Instruction {
  Mul(Int, Int)
  Do
  Dont
}

fn find_instructions(memory: String) -> List(Instruction) {
  let assert Ok(regex) =
    regexp.from_string(
      "(?:mul\\((\\d{1,3}),(\\d{1,3})\\))|(?:do\\(\\))|(?:don't\\(\\))",
    )
  regexp.scan(with: regex, content: memory)
  |> list.fold([], fn(instructions, match) {
    case match {
      regexp.Match(_match, [option.Some(x), option.Some(y)]) -> {
        let assert Ok(a) = int.parse(x)
        let assert Ok(b) = int.parse(y)
        [Mul(a, b), ..instructions]
      }
      regexp.Match("don't()", _submatches) -> [Dont, ..instructions]

      regexp.Match("do()", _submatches) -> [Do, ..instructions]
      _ -> instructions
    }
  })
  |> list.reverse()
}

fn run_instruction_with_conditionals(
  enable: Bool,
) -> fn(#(Int, Bool), Instruction) -> #(Int, Bool) {
  fn(acc, instruction) {
    case enable, acc, instruction {
      False, #(sum, _), Mul(x, y) -> #(sum + x * y, True)
      _, #(sum, True), Mul(x, y) -> #(sum + x * y, True)
      _, #(sum, _), Do -> #(sum, True)
      True, #(sum, _), Dont -> #(sum, False)
      _, acc, _ -> acc
    }
  }
}

fn run_instructions(
  instructions: List(Instruction),
  enable_conditionals: Bool,
) -> Int {
  list.fold(
    instructions,
    #(0, True),
    run_instruction_with_conditionals(enable_conditionals),
  )
  |> pair.first()
}

pub fn run(input: List(String)) -> #(Int, Int) {
  let corrupted_memory = string.join(input, "")
  let sum_without =
    find_instructions(corrupted_memory)
    |> run_instructions(False)
  let sum_with =
    find_instructions(corrupted_memory)
    |> run_instructions(True)
  #(sum_without, sum_with)
}
