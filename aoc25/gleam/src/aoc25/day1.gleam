import gleam/int
import gleam/list
import gleam/result
import gleam/string

type Direction {
  Left
  Right
}

type Instruction {
  Instruction(direction: Direction, amount: Int)
}

type Dial {
  Dial(position: Int)
}

pub fn part_1(input: String) -> Int {
  let #(_, result, _) = input |> parse() |> run(Dial(50))
  result
}

pub fn part_2(input: String) -> Int {
  let #(_, _, result) = input |> parse() |> run(Dial(50))
  result
}

fn run(instructions: List(Instruction), dial: Dial) -> #(Dial, Int, Int) {
  run_internal(instructions, dial, 0, 0)
}

fn run_internal(
  instructions: List(Instruction),
  dial: Dial,
  equal_zero: Int,
  passed_zero: Int,
) {
  case instructions {
    [] -> {
      #(dial, equal_zero, passed_zero)
    }
    [i, ..rest] -> {
      let #(new_dial, passed) = turn(dial, i)
      let new_passed = passed_zero + passed
      let equal = case new_dial.position {
        0 -> 1
        _ -> 0
      }
      run_internal(rest, new_dial, equal_zero + equal, new_passed - equal)
    }
  }
}

fn turn(dial: Dial, instruction: Instruction) -> #(Dial, Int) {
  let new_position = case instruction.direction {
    Left -> {
      dial.position - instruction.amount
    }
    Right -> {
      dial.position + instruction.amount
    }
  }

  let times_past_zero =
    list.range(
      int.min(dial.position, new_position),
      int.max(dial.position, new_position),
    )
    |> list.filter(fn(n) {
      case int.modulo(n, 100) {
        Ok(0) -> {
          True
        }
        _ -> False
      }
    })
    |> list.length

  #(Dial(int.modulo(new_position, 100) |> result.unwrap(0)), times_past_zero)
}

fn parse(input: String) -> List(Instruction) {
  input
  |> string.split(on: "\n")
  |> list.map(fn(instruction) {
    case instruction {
      "L" <> amount -> {
        Instruction(Left, int.parse(amount) |> result.unwrap(0))
      }
      "R" <> amount -> {
        Instruction(Right, int.parse(amount) |> result.unwrap(0))
      }
      _ -> {
        panic
      }
    }
  })
}
