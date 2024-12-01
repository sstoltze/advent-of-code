import gleam/string
import gleeunit
import gleeunit/should

import aoc24/day1

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn run_test() {
  "
3   4
4   3
2   5
1   3
3   9
3   3"
  |> string.trim()
  |> string.split("\n")
  |> day1.run()
  |> should.equal(#(11, 0))
}
