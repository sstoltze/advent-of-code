import gleam/string
import gleeunit
import gleeunit/should

import aoc24/day3

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn run_test() {
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  |> string.trim()
  |> string.split("\n")
  |> day3.run()
  |> should.equal(#(161, 48))
}
