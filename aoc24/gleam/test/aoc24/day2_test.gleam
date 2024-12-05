import gleam/string
import gleeunit
import gleeunit/should

import aoc24/day2

pub fn main() {
  gleeunit.main()
}

pub fn run_test() {
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"
  |> string.trim()
  |> string.split("\n")
  |> day2.run()
  |> should.equal(#(2, 4))
}
