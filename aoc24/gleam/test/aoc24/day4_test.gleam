import gleam/string
import gleeunit
import gleeunit/should

import aoc24/day4

pub fn main() {
  gleeunit.main()
}

pub fn run_test() {
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
  |> string.trim()
  |> string.split("\n")
  |> day4.run()
  |> should.equal(#(18, 9))
}
