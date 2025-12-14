import gleeunit

import aoc25/day1

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn day_1_test() {
  let input =
    "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
  assert day1.part_1(input) == 3

  assert day1.part_2(input) == 6
}
