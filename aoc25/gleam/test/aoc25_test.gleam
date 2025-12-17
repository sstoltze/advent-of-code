import gleeunit

import aoc25/day1
import aoc25/day4
import aoc25/day5
import aoc25/day6

pub fn main() -> Nil {
  gleeunit.main()
}

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

pub fn day_4_test() {
  let input =
    "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."
  assert day4.part_1(input) == 13
  assert day4.part_2(input) == 43
}

pub fn day_5_test() {
  let input =
    "3-5
10-14
16-20
12-18

1
5
8
11
17
32"
  assert day5.part_1(input) == 3
  assert day5.part_2(input) == 14
}

pub fn day_6_test() {
  let input =
    "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +"
  assert day6.part_1(input) == 4_277_556
}
