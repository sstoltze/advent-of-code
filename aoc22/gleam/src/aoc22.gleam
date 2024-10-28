import aoc22/day6
import gleam/io

pub fn main() {
  let assert Ok(input) = day6.input_from_file("../input/day6.txt")
  io.debug(day6.day6(input))
}
