import gleeunit
import gleeunit/should

import aoc22/day6

pub fn main() {
  gleeunit.main()
}

pub fn start_of_packet_test() {
  day6.start_of_packet("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  |> should.equal(7)
  day6.start_of_packet("bvwbjplbgvbhsrlpgdmjqwftvncz")
  |> should.equal(5)
  day6.start_of_packet("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  |> should.equal(10)
  day6.start_of_packet("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
  |> should.equal(11)
}
