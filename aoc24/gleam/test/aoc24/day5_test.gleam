import gleam/dict
import gleam/string
import gleeunit
import gleeunit/should

import aoc24/day5

pub fn main() {
  gleeunit.main()
}

pub fn parse_input_test() {
  "47|53
97|13

47,53"
  |> string.split("\n")
  |> day5.parse_input()
  |> should.equal(#(dict.from_list([#(53, [47]), #(13, [97])]), [[47, 53]]))
}

pub fn is_valid_update_test() {
  day5.is_valid_update([47, 53], dict.from_list([#(53, [47])]))
  |> should.equal(True)
  day5.is_valid_update([47, 53], dict.from_list([#(47, [53])]))
  |> should.equal(False)
}

pub fn middle_element_test() {
  day5.middle_element([1, 2, 3]) |> should.equal(2)
  day5.middle_element([1]) |> should.equal(1)
}

pub fn run_test() {
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"
  |> string.trim()
  |> string.split("\n")
  |> day5.run()
  |> should.equal(#(143, 123))
}
