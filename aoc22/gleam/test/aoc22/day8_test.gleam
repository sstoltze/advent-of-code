import gleam/dict
import gleam/string

import gleeunit
import gleeunit/should

import aoc22/day8.{TreeInfo}

pub fn main() {
  gleeunit.main()
}

pub fn pipeline_test() {
  "30373
25512
65332
33549
35390"
  |> string.split("\n")
  |> day8.parse_map()
  |> day8.calculate_visible()
  |> day8.count_visible()
  |> should.equal(21)
}

pub fn parse_map_test() {
  let map =
    dict.new()
    |> dict.insert(#(0, 0), TreeInfo(height: 3, visible: False))
    |> dict.insert(#(0, 1), TreeInfo(height: 0, visible: False))
    |> dict.insert(#(1, 0), TreeInfo(height: 2, visible: False))
    |> dict.insert(#(1, 1), TreeInfo(height: 5, visible: False))
  "30
25"
  |> string.split("\n")
  |> day8.parse_map()
  |> should.equal(map)
}

pub fn calculate_visible_test() {
  let map =
    dict.new()
    |> dict.insert(#(0, 0), TreeInfo(height: 3, visible: False))
    |> dict.insert(#(0, 1), TreeInfo(height: 0, visible: False))
    |> dict.insert(#(1, 0), TreeInfo(height: 2, visible: False))
    |> dict.insert(#(1, 1), TreeInfo(height: 5, visible: False))
  day8.calculate_visible(map)
  |> should.equal(
    dict.new()
    |> dict.insert(#(0, 0), TreeInfo(height: 3, visible: True))
    |> dict.insert(#(0, 1), TreeInfo(height: 0, visible: True))
    |> dict.insert(#(1, 0), TreeInfo(height: 2, visible: True))
    |> dict.insert(#(1, 1), TreeInfo(height: 5, visible: True)),
  )

  let invisible_center_map =
    dict.new()
    |> dict.insert(#(0, 0), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(0, 1), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(0, 2), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(1, 0), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(1, 1), TreeInfo(height: 0, visible: True))
    |> dict.insert(#(1, 2), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(2, 0), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(2, 1), TreeInfo(height: 9, visible: True))
    |> dict.insert(#(2, 2), TreeInfo(height: 9, visible: True))

  day8.calculate_visible(invisible_center_map)
  |> should.equal(
    invisible_center_map
    |> dict.insert(#(1, 1), TreeInfo(height: 0, visible: False)),
  )
}

pub fn count_visible_test() {
  let map =
    dict.new()
    |> dict.insert(#(0, 0), TreeInfo(height: 3, visible: True))
    |> dict.insert(#(0, 1), TreeInfo(height: 0, visible: False))
    |> dict.insert(#(1, 0), TreeInfo(height: 2, visible: True))
    |> dict.insert(#(1, 1), TreeInfo(height: 5, visible: False))
  day8.count_visible(map) |> should.equal(2)
}
