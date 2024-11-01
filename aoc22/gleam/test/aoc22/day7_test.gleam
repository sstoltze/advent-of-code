// @related [subject](aoc22/gleam/src/aoc22/day7.gleam)
import gleeunit
import gleeunit/should

import aoc22/day7.{Directory, File}

pub fn main() {
  gleeunit.main()
}

pub fn filesystem_size_test() {
  day7.filesystem_size(Directory(name: "/", contents: []))
  |> should.equal(0)

  day7.filesystem_size(File(name: "a", size: 123))
  |> should.equal(123)

  day7.filesystem_size(
    Directory(name: "a dir", contents: [
      File(name: "a", size: 1),
      File(name: "b", size: 2),
    ]),
  )
  |> should.equal(3)
}
