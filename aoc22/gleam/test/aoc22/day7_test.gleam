import gleeunit
import gleeunit/should

import aoc22/day7.{Directory, File}

pub fn main() {
  gleeunit.main()
}

pub fn sum_of_dirs_test() {
  day7.sum_of_dirs(Directory(name: "/", contents: []))
  |> should.equal(0)

  day7.sum_of_dirs(File(name: "a", size: 123))
  |> should.equal(123)

  day7.sum_of_dirs(
    Directory(name: "a dir", contents: [
      File(name: "a", size: 1),
      File(name: "b", size: 2),
    ]),
  )
  |> should.equal(3)
}
