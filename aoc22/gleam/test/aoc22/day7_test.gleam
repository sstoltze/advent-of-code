// @related [subject](aoc22/gleam/src/aoc22/day7.gleam)
import gleeunit
import gleeunit/should

import gleam/string

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

pub fn build_filesystem_test() {
  let input =
    "$ cd /
$ ls
dir a
14848514 b.txt
$ cd a
$ ls
29116 f
2557 g"
    |> string.split("\n")
  day7.build_file_system(input)
  |> should.equal(
    Directory(name: "/", contents: [
      Directory(name: "a", contents: [
        File(name: "f", size: 29_116),
        File(name: "g", size: 2557),
      ]),
      File(name: "b.txt", size: 14_848_514),
    ]),
  )
}
