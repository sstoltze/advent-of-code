// @related [subject](aoc22/gleam/src/aoc22/day7.gleam)
import gleeunit
import gleeunit/should

import gleam/option.{None, Some}
import gleam/string

import aoc22/day7.{Directory, File}

pub fn main() {
  gleeunit.main()
}

pub fn filesystem_size_test() {
  day7.filesystem_size(Directory(name: "/", contents: [], size: None))
  |> should.equal(0)

  day7.filesystem_size(File(name: "a", size: 123))
  |> should.equal(123)

  day7.filesystem_size(Directory(
    name: "a dir",
    contents: [File(name: "a", size: 1), File(name: "b", size: 2)],
    size: None,
  ))
  |> should.equal(3)

  day7.filesystem_size(Directory(
    name: "/",
    contents: [
      Directory(
        name: "a",
        contents: [File(name: "a.txt", size: 10)],
        size: None,
      ),
    ],
    size: None,
  ))
  |> should.equal(10)
}

pub fn find_directories_test() {
  day7.find_directories(Directory(
    name: "/",
    contents: [
      Directory(
        name: "a",
        contents: [File(name: "a.txt", size: 10)],
        size: None,
      ),
    ],
    size: None,
  ), with: day7.small_directory)
  |> should.equal([
    Directory(
      name: "a",
      contents: [File(name: "a.txt", size: 10)],
      size: Some(10),
    ),
    Directory(
      name: "/",
      contents: [
        Directory(
          name: "a",
          contents: [File(name: "a.txt", size: 10)],
          size: Some(10),
        ),
      ],
      size: Some(10),
    ),
  ])
}

pub fn calculate_sizes_test() {
  day7.calculate_sizes(Directory(
    name: "/",
    contents: [
      Directory(
        name: "a",
        contents: [File(name: "a.txt", size: 10)],
        size: None,
      ),
      Directory(
        name: "b",
        contents: [File(name: "b.txt", size: 20), File(name: "c.txt", size: 10)],
        size: None,
      ),
    ],
    size: None,
  ))
  |> should.equal(Directory(
    name: "/",
    contents: [
      Directory(
        name: "a",
        contents: [File(name: "a.txt", size: 10)],
        size: Some(10),
      ),
      Directory(
        name: "b",
        contents: [File(name: "b.txt", size: 20), File(name: "c.txt", size: 10)],
        size: Some(30),
      ),
    ],
    size: Some(40),
  ))
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
  |> should.equal(Directory(
    name: "/",
    contents: [
      Directory(
        name: "a",
        contents: [File(name: "f", size: 29_116), File(name: "g", size: 2557)],
        size: None,
      ),
      File(name: "b.txt", size: 14_848_514),
    ],
    size: None,
  ))
}

pub fn nested_directories_test() {
  let input =
    "$ cd /
$ ls
dir a
$ cd a
$ ls
dir a
$ cd a
$ ls
20 a"
    |> string.split("\n")
  day7.build_file_system(input)
  |> should.equal(Directory(
    name: "/",
    contents: [
      Directory(
        name: "a",
        contents: [
          Directory(
            name: "a",
            contents: [File(name: "a", size: 20)],
            size: None,
          ),
        ],
        size: None,
      ),
    ],
    size: None,
  ))
}
