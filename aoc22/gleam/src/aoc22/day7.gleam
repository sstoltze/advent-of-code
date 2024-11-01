// @related [test](aoc22/gleam/test/aoc22/day7_test.gleam)
import gleam/list

pub type FileSystem {
  File(name: String, size: Int)
  Directory(name: String, contents: List(FileSystem))
}

pub fn filesystem_size(file_system: FileSystem) -> Int {
  case file_system {
    Directory(contents: contents, ..) ->
      list.fold(over: contents, from: 0, with: fn(acc, fs) {
        acc + filesystem_size(fs)
      })
    File(size: size, ..) -> size
  }
}

pub fn build_file_system(_cli: List(String)) -> FileSystem {
  Directory(name: "./", contents: [])
}

pub fn main(_input: FileSystem) -> #(Int, Int) {
  #(0, 0)
}
