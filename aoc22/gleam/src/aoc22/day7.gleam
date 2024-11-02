// @related [test](aoc22/gleam/test/aoc22/day7_test.gleam)
import gleam/int
import gleam/io
import gleam/list
import gleam/string

pub type FileSystem {
  File(name: String, size: Int)
  Directory(name: String, contents: List(FileSystem))
}

pub type Location =
  List(String)

pub type Command {
  Ls(contents: List(FileSystem))
  Cd(location: String)
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

pub fn build_file_system(cli: List(String)) -> FileSystem {
  parse_cli(cli, Directory(name: "/", contents: []), [])
}

pub fn parse_cli(
  cli: List(String),
  current_fs: FileSystem,
  current_location: Location,
) -> FileSystem {
  case cli {
    [] -> current_fs
    [command, ..rest] -> {
      let #(command, remaining_cli) = parse_command(command, rest)
      let #(new_fs, new_location) =
        run_command(command, current_fs, current_location)
      parse_cli(remaining_cli, new_fs, new_location)
    }
  }
}

fn run_command(
  command: Command,
  file_system: FileSystem,
  location: Location,
) -> #(FileSystem, Location) {
  case command {
    Cd(dir) -> #(file_system, change_directory(location, dir))
    Ls(contents) -> #(
      set_contents(file_system, list.drop(location, 1), contents),
      location,
    )
  }
}

fn change_directory(location: Location, new_location: String) -> Location {
  case new_location {
    ".." -> location |> list.reverse() |> list.drop(1) |> list.reverse()
    _ -> list.append(location, [new_location])
  }
}

fn set_contents(
  file_system: FileSystem,
  location: Location,
  new_contents: List(FileSystem),
) -> FileSystem {
  case file_system {
    File(..) -> file_system
    Directory(name: name, contents: contents) -> {
      case location {
        [] -> Directory(name: name, contents: new_contents)
        [next, ..remaining] -> {
          let updated_contents =
            list.map(contents, with: fn(fs) {
              case fs.name == next {
                True -> set_contents(fs, remaining, new_contents)
                False -> fs
              }
            })
          Directory(name: name, contents: updated_contents)
        }
      }
    }
  }
}

fn parse_command(
  command: String,
  rest: List(String),
) -> #(Command, List(String)) {
  case command {
    "$ ls" -> {
      let #(contents, remaining_commands) = take_contents(rest, [])
      #(Ls(contents), remaining_commands)
    }
    "$ cd " <> directory -> #(Cd(directory), rest)
    _ -> {
      io.debug(command)
      panic
    }
  }
}

fn take_contents(
  rest: List(String),
  acc: List(FileSystem),
) -> #(List(FileSystem), List(String)) {
  case rest {
    [] -> #(list.reverse(acc), [])
    ["$ " <> _, ..] -> #(list.reverse(acc), rest)
    ["dir " <> name, ..more_content] ->
      take_contents(more_content, [Directory(name: name, contents: []), ..acc])
    [file, ..more_content] -> {
      let assert [s, name] = string.split(file, " ")
      let assert Ok(size) = int.parse(s)
      take_contents(more_content, [File(name: name, size: size), ..acc])
    }
  }
}

pub fn main(input: List(String)) -> #(Int, Int) {
  let _fs = build_file_system(input)
  #(0, 0)
}
