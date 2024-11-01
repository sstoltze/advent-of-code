// @related [test](aoc22/gleam/test/aoc22/day7_test.gleam)
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/option.{None, Some}
import gleam/string

pub type FileSystem {
  File(name: String, size: Int)
  Directory(name: String, contents: List(FileSystem))
}

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
  parse_cli(cli, Directory(name: "/", contents: []), "/")
}

pub fn parse_cli(
  cli: List(String),
  current_fs: FileSystem,
  current_location: String,
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
  location: String,
) -> #(FileSystem, String) {
  case command {
    Cd(dir) -> #(file_system, change_directory(file_system, location, dir))
    Ls(contents) -> #(set_contents(file_system, location, contents), location)
  }
}

fn change_directory(
  file_system: FileSystem,
  location: String,
  new_location: String,
) -> String {
  case new_location {
    ".." ->
      case find_parent_directory(file_system, location) {
        Some(name) -> name
        None -> {
          io.debug(file_system)
          io.debug(location)
          panic
        }
      }
    _ -> new_location
  }
}

fn find_parent_directory(file_system: FileSystem, location: String) {
  let assert Directory(name: name, contents: contents) = file_system
  let subdirs =
    list.filter(contents, keeping: fn(f) {
      case f {
        Directory(..) -> True
        _ -> False
      }
    })
  list.fold_until(over: subdirs, from: None, with: fn(acc, d) {
    let assert Directory(name: subdir_name, ..) = d
    case subdir_name == location {
      True -> Stop(Some(name))
      False -> {
        case find_parent_directory(d, location) {
          Some(name) -> Stop(Some(name))
          None -> Continue(acc)
        }
      }
    }
  })
}

fn set_contents(
  file_system: FileSystem,
  location: String,
  new_contents: List(FileSystem),
) -> FileSystem {
  case file_system {
    Directory(name: name, contents: contents) ->
      case name == location {
        True -> Directory(name: name, contents: new_contents)
        False ->
          Directory(
            name: name,
            contents: list.map(contents, fn(c) {
              set_contents(c, location, new_contents)
            }),
          )
      }
    File(..) -> file_system
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
