// @related [test](aoc22/gleam/test/aoc22/day7_test.gleam)
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type FileSystem {
  File(name: String, size: Int)
  Directory(name: String, contents: List(FileSystem), size: Option(Int))
}

pub type Location =
  List(String)

pub type Command {
  Ls(contents: List(FileSystem))
  Cd(location: String)
}

pub fn filesystem_size(file_system: FileSystem) -> Int {
  case file_system {
    Directory(contents: contents, size: None, ..) ->
      list.fold(over: contents, from: 0, with: fn(acc, fs) {
        acc + filesystem_size(fs)
      })
    Directory(size: Some(size), ..) -> size
    File(size: size, ..) -> size
  }
}

pub fn calculate_sizes(file_system: FileSystem) -> FileSystem {
  case file_system {
    Directory(name: name, contents: contents, size: None) -> {
      let #(size, sized_contents) =
        list.map_fold(over: contents, from: 0, with: fn(acc, fs) {
          let sized_fs = calculate_sizes(fs)
          let size = case sized_fs {
            File(size: size, ..) -> size
            Directory(size: Some(size), ..) -> size
            Directory(size: None, ..) -> panic
          }
          #(acc + size, sized_fs)
        })
      Directory(name: name, contents: sized_contents, size: Some(size))
    }

    _ -> file_system
  }
}

pub fn small_directory(file_system: FileSystem) -> Bool {
  case calculate_sizes(file_system) {
    Directory(size: size, ..) -> {
      case size {
        Some(a) if a <= 100_000 -> True
        _ -> False
      }
    }
    File(..) -> False
  }
}

pub fn directory_larger_than(file_system: FileSystem, min_size: Int) -> Bool {
  case calculate_sizes(file_system) {
    Directory(size: size, ..) -> {
      case size {
        Some(a) if a >= min_size -> True
        _ -> False
      }
    }
    File(..) -> False
  }
}

pub fn find_directories(
  file_system: FileSystem,
  with predicate: fn(FileSystem) -> Bool,
) -> List(FileSystem) {
  let sized_fs = calculate_sizes(file_system)
  let current_result = case predicate(sized_fs) {
    True -> [sized_fs]
    False -> []
  }
  let subdir_results = case sized_fs {
    Directory(contents: contents, ..) -> {
      list.flat_map(over: contents, with: fn(fs) {
        find_directories(fs, with: predicate)
      })
    }
    _ -> []
  }
  list.append(subdir_results, current_result)
}

pub fn build_file_system(cli: List(String)) -> FileSystem {
  parse_cli(cli, Directory(name: "/", contents: [], size: None), [])
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
    Directory(name: name, contents: contents, ..) -> {
      case location {
        [] -> Directory(name: name, contents: new_contents, size: None)
        [next, ..remaining] -> {
          let updated_contents =
            list.map(contents, with: fn(fs) {
              case fs.name == next {
                True -> set_contents(fs, remaining, new_contents)
                False -> fs
              }
            })
          Directory(name: name, contents: updated_contents, size: None)
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
      take_contents(more_content, [
        Directory(name: name, contents: [], size: None),
        ..acc
      ])
    [file, ..more_content] -> {
      let assert [s, name] = string.split(file, " ")
      let assert Ok(size) = int.parse(s)
      take_contents(more_content, [File(name: name, size: size), ..acc])
    }
  }
}

pub fn part_one(file_system: FileSystem) -> Int {
  let small_dirs = find_directories(file_system, with: small_directory)
  let small_size_total =
    list.fold(over: small_dirs, from: 0, with: fn(acc, fs) {
      let assert Directory(size: Some(size), ..) = fs
      acc + size
    })
  small_size_total
}

pub fn part_two(file_system: FileSystem) -> Int {
  let total_size = 70_000_000
  let required_space = 30_000_000
  let assert Directory(size: Some(used_space), ..) = file_system
  let unused_space = total_size - used_space
  let required_to_be_deleted = required_space - unused_space
  let possible_dirs = find_directories(file_system, with: fn (fs) { directory_larger_than(fs, required_to_be_deleted)})
  let assert [Directory(size: Some(size), ..), ..] = list.sort(possible_dirs, by: fn (a, b) {
    let assert Directory(size: Some(sa), ..) = a
    let assert Directory(size: Some(sb), ..) = b
int.compare(sa, sb)
  })
  size
}

pub fn main(input: List(String)) -> #(Int, Int) {
  let fs = build_file_system(input) |> calculate_sizes()

  let small_size_total = part_one(fs)
  let size_of_directory_to_delete = part_two(fs)
  #(small_size_total, size_of_directory_to_delete)
}
