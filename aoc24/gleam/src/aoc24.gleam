import argv
import file_streams/file_stream
import file_streams/file_stream_error
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import aoc24/day1
import aoc24/day2
import aoc24/day3
import aoc24/day4

fn read_lines(
  stream: file_stream.FileStream,
  result: List(String),
) -> List(String) {
  case file_stream.read_line(stream) {
    Ok(line) -> read_lines(stream, [string.trim(line), ..result])
    Error(_) -> list.reverse(result)
  }
}

fn input_from_file(
  file: String,
) -> Result(List(String), file_stream_error.FileStreamError) {
  use stream <- result.try(file_stream.open_read(file))
  let result = read_lines(stream, [])
  let assert Ok(Nil) = file_stream.close(stream)
  Ok(result)
}

pub fn main() {
  let day = case argv.load().arguments {
    [day] -> day
    _ -> panic as { "Usage: gleam run <day>" }
  }

  let input_file =
    "../input/day"
    |> string.append(day)
    |> string.append(".txt")

  let input = case input_from_file(input_file) {
    Ok(lines) -> lines
    _ ->
      panic as {
        "Can't read file "
        |> string.append(input_file)
        |> string.append(" for requested day ")
        |> string.append(day)
      }
  }

  case day {
    "1" -> day1.run(input)
    "2" -> day2.run(input)
    "3" -> day3.run(input)
    "4" -> day4.run(input)
    _ -> panic
  }
  |> io.debug()
}
