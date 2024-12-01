import file_streams/file_stream
import file_streams/file_stream_error
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import aoc24/day1

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
  let assert Ok(input) = input_from_file("../input/day1.txt")
  day1.run(input) |> io.debug()
}
