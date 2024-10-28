import file_streams/file_stream
import file_streams/file_stream_error
import gleam/list
import gleam/result
import gleam/string

fn read_lines(
  stream: file_stream.FileStream,
  result: List(String),
) -> List(String) {
  case file_stream.read_line(stream) {
    Ok(line) -> read_lines(stream, [line, ..result])
    Error(_) -> list.reverse(result)
  }
}

pub fn input_from_file(
  file: String,
) -> Result(String, file_stream_error.FileStreamError) {
  use stream <- result.try(file_stream.open_read(file))
  let assert [result] = read_lines(stream, [])
  let assert Ok(Nil) = file_stream.close(stream)
  Ok(result)
}

pub fn start_of_packet(packet: String) -> Int {
  start_of(packet, 0, 4)
}

pub fn start_of_message(packet: String) -> Int {
  start_of(packet, 0, 14)
}

fn start_of(packet: String, result: Int, length: Int) -> Int {
  let chars = packet |> string.to_graphemes() |> list.take(length)
  case list.unique(chars) == chars {
    True -> result + length
    False -> start_of(string.drop_left(packet, 1), result + 1, length)
  }
}

pub fn day6(input: String) -> #(Int, Int) {
  #(start_of_packet(input), start_of_message(input))
}
