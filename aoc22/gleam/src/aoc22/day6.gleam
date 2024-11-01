import gleam/list
import gleam/string

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

pub fn main(input: List(String)) -> #(Int, Int) {
  let assert [packet] = input
  #(start_of_packet(packet), start_of_message(packet))
}
