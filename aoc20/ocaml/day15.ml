type game_state = { turn: int; spoken: int list; last_spoken: int }

let game_from_spoken l =
  { turn = List.length l; spoken = List.rev l; last_spoken = List.hd (List.rev l) }

let parse_input s =
  s
  |> String.trim
  |> String.split_on_char ','
  |> List.map int_of_string
  |> game_from_spoken

let input = CCIO.(with_in "../input/day15.txt" read_all) |> parse_input

let find_last_round_spoken state k =
  let find_index l x =
    let rec find_index' l' x' i' =
      match l' with
      | [] -> None
      | x'' :: l'' -> if x'' = x'
                      then Some i'
                      else find_index' l'' x' (i' + 1) in
    find_index' l x 0 in
  let round = state.turn in
  let spoken = state.spoken in
  match find_index (List.tl spoken) k with
  | None -> None
  | Some j -> Some (round - j)

let play_round state =
  let current_round = state.turn + 1 in
  let now_spoken = match find_last_round_spoken state (state.last_spoken) with
    | None -> 0
    | Some n -> current_round - n in
  { turn = current_round; spoken = now_spoken :: state.spoken; last_spoken = now_spoken; }

let rec spoken_in_round state round =
  if state.turn = round
  then state.last_spoken
  else spoken_in_round (play_round state) round

let part_one state = spoken_in_round state 2020

let part_two state = spoken_in_round state 30000000

let () =
  print_string "Day 15-1: The 2020th number spoken is ";
  input |> part_one |> print_int;
  print_newline ();
  print_string "Day 15-2: The 30000000th number spoken is ";
  input |> part_two |> print_int;
  print_newline ();
