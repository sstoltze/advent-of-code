let input = CCIO.(with_in "../input/day5.txt" read_lines_l)

type possible_seat = { row_min : int; row_max : int; seat_min : int; seat_max : int;}

let start_seat = { row_min = 0; row_max = 127; seat_min = 0; seat_max = 7; }

let seat_id seat =
  if seat.row_max = seat.row_min && seat.seat_max = seat.seat_min
  then Some (8 * seat.row_max + seat.seat_max)
  else None

let update_seat seat = function
  'F' ->   { seat with row_max  = (seat.row_max + seat.row_min - 1) / 2 }
  | 'B' -> { seat with row_min  = (seat.row_max + seat.row_min + 1) / 2 }
  | 'L' -> { seat with seat_max = (seat.seat_max + seat.seat_min - 1) / 2 }
  | 'R' -> { seat with seat_min = (seat.seat_max + seat.seat_min + 1) / 2 }
  | _ -> raise Exit

let check_position partition = partition |> String.to_seq |> Seq.fold_left update_seat start_seat |> seat_id

let get_seats l = l |> List.filter Option.is_some |> List.map Option.get

let possible_boarding_pass_id visible id =
  not (List.mem id visible) && List.mem (id-1) visible && List.mem (id+1) visible

let all_ids = List.init (8 * 128 + 8) (fun x -> x)

let () =
  print_string "Day 5-1: The max id among the visible boarding passes is ";
  input |> List.map check_position |> get_seats |> List.fold_left max 0 |> print_int;
  print_newline ();
  print_string "Day 5-2: Your seat is ";
  all_ids |> List.filter (possible_boarding_pass_id (input |> List.map check_position |> get_seats)) |> List.hd |> print_int;
  print_newline ()
