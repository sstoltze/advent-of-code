type seat = Empty | Occupied

type position = Floor | Seat of seat

let parse_position = function
    'L' -> Seat Empty
  | '#' -> Seat Occupied
  | _   -> Floor

let parse_string s = s
                     |> String.trim
                     |> String.split_on_char '\n'
                     |> List.map (fun s -> String.trim s
                                           |> String.to_seq
                                           |> List.of_seq
                                           |> List.map parse_position)

let input = CCIO.(with_in "../input/day11.txt" read_all)
            |> parse_string

let get_coord_opt map (x,y) =
  if x < 0 || y < 0
  then None
  else List.nth_opt map y
       |> Option.map (fun l -> List.nth_opt l x)
       |> Option.join

let list_product l m =
  let rec list_product' acc l' m' =
    match (l', m') with
      ([], _) -> acc
    | (_ :: is, []) -> list_product' acc is m
    | ((i :: is), (j :: js)) -> list_product' ((i,j) :: acc) (i :: is) js
  in list_product' [] l m

let directions = list_product [-1; 0; 1] [-1; 0; 1]
                 |> List.filter (fun p -> p <> (0,0))

let count_occupied_neighbours f map (x,y) =
  f map (x,y)
  |> List.filter (fun s -> s = Seat Occupied)
  |> List.length

let update_seat neighbour_map seat_updater_map =
  fun map (x,y) ->
  let occupied_neighbours = count_occupied_neighbours neighbour_map map (x,y) in
  get_coord_opt map (x,y)
  |> Option.map (fun s -> match s with
                            Seat a -> Seat (seat_updater_map a occupied_neighbours)
                          | Floor         -> Floor)

let update seat_updater map =
  List.mapi (fun j l -> List.mapi (fun i _ -> seat_updater map (i,j)
                                              |> Option.get)
                          l)
    map

let rec run update map =
  let new_map = update map
  in if map = new_map
     then map
     else run update new_map

let count_occupied map = List.fold_left (fun n l -> n + (List.filter (fun s -> s = Seat Occupied) l |> List.length)) 0 map

(* V1 *)
let neighbours_v1 map (x,y) =
  directions
  |> List.map (fun (i,j) -> get_coord_opt map (x+i,y+j))
  |> List.filter Option.is_some
  |> List.map Option.get

let seat_updater_v1 seat neighbours =
  match (seat, neighbours) with
    (Occupied, n) when n >= 4 -> Empty
  | (Empty, 0)                -> Occupied
  | (s, _)                    -> s

let update_seat_v1 = update_seat neighbours_v1 seat_updater_v1

let update_v1 = update update_seat_v1

(* V2 *)
let rec walk map (x,y) (i,j) =
  match get_coord_opt map (x+i,y+j) with
    None        -> None
  | Some Floor  -> walk map (x+i,y+j) (i,j)
  | Some Seat a -> Some (Seat a)

let neighbours_v2 map (x,y) =
  List.map (walk map (x,y)) directions
  |> List.filter Option.is_some
  |> List.map Option.get

let seat_updater_v2 seat neighbours =
  match (seat, neighbours) with
    (Occupied, n) when n >= 5 -> Empty
  | (Empty, 0)                -> Occupied
  | (s,_)                     -> s

let update_seat_v2 = update_seat neighbours_v2 seat_updater_v2

let update_v2 = update update_seat_v2

let () =
  print_string "Day 11-1: The number of occupied seats after everyone has finished shifting is ";
  input |> run update_v1 |> count_occupied |> print_int;
  print_newline ();
  print_string "Day 11-2: The number of occupied seats with updated rules are ";
  input |> run update_v2 |> count_occupied |> print_int;
  print_newline ()
