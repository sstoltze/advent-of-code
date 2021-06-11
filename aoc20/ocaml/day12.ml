type direction = N | S | E | W
type movement = L | R | F
type instruction =
    | Dir of direction * int
    | Mov of movement * int
type ship = { facing: direction; x: int; y: int; waypoint: int * int }

let initial_ship = { facing = E; x = 0; y = 0; waypoint = (10,1) }

let parse_instruction i =
  let len = String.length i
  in let number = String.sub i 1 (len - 1) |> int_of_string
     in match String.get i 0 with
          'N' -> Dir (N, number)
        | 'S' -> Dir (S, number)
        | 'E' -> Dir (E, number)
        | 'W' -> Dir (W, number)
        | 'F' -> Mov (F, number)
        | 'L' -> Mov (L, number)
        | 'R' -> Mov (R, number)
        | _   -> Mov (F, 0) (* Do nothing *)

let parse_string s = s
                     |> String.trim
                     |> String.split_on_char '\n'
                     |> List.map parse_instruction

let input = CCIO.(with_in "../input/day12.txt" read_all)
            |> parse_string

let move_boat boat x y = { boat with x = boat.x + x; y = boat.y + y }

let turn_boat boat degrees =
  let index_of x l =
    let rec index_of' k x = function
        [] -> exit 0
      | (y :: ys) -> if x = y then k else index_of' (k+1) x ys
    in index_of' 0 x l
  in let compass = [N; E; S; W;]
     in let current_index = index_of boat.facing compass
        in let new_index = (current_index + (degrees / 90)) mod 4
           in let new_facing = List.nth compass (if new_index < 0 then new_index + 4 else new_index)
              in { boat with facing = new_facing }

let rec run_boat_instruction boat instruction =
  match instruction with
    Dir (N, n) -> move_boat boat 0 n
  | Dir (S, n) -> move_boat boat 0 (-n)
  | Dir (E, n) -> move_boat boat n 0
  | Dir (W, n) -> move_boat boat (-n) 0
  | Mov (F, n) -> run_boat_instruction boat (Dir (boat.facing, n))
  | Mov (L, n) -> turn_boat boat (-n)
  | Mov (R, n) -> turn_boat boat n

let run_boat_instructions boat instructions = List.fold_left run_boat_instruction boat instructions

let move_waypoint boat x y =
  let (wp_x, wp_y) = boat.waypoint
  in { boat with waypoint = (wp_x + x, wp_y + y)}

let rec turn_waypoint boat degrees =
  if degrees < 0
  then turn_waypoint boat (degrees + 360)
  else
    let rec turn_times boat k =
      if k = 0
      then boat
      else let (wp_x, wp_y) = boat.waypoint
           in turn_times { boat with waypoint = (wp_y, -wp_x) } (k - 1)
            in turn_times boat (degrees / 90)

let rec move_boat_to_waypoint boat k =
  if k = 0
  then boat
  else let (wp_x, wp_y) = boat.waypoint
       in move_boat_to_waypoint { boat with x = boat.x + wp_x; y = boat.y + wp_y } (k-1)

let rec run_waypoint_instruction boat instruction =
  match instruction with
    Dir (N, n) -> move_waypoint boat 0 n
  | Dir (S, n) -> move_waypoint boat 0 (-n)
  | Dir (E, n) -> move_waypoint boat n 0
  | Dir (W, n) -> move_waypoint boat (-n) 0
  | Mov (F, n) -> move_boat_to_waypoint boat n
  | Mov (L, n) -> turn_waypoint boat (-n)
  | Mov (R, n) -> turn_waypoint boat n

let run_waypoint_instructions boat instructions = List.fold_left run_waypoint_instruction boat instructions

let manhattan_distance boat = (abs boat.x) + (abs boat.y)

let () =
  print_string "Day 12-1: The manhattan distance of the boat is ";
  input |> run_boat_instructions initial_ship |> manhattan_distance |> print_int;
  print_newline ();
  print_string "Day 12-2: The manhattan distance of the boat after doing things correctly is ";
  input |> run_waypoint_instructions initial_ship |> manhattan_distance |> print_int;
  print_newline ()
