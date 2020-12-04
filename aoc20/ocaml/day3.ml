type map_tile = Open
              | Tree

type map = { tiles: map_tile list list; width: int; height: int}

type position = { east: int; south: int }

let make_position east south = {east; south}

let parse_map_line l =
  String.to_seq l
  |> List.of_seq
  |> List.map (fun c -> if c = '.' then Open else Tree)

let input =
  let tiles = CCIO.(with_in "../input/day3.txt" read_lines_l) |> List.map parse_map_line
  in {tiles; width = tiles |> List.hd |> List.length; height = List.length tiles}

let map_tile_at map position =
  if position.south >= map.height
  then None
  else Some (List.nth (List.nth map.tiles position.south) (position.east mod map.width))

let rec walk map position east south =
  let new_pos = make_position ((position.east + east) mod map.width) (position.south + south)
  in match map_tile_at map new_pos with
    Some tile -> tile :: walk map new_pos east south
  | None -> []

let find_trees path = path
                      |> List.filter (fun t -> t = Tree)
                      |> List.length

let () =
  print_string "Day 03-1: The ride encounters ";
  let path = walk input (make_position 0 0) 3 1 in
  let trees = find_trees path in
  print_int trees;
  print_string " trees.";
  print_newline ();
  print_string "Day 03-2: The product of trees on all slopes is ";
  let start = make_position 0 0 in
  let paths = [walk input start 1 1; walk input start 3 1; walk input start 5 1; walk input start 7 1; walk input start 1 2] in
  let trees = paths |> List.map find_trees |> List.fold_left ( * ) 1 in
  print_int trees;
  print_string ".";
  print_newline ()
