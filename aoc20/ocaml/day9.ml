let parse_string s = s |> String.split_on_char '\n' |> List.map (fun s -> String.trim s |> int_of_string)

let input = CCIO.(with_in "../input/day9.txt" read_all) |> parse_string

type xmas = { seen: int list; preamble_length: int }

let rec split n l =  match (n, l) with
    (0, xs) -> ([], xs)
  | (_, []) -> ([], [])
  | (n, x :: xs) -> let (a, b) = split (n-1) xs in (x :: a, b)

let take n l = split n l |> fst

let rec find_sum sum = function
    a :: a' -> let b = sum - a in
    if b != a && List.mem b a'
    then true
    else find_sum sum a'
  | []      -> false

let validate_number xmas k =
  let preamble = take xmas.preamble_length xmas.seen in
  if find_sum k preamble
  then Some {xmas with seen = k :: xmas.seen}
  else None

let validate_list len l =
  let validate_some (xmas, res) k = match xmas with
      Some x -> (match validate_number x k with
          Some x' -> (Some x', res)
        | None    -> (None, k))
    | None   -> (None, res)
  and (preamble, rest) = split len l in
  if List.length preamble < len
  then (None, -1)
  else let xmas = Some { seen = List.rev preamble; preamble_length = len; } in
    List.fold_left validate_some (xmas, -1) rest

let all_contiguous_sums sum l =
  let rec take_sum acc = function
      []        -> None
    | x :: xs -> let new_acc = acc + x in
      if new_acc > sum
      then None
      else if new_acc = sum
      then Some [x]
      else take_sum new_acc xs |> Option.map (fun l -> x :: l)
  and all_prefixes = function
      [] -> []
    | x :: xs -> (x :: xs) :: all_prefixes xs
  in List.filter_map (take_sum 0) (all_prefixes l) |> List.filter (fun list -> List.length list >= 2)

let min_max l =
  let sorted = List.sort ( Int.compare ) l
  in (List.hd sorted, List.rev sorted |> List.hd)

let () =
  print_string "Day 9-1: The first number that does not have the property is ";
  let res = input |> validate_list 25 |> snd in
  print_int res;
  print_newline ();
  print_string "Day 9-2: The encryption weakness is ";
  all_contiguous_sums res input |> List.map min_max |> List.map (fun (x, y) -> x + y) |> List.hd |> print_int;
  print_newline ()
