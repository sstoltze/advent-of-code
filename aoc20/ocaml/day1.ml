let input = CCIO.(with_in "../input/day1.txt" read_lines_l) |> List.map int_of_string

let rec find_elem target = function
    a :: a' -> if a = target then Some a else find_elem target a'
  | []      -> None

let rec find_sum sum = function
    a :: a' -> (match find_elem (sum - a) a' with
        Some b -> Some (a,b)
      | None   -> find_sum sum a')
  | [] -> None

let day1_mul =  function
    Some (x,y) -> x*y
  | None       -> 0

let rec find_multiple_sum k sum =
  match (k,sum) with
    (0,0)         -> (fun _ -> Some [])
  | (_,0) | (0,_) -> (fun _ -> None)
  | (_,_) -> (function
        a :: a' -> if sum - a >= 0
        then (match find_multiple_sum (k-1) (sum-a) a' with
              Some l -> Some (a :: l)
            | None   -> find_multiple_sum k sum a')
        else find_multiple_sum k sum a'
      | [] -> None)

let day1_list_mul = function
    [] -> 0
  | l -> List.fold_left ( * ) 1 l

let () =
  print_string "Day 01-1: The product of the numbers are: ";
  input
  |> find_sum 2020
  |> day1_mul
  |> print_int;
  print_newline ();
  print_string "Day 01-2: The product of the numbers are: ";
  input |> find_multiple_sum 3 2020 |> (Option.fold ~none:0 ~some:day1_list_mul) |> print_int;
  print_newline ();
