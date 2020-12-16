let parse_string s = s |> String.trim |> String.split_on_char '\n' |> List.map (fun s -> String.trim s |> int_of_string) |> List.sort ( Int.compare )

let input = CCIO.(with_in "../input/day10.txt" read_all) |> parse_string

let rec count_differences = function
    []           -> []
  | [_]          -> [(1,1); (3,1)] (* The last difference is always three and the first is always one *)
  | x :: y :: xs ->
    let difference = y - x
    and differences = count_differences (y :: xs)
    in match List.assoc_opt difference differences with
      Some n -> (difference, n + 1) :: differences
    | None   -> (difference, 1)     :: differences

let output_result l = List.assoc 1 l * List.assoc 3 l

let rec build_subsets acc =
  let possible k l = k - List.hd l <= 3
  in let add k acc =
       (* Tail recursion *)
       let rec add_rec res = function
           [] -> res
         | l :: ls ->
           if possible k l
           then add_rec (l :: (k :: l) :: res) ls
           else add_rec res ls
       in add_rec [] acc
  in function
      []      -> acc
    | [x]     -> add x acc |> List.filter (possible (x + 3))
    | x :: xs -> build_subsets (add x acc) xs

(* Brute force - too slow/memory heavy for the entire list *)
let possible_subsets_hd l = build_subsets [[List.hd l]] (List.tl l)

(* Split the list into smaller parts.
   When two numbers follow each other in the sorted input with a difference of exactly three,
   any configuration must include both.
   That means we can split the list into two smaller lists,
   one up to and including the first number and one with the rest.
   E.g.
     split_input [0; 1; 2; 5; 8; 9;] = [[0; 1; 2]; [5]; [8; 9;]]
   A configuration must then include a subset of each of these sublists,
   so we can reduce the size of the problem enough that the brute force solution above works on each sublist.
*)
let rec split_input = function
    []             -> [[]]
  | x :: []        -> [[x]]
  | x :: y :: rest -> (match split_input (y :: rest) with
        z :: zs -> if y - x = 3 then [x] :: z :: zs else (x :: z) :: zs
      | [] -> [[]])

let () =
  print_string "Day 10-1: The product of 1- and 3- jolt differences is ";
  input |> count_differences |> output_result |> print_int;
  print_newline ();
  print_string "Day 10-2: The number of possible arrangements is ";
  (0 :: input) |> split_input |> List.map (fun l -> possible_subsets_hd l |> List.length) |> List.fold_left ( * ) 1 |> print_int;
  print_newline ()
