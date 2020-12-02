type policy = { min : int; max : int; character : char }

let parse_policy s =
  let array_to_policy = (function
        count :: c :: password :: [] -> (match String.split_on_char '-' count with
            [min; max] -> ({ min = int_of_string min; max = int_of_string max; character = String.get c 0}, password)
          | _ -> exit 1)

      | _ -> exit 1
    )
  in String.split_on_char ' ' s |> array_to_policy

let sled_rental_password (policy, password) =
  let amount = (String.split_on_char policy.character password |> List.length) - 1
  in policy.min <= amount && amount <= policy.max

let toboggan_rental_password (policy, password) =
  let amount = [String.get password (policy.min - 1); String.get password (policy.max - 1)]
               |> List.filter (fun c -> c = policy.character)
               |> List.length
  in amount == 1

let input = CCIO.(with_in "../input/day2.txt" read_lines_l) |> List.map parse_policy

let () =
  print_string "Day 02-1: There are ";
  input |> List.filter sled_rental_password |> List.length |> print_int;
  print_string " valid passwords according to the sled rental rules.";
  print_newline();
  print_string "Day 02-2: There are ";
  input |> List.filter toboggan_rental_password |> List.length |> print_int;
  print_string " valid passwords according to the toboggan rental rules.";
  print_newline();
