let parse_input lines =
  let rec parse_contains = function
      [] -> []
    | number :: adjective :: colour :: _ :: contains -> ((adjective ^ " " ^ colour), int_of_string number) :: (parse_contains contains)
    | _ -> [("", 0)]
  and parse_line = function
      adjective :: colour :: _ :: _ :: contains -> (adjective ^ " " ^ colour, parse_contains contains)
    | _ -> ("", [])
  in lines |> List.map (String.split_on_char ' ') |> List.map parse_line

let input = CCIO.(with_in "../input/day7.txt" read_lines_l) |> parse_input

module StringSet = Set.Make(String)

let print_string_set s =
  print_string "{ ";
  StringSet.iter (fun str -> print_string "\""; print_string str; print_string "\" ") s;
  print_string "}"

let contained_in bag_list bag =
  let scan_bag res (colour, contains) =
    if contains |> List.assoc_opt bag |> Option.is_some
    then StringSet.add colour res
    else res
  in List.fold_left scan_bag StringSet.empty bag_list

let all_contained_in bag_list bag =
  let rec all_contained_in' res =
    let res' = StringSet.fold (fun colour result -> contained_in bag_list colour |> StringSet.union result) res res
    in if res = res' then res else all_contained_in' res'
  in all_contained_in' (contained_in bag_list bag)

let set_count s = s |> StringSet.elements |> List.length

let count_contains bag_list bag =
  (* This counts the containing bag as well, so we subtract one to not count the outermost bag *)
  let rec count_contains' b =
    match List.assoc_opt b bag_list with
      Some l -> 1 + List.fold_left (fun res (colour, count) -> res + (count * count_contains' colour)) 0 l
    | None -> 0
  in count_contains' bag - 1


let test_input = "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags." |> String.split_on_char '\n' |> parse_input

let test_input2 = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags." |> String.split_on_char '\n' |> parse_input

let () =
  print_string "Day 7-1: The number of different colours a shiny gold bag can be contained in is ";
  all_contained_in input "shiny gold" |> set_count |> print_int;
  print_newline ();
  print_string "Day 7-2: The number of bags contained in a shiny gold bag is ";
  count_contains input "shiny gold" |> print_int;
  print_newline ()
