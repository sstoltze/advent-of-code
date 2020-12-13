let split_on_string sep str =
  let sep_length = String.length sep in
  let rec split_on_char_list i s =
    let new_index = i + sep_length
    and string_length = String.length s in
    if new_index >= string_length
    then [s;]
    else if sep = String.sub s i sep_length
    then (String.sub s 0 i) :: split_on_char_list 0 (String.sub s new_index (string_length - new_index))
    else split_on_char_list (i+1) s
  in split_on_char_list 0 str

module CharSet = Set.Make(Char)

let line_to_char_set l = l |> String.to_seq |> CharSet.of_seq

let lines_to_char_sets ls = ls |> String.split_on_char '\n' |> List.map line_to_char_set

let input =
  CCIO.(with_in "../input/day6.txt" read_all)
  |> split_on_string "\n\n"
  |> List.map (fun l -> l |> String.trim |> lines_to_char_sets)

let print_char_set s =
  print_string "{";
  CharSet.iter (fun c -> print_char c; print_string ", ") s;
  print_string "}"

let any_yes_answer l = List.fold_left CharSet.union CharSet.empty l

let all_yes_answers l = List.fold_left CharSet.inter (List.hd l) l

let answer_count s = s |> CharSet.elements |> List.length

let () =
  print_string "Day 6-1: The sum of questions with 'yes' answers in each group is ";
  input |> List.map (fun l -> l |> any_yes_answer |> answer_count) |> List.fold_left ( + ) 0 |> print_int;
  print_newline ();
  print_string "Day 6-2: The sum of questions with all 'yes' answers in each group is ";
  input |> List.map (fun l -> l |> all_yes_answers |> answer_count) |> List.fold_left ( + ) 0 |> print_int;
  print_newline ()
