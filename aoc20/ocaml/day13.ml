let input =
  CCIO.(with_in "../input/day13.txt" read_all)
  |> String.trim
  |> String.split_on_char '\n'
  |> (function
        [time; timetable] -> (int_of_string time, String.split_on_char ',' timetable)
      | _ -> raise Exit)

let wait_time current_time bus_id =
  (* The bus runs every bus_id minutes, so the remainder after division
   * is how long we've waited since the last bus.
   * So the remaining wait time is the id - the remainder*)
  bus_id - (current_time mod bus_id)

let sort_by_wait_time current_time =
  List.sort (fun id_a id_b ->
      compare
        (wait_time current_time id_a)
        (wait_time current_time id_b))

let bus_ids timetable = List.filter_map int_of_string_opt timetable

let part_one (current_time, timetable) =
  let first_part_calc current_time bus_id = (wait_time current_time bus_id) * bus_id
  in timetable
     |> bus_ids
     |> sort_by_wait_time current_time
     |> List.hd
     |> first_part_calc current_time

let () =
  print_string "Day 13-1: Earliest wait time multiplied by bus id is ";
  input |> part_one |> print_int;
  print_newline ()
