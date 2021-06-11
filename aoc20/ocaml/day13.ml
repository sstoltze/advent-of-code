let parse_input s =
  s
  |> String.trim
  |> String.split_on_char '\n'
  |> (function
      | [time; timetable] -> (int_of_string time, String.split_on_char ',' timetable)
      | _ -> raise Exit)

let input = CCIO.(with_in "../input/day13.txt" read_all) |> parse_input


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

type congruence = { congruent: Z.t; modulo: Z.t; }

let rec make_congruence remainder modulo =
  if Z.lt remainder Z.zero
  then make_congruence (Z.add remainder modulo) modulo
  else { congruent = Z.rem remainder modulo; modulo = modulo }

let congruences_of_timetable timetable =
  let rec congruences res count = function
    | [] -> res
    | ("x" :: xs) -> congruences res (Z.succ count) xs
    | x :: xs -> congruences (make_congruence (Z.neg count) (Z.of_string x) :: res) (Z.succ count) xs
  in congruences [] Z.zero timetable

let congruences = congruences_of_timetable (snd input)

let rec chinese_remainder =
  let combine_congruences x1 x2 =
    let (_, m1, m2) = Z.gcdext x1.modulo x2.modulo
    in let new_cong = Z.(x1.congruent * x2.modulo * m2 + x2.congruent * x1.modulo * m1)
    and new_mod = Z.(x1.modulo * x2.modulo)
    in make_congruence new_cong new_mod
  in function
    | [] -> raise Exit
    | cong :: [] -> cong.congruent
    | x1 :: x2 :: xs -> chinese_remainder ((combine_congruences x1 x2) :: xs)

(* Very inefficient *)
let part_two = congruences |> chinese_remainder

let () =
  print_string "Day 13-1: Earliest wait time multiplied by bus id is ";
  input |> part_one |> print_int;
  print_newline ();
  print_string "Day 13-2: The earliest timestamp is ";
  print_string (Z.to_string part_two);
  print_newline ()
