let parse_input s =
  s
  |> String.trim
  |> String.split_on_char '\n'
  |> (function
        [time; timetable] -> (int_of_string time, String.split_on_char ',' timetable)
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

type congruence = { congruent: int; modulo: int; }

let rec make_congruence remainder modulo =
  if remainder < 0
  then make_congruence (remainder + modulo) modulo
  else { congruent = remainder mod modulo; modulo = modulo }

let congruences_of_timetable timetable =
  let rec congruences res count = function
      [] -> res
    | ("x" :: xs) -> congruences res (count + 1) xs
    | x :: xs -> congruences (make_congruence (-count) (int_of_string x) :: res) (count + 1) xs
  in congruences [] 0 timetable

let congruences = congruences_of_timetable (snd input)

let extended_euclidian a b =
  let rec extended' (old_r, r) (old_s, s) (old_t, t) =
    if r = 0
    then (old_s, old_t)
    else let q = old_r / r
         in extended' (r, old_r - q * r) (s, old_s - q * s) (t, old_t - q * t)
  in extended' (a, b) (1, 0) (0, 1)

let gcd a b = let (m1, m2) = extended_euclidian a b in m1 * a + m2 * b

let rec pairwise_gcd =
  let pairwise_gcd' a bs = List.map (fun b -> (a, b, gcd a b)) bs
  in function
    [] -> []
  | x :: xs -> (pairwise_gcd' x xs) @ pairwise_gcd xs

let rec chinese_remainder =
  let combine_congruences x1 x2 =
    let (m1, m2) = extended_euclidian x1.modulo x2.modulo
    in let new_cong = x1.congruent * x2.modulo * m2 + x2.congruent * x1.modulo * m1
       and new_mod = x1.modulo * x2.modulo
       in make_congruence new_cong new_mod
  in function
    [] -> raise Exit
  | cong :: [] -> cong.congruent
  | x1 :: x2 :: xs -> chinese_remainder (combine_congruences x1 x2 :: xs)

(* This is wrong *)
let test = congruences |> chinese_remainder

(* But all of these work... *)
let test_chinese s = s |> parse_input |> snd |> congruences_of_timetable  |> chinese_remainder

let test_1 = "0\n7,13,x,x,59,x,31,19" |> test_chinese
let test_2 = "0\n17,x,13,19" |> test_chinese
let test_3 = "0\n67,7,59,61" |> test_chinese
let test_4 = "0\n67,x,7,59,61" |> test_chinese
let test_5 = "0\n67,7,x,59,61" |> test_chinese
let test_6 = "0\n1789,37,47,1889" |> test_chinese

let () =
  print_string "Day 13-1: Earliest wait time multiplied by bus id is ";
  input |> part_one |> print_int;
  print_newline ()
