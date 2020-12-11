(* I spent a long time trying to get the parser working with detecting "\n\n", but had to resort to this instead. *)
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

type passport = { byr: string;
                  iyr: string;
                  eyr: string;
                  hgt: string;
                  hcl: string;
                  ecl: string;
                  pid: string;
                  cid: string option}

let list_to_passport = function
    None -> None
  | Some l -> (match ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid"] |> List.map (fun a -> List.assoc_opt a l) with
        [Some byr; Some iyr; Some eyr; Some hgt; Some hcl; Some ecl; Some pid; cid] -> Some {byr; iyr; eyr; hgt; hcl; ecl; pid; cid}
      | _ -> None)

let valid_passport p = Option.is_some p.cid

let filter_some l = List.filter Option.is_some l |> List.map Option.get

let parser str =
  let open Opal in
  let field_parser name = token (name ^ ":") >> many (alpha_num <|> exactly '#') => fun x -> (name, implode x) in
  let field_parsers = List.map field_parser ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid"] |> choice in
  let passport_line_parser = optional spaces >> sep_by1 field_parsers spaces in
  let passport_parser = many passport_line_parser => List.flatten in
  str |> split_on_string "\n\n"  |> List.map LazyStream.of_string |> List.map (parse passport_parser % list_to_passport)

let parse_int digits min max =
  let open Opal in
  let parse_int = count digits digit << eof() => implode % int_of_string in
  fun s -> match (s |> LazyStream.of_string |> parse parse_int) with
      Some n -> (min <= n && n <= max)
    | None   -> false

let valid_byr p = parse_int 4 1920 2002 p.byr

let valid_iyr p = parse_int 4 2010 2020 p.iyr

let valid_eyr p = parse_int 4 2020 2030 p.eyr

let valid_pid p = parse_int 9 0 999999999 p.pid

let valid_ecl p =
  let open Opal in
  let ecl_parser = (token "amb" <|> token "blu" <|> token "brn" <|> token "gry" <|> token "grn" <|> token "hzl" <|> token "oth") << eof () in
  p.ecl |> LazyStream.of_string |> parse ecl_parser |> Option.is_some

let valid_hcl p =
  let open Opal in
  let hcl_parser = exactly '#' >> count 6 (digit <|> one_of ['a'; 'b'; 'c'; 'd'; 'e'; 'f';]) << eof () in
  p.hcl |> LazyStream.of_string |> parse hcl_parser |> Option.is_some

let valid_hgt p =
  let open Opal in
  let hgt_parser = many digit >>= fun n -> (token "cm" <|> token "in") => fun units -> (implode n |> int_of_string, units) in
  match p.hgt |> LazyStream.of_string |> parse hgt_parser with
    Some (n, "cm") -> 150 <= n && n <= 193
  | Some (n, "in") -> 59 <= n && n <= 76
  | _ -> false

let strict_validation p = List.for_all (fun f -> f p) [valid_byr; valid_iyr; valid_eyr; valid_pid; valid_ecl; valid_hcl; valid_hgt;]

let input = CCIO.(with_in "../input/day4.txt" read_all) |> parser

let () =
  print_string "Day 4-1: The number of valid passports: ";
  let valid_passports = input |> filter_some  |> List.length in
  print_int valid_passports;
  print_newline ();
  print_string "Day 4-2: The number of stricter passports: ";
  let strict_passports = input |> filter_some |> List.filter strict_validation |> List.length in print_int strict_passports;
  print_newline ()
