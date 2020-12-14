type operation = Nop | Acc | Jmp | Err

let op_of_string = function
    "nop" -> Nop
  | "acc" -> Acc
  | "jmp" -> Jmp
  | _     -> Err

type instruction = { op: operation; arg: int }

let error_instruction = { op = Err; arg = 0 }

type program = { instructions: instruction list; pointer: int; accumulator: int; length: int }

let read_program s =
  let read_line l = l
                    |> String.split_on_char ' '
                    |> (function
                          [operation; argument] -> { op = op_of_string operation; arg = int_of_string argument }
                        | _                     -> error_instruction) in
  let instructions = s |> String.trim |> String.split_on_char '\n' |> List.map read_line in
  { instructions; pointer = 0; accumulator = 0; length = List.length instructions }

let test_program = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6" |> read_program

let run_instruction prog { op; arg } =
  let pointer = prog.pointer + 1 in
  match op with
    Nop -> { prog with pointer }
  | Acc -> { prog with pointer; accumulator = prog.accumulator + arg }
  | Jmp -> { prog with pointer = prog.pointer + arg }
  | Err -> raise Exit

let run_program prog =
  let rec run_program' visited p =
    if p.pointer = p.length
    then p
    else
      let new_prog = run_instruction p (List.nth p.instructions p.pointer) in
      if List.mem new_prog.pointer visited
      then new_prog
      else run_program' (new_prog.pointer :: visited) new_prog
  in run_program' [prog.pointer] prog

let modify_instructions prog =
  let modify_instruction inst = match inst.op with
      Nop -> { inst with op = Jmp }
    | Jmp -> { inst with op = Nop }
    | _   -> inst
  in
  let rec modify_instruction_i i = (function
        [] -> []
      | inst :: insts -> if i = 0 then (modify_instruction inst) :: insts else inst :: (modify_instruction_i (i-1) insts))
  in
  let modify_i i = { prog with instructions = modify_instruction_i i prog.instructions}
  in List.init prog.length modify_i

let accumulator { accumulator; _ } = accumulator

let finished { pointer; length; _ } = pointer = length

let input = CCIO.(with_in "../input/day8.txt" read_all) |> read_program

let () =
  print_string "Day 8-1: The accumulator before looping is ";
  input |> run_program |> accumulator |> print_int;
  print_newline ();
  print_string "Day 8-2: The accumulator at termination is ";
  input |> modify_instructions |> List.map run_program  |> List.filter finished |> List.hd |> accumulator |> print_int;
  print_newline ()
