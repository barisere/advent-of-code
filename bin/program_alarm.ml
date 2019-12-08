open Core
open Aoc.Program_alarm
;;

let replace_input_values i1 i2 (p: intcode_program): intcode_program =
  let () = p.(1) <- i1; p.(2) <- i2 in
  p
;;

let input_program_from_file =
  let file_arg =
    if Array.length Sys.argv <> 2
    then failwith "requires exactly one argument for the input data file name"
    else Sys.argv.(1)
  in
  let file_name = Filename.realpath file_arg in
  In_channel.(with_file file_name ~f:input_line)
  |> Option.value ~default:""
;;

let part_one () =
  input_program_from_file
  |> intcode_program_of_string
  |> replace_input_values 12 2
  |> eval_intcode_program
  |> Fn.flip Array.get 0
  |> Int.to_string
  |> Stdio.print_endline
;;

let part_two () =
  let program = input_program_from_file |> intcode_program_of_string in
  let inputs = Array.init ~f:Fn.id 100 in
  let inputs = Array.cartesian_product inputs inputs in
  let input_pair = Array.find inputs ~f:(fun (x, y) ->
      let first_val =
        replace_input_values x y program
        |> eval_intcode_program
        |> Fn.flip Array.get 0
      in
      first_val = 19690720)
  in
  match input_pair with
  | Some(noun, verb) -> begin
      Printf.printf "Noun:\t%d\nVerb:\t%d\n" noun verb;
      Printf.printf "100 * noun + verb = %d" (100 * noun + verb)
    end
  | None -> Stdio.print_string "No result"
;;

let () =
  Printf.printf "Part one\n";
  part_one ();
  Printf.printf "\nPart two\n";
  part_two ();
  Stdio.print_endline "";
;;

