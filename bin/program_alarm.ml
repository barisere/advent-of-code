open Core
open Aoc.Program_alarm
;;

let replace_input_values (p: intcode_program): intcode_program =
  let () = p.(1) <- 12; p.(2) <- 2 in
  p
;;


let () =
  let file_arg =
    if Array.length Sys.argv <> 2
    then failwith "requires exactly one argument for the input data file name"
    else Sys.argv.(1)
  in
  let file_name = Filename.realpath file_arg in
  In_channel.(with_file file_name ~f:input_line)
  |> Option.value ~default:""
  |> intcode_program_of_string
  |> replace_input_values
  |> eval_intcode_program
  |> string_of_intcode_program
  |> Stdio.print_endline
;;

