open Core
open Aoc.Rocket_equation

let () =
  let file_arg =
    if Array.length Sys.argv <> 2
    then failwith "requires exactly one argument for the input data file name"
    else Sys.argv.(1)
  in
  let file_name = Filename.realpath file_arg in
  let sum = ref 0 in
  let sum_with_fuel_mass_considered = ref 0 in
  let incr_sum_from_string num_string =
    let n = num_string |> int_of_string in
    sum := n |> required_fuel |> (+) !sum;
    sum_with_fuel_mass_considered :=
      n
      |> required_fuel_accounting_for_fuel_mass
      |> (+) !sum_with_fuel_mass_considered;
  in
  In_channel.(with_file file_name~f:(fun ch ->
      iter_lines ch ~f:incr_sum_from_string));
  print_endline ("Part one: " ^ string_of_int !sum);
  print_endline ("Part two: " ^ string_of_int !sum_with_fuel_mass_considered)
;;

