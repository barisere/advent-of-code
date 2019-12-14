open Aoc.Crossed_wires
open Core

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command = Command.basic ~summary:"Compute Manhattan distance with inputs from file"
    (Command.Param.map filename_param ~f:(fun filename ->
         fun () ->
           let f = Filename.realpath filename in
           In_channel.with_file f ~f:(fun file ->
               let direction_codes =
                 In_channel.input_lines file
                 |> List.map ~f:(String.split ~on:',')
                 |> List.map ~f:(List.map ~f:direction_code_of_string)
               in
               let gs = direction_codes |> List.map ~f:grid_of_direction_codes in
               let steps_to_first_intersection = find_first_intersection
                   (List.nth_exn direction_codes 0)
                   (List.nth_exn direction_codes 1)
               in
               match gs with
               | g1::g2::_ ->
                 begin match Grid.manhattan_distance g1 g2 with
                   | Some d -> Printf.printf "distance to closest intersection: %d\n" d;
                     Printf.printf "distance to first intersection: %d\n"
                       steps_to_first_intersection
                   | None -> Stdio.print_endline "no distance"
                 end
               | _ -> Stdio.prerr_endline "no enough grids"
             )))

let () = Command.run ~version:"0.1" ~build_info:"AoC" command
