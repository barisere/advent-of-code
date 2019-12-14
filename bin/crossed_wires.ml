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
               let line1 = In_channel.input_line_exn file |> String.split ~on:',' in
               let line2 = In_channel.input_line_exn file |> String.split ~on:',' in
               let direction_codes1 = List.map line1 ~f:direction_code_of_string in
               let direction_codes2 = List.map line2 ~f:direction_code_of_string in
               let intersections = find_intersections (create_moves direction_codes1) (create_moves direction_codes2) in
               let distances = List.map intersections ~f:(fun (x, y, _) -> manhattan_distance (x, y) coordinate_origin)
                               |> List.sort ~compare:Int.compare
               in
               let steps_to_first_intersection = match intersections with
                 | (_, _, n)::_ -> n
                 | _ -> 0
               in
               match distances with
               | d::_ -> Printf.printf "distance to closest intersection: %d\n" d;
                 Printf.printf "distance to first intersection: %d\n"
                   steps_to_first_intersection
               | [] -> Stdio.print_endline "no distance"
             )))

let () = Command.run ~version:"0.1" ~build_info:"AoC" command
