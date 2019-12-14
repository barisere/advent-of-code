open Base
open Core
open OUnit2
open Aoc.Crossed_wires

type code_input = string * string

let direction_code_of_input = function
  | a -> List.map (String.split ~on:',' a) ~f:direction_code_of_string

let first_wire = "U62,R66,U55,R34,D71,R55,D58,R83"
let second_wire = "R75,D30,R83,U83,L12,D49,R71,U7,L72"

let first_wire_code: direction_code list = [
  ('U', 62); ('R', 66); ('U', 55); ('R', 34); ('D', 71); ('R', 55); ('D', 58); ('R', 83)
]

let test_parse_direction_codes line_input expected_code _ =
  let direction_code = direction_code_of_input line_input in
  let cmp = List.equal (fun (c, d) (c', d') -> Char.(c = c') && d = d') in
  assert_equal
    ~msg:"Parsed direction codes should be tuples of the direcion label and the move steps"
    ~cmp
    expected_code
    direction_code

let test_manhattan_distance wire_code_1 wire_code_2 expected_distance _ =
  let first_wire_code = direction_code_of_input wire_code_1 in
  let second_wire_code = direction_code_of_input wire_code_2 in
  let moves = create_moves first_wire_code in
  let moves' = create_moves second_wire_code in
  let distances = List.map (find_intersections moves moves') ~f:(fun (x,y,_) ->
      manhattan_distance (x,y) coordinate_origin) |> List.sort ~compare:Int.compare
  in
  match distances with
  | d::_ -> assert_equal
                ~msg:(Printf.sprintf "Expected manhattan distance %d, got %d" expected_distance d)
                expected_distance d
  | [] -> OUnitAssert.assert_failure "Got None for manhattan distance"

let parse_suite = "Computing intersection point of wires closes to a central point" >::: [
    "Parsing direction codes maps each code to a tuple (direction label * displacement)" >::
    test_parse_direction_codes first_wire first_wire_code;
  ]

let manhattan_distance_suite = "Manhattan distance on a grid" >::: [
    "Example 1" >:: test_manhattan_distance "R8,U5,L5,D3" "U7,R6,D4,L4" 6;

    "Example 2" >:: test_manhattan_distance
      "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      "U62,R66,U55,R34,D71,R55,D58,R83"
      159;

    "Example 3" >:: test_manhattan_distance
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      135
  ]

let test_first_intersection first_wire second_wire expected_moves _ =
  let first_code = direction_code_of_input first_wire in
  let second_code = direction_code_of_input second_wire in
  let n_moves = find_intersections (create_moves first_code) (create_moves second_code) in
  match n_moves with
  | (_, _, n)::_ ->
    assert_equal n expected_moves ~msg:
      "Intersection found after unexpected number of moves"
  | _ -> OUnitAssert.assert_failure "Intersection found after unexpected number of moves"

let intersection_suite =
  "Finding the fewest number of moves until wires intersect" >::: [
    "Example 1" >:: test_first_intersection
      "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      "U62,R66,U55,R34,D71,R55,D58,R83"
      610;

    "Example 2" >:: test_first_intersection
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      410;

    "Example 3" >:: test_first_intersection
      "R8,U5,L5,D3" "U7,R6,D4,L4" 30
  ]
let () =
  run_test_tt_main parse_suite;
  run_test_tt_main manhattan_distance_suite;
  run_test_tt_main intersection_suite
