open Base
open OUnit2
open Aoc.Elves_password

let test_password_matches_elves_criteria p expected _ =
  assert_equal ~msg:(Printf.sprintf "%d does not match the criteria" p) expected (password_matches_criteria p)

let test_table = [(111111, false); (223450, false); (123789, false); (122345, true); (123444, false)]

let suite = "Rules for Elves password" >::: List.map test_table ~f:(fun (p, expected) ->
    Printf.sprintf "Expect %B for %d" expected p >:: test_password_matches_elves_criteria p expected)

let () = run_test_tt_main suite

