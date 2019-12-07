open Core
open OUnit2
open Aoc.Program_alarm

type test_table = {
  input: string;
  expected: string;
}

let test_cases: test_table list = [
  {
    input = "1,9,10,3,2,3,11,0,99,30,40,50";
    expected = "3500,9,10,70,2,3,11,0,99,30,40,50"
  };
  { input = "1,0,0,0,99"; expected = "2,0,0,0,99" };
  { input = "2,4,4,5,99,0"; expected = "2,4,4,5,99,9801"};
  { input = "1,1,1,4,99,5,6,0,99"; expected = "30,1,1,4,2,5,6,0,99"}
]

let test_fn (tc: test_table) _ =
  let result =
    tc.input
    |> intcode_program_of_string
    |> eval_intcode_program
    |> string_of_intcode_program
  in
  assert_equal ~msg:(Printf.sprintf "%s should equal %s" result tc.expected) result tc.expected

let suite = "Running Intcode programs" >::: List.mapi
              ~f:(fun i t -> Printf.sprintf "Test case %d: %s" i t.input >:: test_fn t)
              test_cases

let () = run_test_tt_main suite
