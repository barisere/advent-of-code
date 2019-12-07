open OUnit2
open Aoc.Rocket_equation

let test_required_fuel mass expected_fuel _ =
  assert_equal ~msg:"required_fuel should produce the expected fuel"
    expected_fuel (required_fuel mass)
;;

let test_required_fuel_accounting_for_fuel_mass mass expected_fuel _ =
  assert_equal ~msg:"required_fuel_accounting_for_fuel_mass should produce the expected fuel"
    expected_fuel (required_fuel_accounting_for_fuel_mass mass)
;;

let suite_part_one = "Day 1: The Tyranny of the Rocket Equation (Part One)" >::: [
    "Mass of 12 requires fuel amount 2" >:: test_required_fuel 12 2;
    "Mass of 14 requires fuel amount 2" >:: test_required_fuel 14 2;
    "For a mass of 1969, the fuel required is 654" >:: test_required_fuel 1969 654;
    "For a mass of 100756, the fuel required is 33583" >:: test_required_fuel 100756 33583
  ]
;;

let suite_part_two = "Day 1: The Tyranny of the Rocket Equation (Part One)" >::: [
    "Mass of 12 requires fuel amount 2"
    >:: test_required_fuel_accounting_for_fuel_mass 12 2;
    "Mass of 14 requires fuel amount 2"
    >:: test_required_fuel_accounting_for_fuel_mass 14 2;
    "For a mass of 1969, the fuel required is 966"
    >:: test_required_fuel_accounting_for_fuel_mass 1969 966;
    "For a mass of 100756, the fuel required is 33583"
    >:: test_required_fuel_accounting_for_fuel_mass 100756 50346
  ]
;;

let () =
  run_test_tt_main suite_part_one;
  run_test_tt_main suite_part_two
;;
