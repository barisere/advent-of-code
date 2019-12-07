let required_fuel mass = mass / 3 - 2;;

let rec required_fuel_accounting_for_fuel_mass ?(acc = 0) mass =
  let r = required_fuel mass in
  if r <= 0 then acc else required_fuel_accounting_for_fuel_mass ~acc:(r + acc) r
;;
