open Aoc.Elves_password

let count_matching_passwords lb ub =
  let rec loop acc i =
    if i = ub then acc
    else if password_matches_criteria i then loop (acc + 1) (i + 1)
    else loop acc (i + 1)
  in
  loop 0 lb

let () =
  let matches = count_matching_passwords 271973 785961 in
  Stdio.printf "Matching passwords: %d\n" matches
