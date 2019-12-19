let password_matches_criteria p =
  let repeats = Hashtbl.create 3 in
  let rec test_criteria (last_digit) n =
    if n = 0 then
      Hashtbl.fold (fun _ v acc -> v = 2 || acc) repeats false
    else
      let current_digit = Int.rem n 10 in
      match Int.compare current_digit last_digit with
      | -1 -> test_criteria (current_digit) (n / 10)
      | 0 -> begin match Hashtbl.find_opt repeats current_digit with
          | Some v -> Hashtbl.remove repeats current_digit;
            Hashtbl.add repeats current_digit (v + 1)
          | None -> Hashtbl.add repeats current_digit 2
        end;
        test_criteria (current_digit) (n / 10)
      | _ -> false
  in
  test_criteria (Int.max_int) p
