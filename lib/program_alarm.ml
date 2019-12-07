open Base
;;

type intcode = Add of (int * int) | Mult of (int * int)
;;

type intcode_program = int array
;;

let intcode_program_of_string (p: string): intcode_program =
  String.split ~on:',' p
  |> Array.of_list
  |> Array.map ~f:Int.of_string
;;

let eval_intcode_program (p: intcode_program): intcode_program =
  let length = Array.length p in
  let p' = Array.copy p in
  let idx = ref 0 in
  let () = while length - !idx >= 4 do
      let () = match (p'.(!idx), p'.(!idx + 3)) with
        | (99, _) -> ()
        | (1, n) when n < length -> p'.(n) <- p'.(p'.(!idx + 1)) + p'.(p'.(!idx + 2))
        | (2, n) when n < length -> p'.(n) <- p'.(p'.(!idx + 1)) * p'.(p'.(!idx + 2))
        | _ -> ();
      in
      idx := !idx + 4
    done
  in
  p'
;;

let string_of_intcode_program (r: intcode_program): string =
  Array.fold ~init:""
    ~f:(fun acc v -> if String.(acc = "") then Int.to_string v else acc ^ "," ^ Int.to_string v) r
;;

