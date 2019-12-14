open Core

let coordinate_origin = (0, 0)

let manhattan_distance (x, y) (x0, y0) = (Int.abs (x - x0) + Int.abs (y - y0))

type direction_code = char * int

let direction_code_of_string (code: string): direction_code =
  match code.[0] with
  | ('L' | 'R' | 'U' | 'D') as d ->
    let n = String.to_list code
            |> Fn.flip List.drop 1
            |> String.of_char_list
            |> Int.of_string
    in
    (d, Int.abs n)
  | _ -> raise (Invalid_argument code)

type point = int * int

type line = point * point

type move = { line: line; distance_from_origin: int }

let create_moves (codes: direction_code list) =
  let make_next_move (code: direction_code) (last_move: move) =
    let { line; distance_from_origin } = last_move in
    let d' = distance_from_origin in
    let (_, (x2, y2)) = line in
    match code with
    | ('L', d) -> { line = ((x2, y2), (x2 - d, y2)); distance_from_origin = d' + d}
    | ('R', d) -> { line = ((x2, y2), (x2 + d, y2)); distance_from_origin = d' + d}
    | ('D', d) -> { line = ((x2, y2), (x2, y2 - d)); distance_from_origin = d' + d}
    | ('U', d) -> { line = ((x2, y2), (x2, y2 + d)); distance_from_origin = d' + d}
    | _ -> last_move
  in
  List.fold codes ~init:[{ line = (coordinate_origin, coordinate_origin); distance_from_origin = 0 }]
    ~f:(fun moves code ->
        match moves with
        | last_move::_ -> (make_next_move code last_move)::moves
        | [] -> moves
      )

let find_intersections (first_line: move list) (second_line: move list) =
  let in_range x start stop = List.exists ~f:(fun a -> a = x)
      (List.range ~start:`inclusive ~stop:`inclusive (Int.min start stop) (Int.max start stop)) in
  List.fold_right first_line ~init:[] ~f:(fun { line; distance_from_origin } acc ->
      let ((x1, y1), (x2, y2)) = line in
      let d = distance_from_origin in
      List.fold_right second_line ~init:acc ~f:(fun { line; distance_from_origin } acc ->
          let ((x1', y1'), (x2', y2')) = line in
          if x1 = x2 && in_range x1 x1' x2' && in_range y1' y1 y2 then
            (* (x1, y1), (x2, y2) is vertical *)
            let (x, y) = (x1, y1') in
            let d' = (d - Int.abs (y2 - y)) + (distance_from_origin - Int.abs (x2' - x)) in
            (x, y, d')::acc
          else if y2 = y1 && in_range y1 y1' y2' && in_range x1' x1 x2 then
            (* (x1, y1), (x2, y2) is horizontal *)
            let (x, y) = (x1', y1) in
            let d' = (d - Int.abs (x2 - x)) + (distance_from_origin - Int.abs (y2' - y)) in
            (x, y, d')::acc
          else acc
        )
    )
  |> List.filter ~f:(fun (x, y, a) -> a > 0 && (x,y) <> coordinate_origin)
  |> List.sort ~compare:(fun (_, _, a) (_, _, b) -> Int.compare a b)
