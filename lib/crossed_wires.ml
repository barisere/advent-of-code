open Core

let pos_range start stop = List.range ~start:`inclusive ~stop:`inclusive start stop
let neg_range start stop = List.range ~start:`inclusive ~stop:`inclusive ~stride:(-1)
    start stop

module type Grid_inf = sig
  type t
  val origin: int * int
  val create: unit -> t
  val add_point: t -> (int * int) -> t
  val intersection: t -> t -> (int * int) list
  val manhattan_distance: t -> t -> int option
end

module Grid: Grid_inf = struct
  type r = Int.Set.t
  type t = (int, r) Hashtbl.t

  let origin = (1,1)

  let create () = Hashtbl.create (module Int)

  let add_point grid (x, y) =
    let s = Hashtbl.find_or_add grid ~default:(fun () -> Int.Set.empty) x in
    let s = Int.Set.add s y in
    ignore (Hashtbl.set ~key:x ~data:s grid); grid

  let intersection grid_a grid_b =
    let int_pairs = List.fold (Hashtbl.keys grid_a) ~init:[] ~f:(fun acc key ->
        let set = Hashtbl.find grid_a key in
        let set' = Hashtbl.find grid_b key in
        let ys = Option.map2 set set' ~f:Int.Set.inter
                 |> Option.value ~default:Int.Set.empty
                 |> Int.Set.to_list
        in
        List.append (List.map ys ~f:(fun y -> (key, y))) acc (*  *)
      )
    in
    List.sort int_pairs ~compare:(fun (x, y) (x', y') ->
        Int.(compare (abs x + abs y) (abs x' + abs y')))

  let manhattan_distance grid_a grid_b =
    match (intersection grid_a grid_b, origin) with
    | (x,y)::_, (x0,y0) -> Some (Int.abs (x - x0) + Int.abs (y - y0))
    | _ -> None
end

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

let grid_of_direction_codes (codes: direction_code list) =
  let grid = Grid.create () in
  let (x0, y0) = Grid.origin in
  let x = ref x0 in
  let y = ref y0 in
  List.fold codes ~init:grid ~f:(fun acc code ->
      match code with
      | ('L', d) -> List.fold ~init:acc ~f:(fun acc i -> x := i; Grid.add_point acc (!x,!y))
                      (neg_range (!x - 1) (!x - d))
      | ('R', d) -> List.fold ~init:acc ~f:(fun acc i -> x := i; Grid.add_point acc (!x,!y))
                      (pos_range (!x + 1) (!x + d))
      | ('U', d) -> List.fold ~init:acc ~f:(fun acc i -> y := i; Grid.add_point acc (!x,!y))
                      (pos_range (!y + 1) (!y + d))
      | ('D', d) -> List.fold ~init:acc ~f:(fun acc i -> y := i; Grid.add_point acc (!x,!y))
                      (neg_range (!y - 1) (!y - d))
      | _ -> acc)

(* The time complexity of this function is poor. If I figure out a better
representation for the data, I could improve this function. *)
let find_first_intersection first_wire second_wire =
  let next_points (x, y) = function
    | ('L', d) -> neg_range (x - 1) (x - d), List.init d ~f:(Fn.const y)
    | ('R', d) -> pos_range (x + 1) (x + d), List.init d ~f:(Fn.const y)
    | ('D', d) -> List.init d ~f:(Fn.const x), neg_range (y - 1) (y - d)
    | ('U', d) -> List.init d ~f:(Fn.const x), pos_range (y + 1) (y + d)
    | _ -> [], []
  in
  let rec build_points wire1 wire2 points1 points2 (x, y) (x', y') =
    match wire1, wire2 with
    | h::t, [] ->
      let (next_xs, next_ys) = next_points (x, y) h in
      let points1' = points1 @ List.zip_exn next_xs next_ys in
      build_points t [] points1' points2 (List.last_exn next_xs, List.last_exn next_ys) (x', y')
    | [], h'::t' ->
      let (next_xs', next_ys') = next_points (x', y') h' in
      let points2' = points2 @ List.zip_exn next_xs' next_ys' in
      build_points [] t' points1 points2' (x',y') (List.last_exn next_xs', List.last_exn next_ys')
    | h::t, h'::t' ->
      let (next_xs, next_ys) = next_points (x, y) h in
      let (next_xs', next_ys') = next_points (x', y') h' in
      let points1' = points1 @ List.zip_exn next_xs next_ys in
      let points2' = points2 @ List.zip_exn next_xs' next_ys' in
      build_points t t' points1' points2'
        (List.last_exn next_xs, List.last_exn next_ys)
        (List.last_exn next_xs', List.last_exn next_ys')
    | _, _ -> points1, points2
  in
  let g, g' = grid_of_direction_codes first_wire, grid_of_direction_codes second_wire in
  let intersections = Grid.intersection g g' in
  let (p, p') = build_points first_wire second_wire [] [] Grid.origin Grid.origin in
  let distances = List.map intersections ~f:(fun (x, y) ->
      let d = List.findi p ~f:(fun _ (x', y') -> x = x' && y = y') in
      let d' = List.findi p' ~f:(fun _ (x', y') -> x = x' && y = y') in
      Option.map2 d d' ~f:(fun (i, _) (i', _) -> 2 + i + i')
      |> Option.value ~default:Int.max_value
    )
  in
  List.fold2_exn intersections distances ~init:Int.max_value ~f:(fun i _ d -> Int.min i d)
