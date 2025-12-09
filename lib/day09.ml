open! Core

let pair_of_string s =
  match String.split s ~on:',' with
    | [a ; b] -> (int_of_string a, int_of_string b)
    | _       -> invalid_arg "Couldn't parse pair"

let rec pairs a =
  match a with
    | []       -> []
    | hd :: tl -> List.map tl ~f:(fun a -> (hd, a)) |> List.append (pairs tl)

let part1 points =
  let open List.Monad_infix in
  let areas = pairs points >>| (fun ((x1, y1), (x2, y2)) -> (Int.abs (x1 - x2) + 1) * (Int.abs (y1 - y2) + 1)) in
  List.max_elt areas ~compare:Int.compare
  |> Option.value_exn

let parse_input input =
  let open List.Monad_infix in
  String.strip input
  |> String.split_lines
  >>| pair_of_string

let solve input =
  let points = parse_input input in
  printf "%i\n" (part1 points)
