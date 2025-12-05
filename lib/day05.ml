open! Core

type range = {
  lo : int;
  hi : int;
}

type merge_error = LO | HI

let contains { lo ; hi } n = n >= lo && n <= hi

let num_elements { lo ; hi } = hi - lo + 1

let merge_ranges { lo = alo ; hi = ahi } { lo = blo ; hi = bhi } =
  if bhi < alo then
    Error LO
  else if blo > ahi then
    Error HI
  else
    Ok { lo = min alo blo ; hi = max ahi bhi }

let rec add_range ranges r =
  match ranges with
    | [] -> [r]
    | hd :: tl ->
      match merge_ranges hd r with
        | Error LO -> r :: hd :: tl
        | Error HI -> hd :: add_range tl r
        | Ok nr    -> add_range tl nr

let part1 database queries = List.count queries ~f:(fun q -> List.exists database ~f:(Fun.flip contains q))

let part2 database =
  List.fold database ~init:[] ~f:add_range
  |> List.sum (module Int) ~f:num_elements

let range_of_string line =
  match String.split line ~on:'-' with
    | a :: b :: [] -> { lo = int_of_string a ; hi = int_of_string b }
    | _            -> invalid_arg "Couldn't parse range"

let parse_input input =
  let open List.Monad_infix in
  let chunks = String.strip input
    |> String.substr_replace_first ~pattern:"\n\n" ~with_:";"
    |> String.split ~on:';' in
  match chunks with
    | a :: b :: [] -> (String.split_lines a >>| range_of_string, String.split_lines b >>| int_of_string)
    | _            -> invalid_arg "Couldn't parse input"

let solve input =
  let (database, queries) = parse_input input in
  printf "%i %i\n" (part1 database queries) (part2 database)
