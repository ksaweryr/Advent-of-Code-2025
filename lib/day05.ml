open! Core

type range = {
  lo : int;
  hi : int;
}

let contains { lo ; hi } n = n >= lo && n <= hi

let part1 database queries = List.count queries ~f:(fun q -> List.exists database ~f:(Fun.flip contains q))

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
  printf "%i\n" (part1 database queries)
