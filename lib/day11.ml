(* Useful observation: the graph is a DAG *)
open! Core

module DP = Map.Make(String)

let parse_line line = match String.strip line |> String.split ~on:' ' with
  | []       -> invalid_arg "Couldn't parse line"
  | hd :: tl -> (String.drop_suffix hd 1, tl)

let rec parse_input_aux m = function
  | []       -> m
  | hd :: tl ->
    let (src, dsts) = parse_line hd in
    parse_input_aux (List.fold dsts ~init:m ~f:(fun acc dst -> Map.add_multi acc ~key:dst ~data:src)) tl

let parse_input input = parse_input_aux (Map.empty (module String)) (String.strip input |> String.split_lines)

let rec part1_dp dp g node =
  match Map.find dp node with
    | Some n -> (dp, n)
    | None   ->
      let (dp, n) = List.fold
        (Map.find g node |> Option.value ~default:[])
        ~init:(dp, 0)
        ~f:(fun (dp', n') prev -> let (dp'', n'') = part1_dp dp' g prev in (dp'', n' + n'')) in
      (Map.set dp ~key:node ~data:n, n)

let part1 transposed_graph =
  part1_dp (Map.empty (module String) |> Map.set ~key:"you" ~data:1) transposed_graph "out"
  |> snd

let solve input =
  let transposed_graph = parse_input input in
  printf "%i\n" (part1 transposed_graph)
