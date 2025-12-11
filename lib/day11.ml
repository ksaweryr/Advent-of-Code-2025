(* Useful observation: the graph is a DAG *)
open! Core

let parse_line line = match String.strip line |> String.split ~on:' ' with
  | []       -> invalid_arg "Couldn't parse line"
  | hd :: tl -> (String.drop_suffix hd 1, tl)

let rec parse_input_aux m = function
  | []       -> m
  | hd :: tl ->
    let (src, dsts) = parse_line hd in
    parse_input_aux (List.fold dsts ~init:m ~f:(fun acc dst -> Map.add_multi acc ~key:dst ~data:src)) tl

let parse_input input = parse_input_aux (Map.empty (module String)) (String.strip input |> String.split_lines)

let rec numpaths_dp dp g node =
  match Map.find dp node with
    | Some n -> (dp, n)
    | None   ->
      let (dp, n) = List.fold
        (Map.find g node |> Option.value ~default:[])
        ~init:(dp, 0)
        ~f:(fun (dp', n') prev -> let (dp'', n'') = numpaths_dp dp' g prev in (dp'', n' + n'')) in
      (Map.set dp ~key:node ~data:n, n)

let numpaths ?(init_num=1) startnode endnode transposed_graph =
  numpaths_dp (Map.empty (module String) |> Map.set ~key:startnode ~data:init_num) transposed_graph endnode
  |> snd

let part1 = numpaths "you" "out" ~init_num:1

let part2 transposed_graph =
  let svr_to_fft = numpaths "svr" "fft" transposed_graph
  and svr_to_dac = numpaths "svr" "dac" transposed_graph in
  let fft_to_dac = numpaths "fft" "dac" transposed_graph ~init_num:svr_to_fft
  and dac_to_fft = numpaths "dac" "fft" transposed_graph ~init_num:svr_to_dac in
  (numpaths "dac" "out" transposed_graph ~init_num:fft_to_dac) + (numpaths "fft" "out" transposed_graph ~init_num:dac_to_fft)

let solve input =
  let transposed_graph = parse_input input in
  printf "%i %i\n" (part1 transposed_graph) (part2 transposed_graph)
