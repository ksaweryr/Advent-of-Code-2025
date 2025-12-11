open! Core

type problem = {
  target : int list;
  switches : int list list;
  joltages : int list;
}

let list_xor a b =
  let open List.Monad_infix in
  List.zip_exn a b
  >>| (fun (aa, bb) -> Int.bit_xor aa bb)

let is_zero = List.for_all ~f:(fun x -> x = 0)

let rec apply_bitmask target bm switches =
  match switches with
    | []       ->
      if bm = 0 then
        target
      else
        invalid_arg "No more switches but bitmask is not empty!"
    | hd :: tl ->
      if bm % 2 = 0 then
        apply_bitmask target (bm / 2) tl
      else
        apply_bitmask (list_xor target hd) (bm / 2) tl

let bitmask_possible { target ; switches ; _ } bm =
  apply_bitmask target bm switches
  |> is_zero

let rec bits_set a =
  if a = 0 then
    0
  else
    (a % 2) + bits_set (a / 2)

let part1_single ({ switches ; _ } as problem) =
  List.init (Int.shift_left 1 (List.length switches)) ~f:Fun.id
  |> List.filter ~f:(bitmask_possible problem)
  |> List.map ~f:bits_set
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let part1 = List.sum (module Int) ~f:part1_single

let part2_single { switches ; joltages ; _ } =
  let open Lp in
  let open List.Monad_infix in
  let max_joltage = List.max_elt joltages ~compare:Int.compare |> Option.value_exn in
  let vars = List.init (List.length switches) ~f:(fun i -> var (sprintf "x_%02i" i) ~integer:true ~lb:(Float.of_int 0) ~ub:(Float.of_int max_joltage)) in
  let obj = minimize (List.fold vars ~init:(c 0.0) ~f:(++))
  and cstrs =
    List.zip_exn switches vars
    >>| (fun (sw, v) -> sw >>| (fun b -> c (Float.of_int b) *~ v))
    |> List.fold ~init:(List.init (List.length joltages) ~f:(fun _ -> c 0.0)) ~f:(fun acc p -> List.zip_exn acc p >>| (fun (a, pp) -> a ++ pp))
    |> List.zip_exn joltages
    >>| (fun (j, e) -> (c (Float.of_int j)) =~ e) in
  match make obj cstrs |> Lp_glpk.solve with
    | Ok (obj, _) -> Float.round_nearest obj |> Int.of_float
    | Error msg   -> invalid_arg msg

let part2 = List.sum (module Int) ~f:part2_single

let parse_target s =
  let open List.Monad_infix in
  String.drop_prefix s 1
  |> Fun.flip String.drop_suffix 1
  |> String.to_list
  >>| (function
    | '.' -> 0
    | '#' -> 1
    | _   -> invalid_arg "Couldn't parse target")

let parse_switch num_lights s =
  let open List.Monad_infix in
  let positions = String.drop_prefix s 1
    |> Fun.flip String.drop_suffix 1
    |> String.split ~on:','
    >>| Int.of_string in
  List.init num_lights ~f:(fun i -> match List.find positions ~f:(fun j -> i = j) with
    | Some _ -> 1
    | _      -> 0)

let parse_joltages s =
  let open List.Monad_infix in
  String.drop_prefix s 1
  |> Fun.flip String.drop_suffix 1
  |> String.split ~on:','
  >>| Int.of_string

let rec problem_of_string_aux n = function
  | []       -> { target = [] ; switches = [] ; joltages = [] }
  | [hd]     -> { target = [] ; switches = [] ; joltages = parse_joltages hd }
  | hd :: tl ->
    let { target ; switches ; joltages } = problem_of_string_aux n tl in
    { target ; switches = parse_switch n hd :: switches ; joltages }

let problem_of_string s =
  match String.strip s |> String.split ~on:' ' with
    | []       -> invalid_arg "Couldn't parse problem"
    | hd :: tl ->
      let target = parse_target hd in
      let { switches ; joltages ; _ } = problem_of_string_aux (List.length target) tl in
      { target ; switches ; joltages }

let parse_input input =
  let open List.Monad_infix in
  String.strip input
  |> String.split_lines
  >>| problem_of_string

let solve input =
  let problems = parse_input input in
  printf "%i %i\n" (part1 problems) (part2 problems)
