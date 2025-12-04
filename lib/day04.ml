open! Core

type cell = Floor | Paper

let safe_get a idx = try Some a.(idx) with _ -> None

let reachable grid (x, y) = match grid.(y).(x) with
  | Floor -> false
  | Paper -> (List.map
    [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]
    ~f:(fun (dx, dy) -> let open Option.Monad_infix in safe_get grid (y + dy) >>= (fun row -> safe_get row (x + dx)) |> Option.value ~default:Floor)
    |> List.sum (module Int) ~f:(fun c -> match c with | Floor -> 0 | Paper -> 1)) < 4

let rec fromto a b = if a >= b then [] else a :: fromto (a + 1) b

let part1 grid =
  let open List.Monad_infix in
  List.cartesian_product (fromto 0 (Array.length grid)) (fromto 0 (Array.length grid.(0))) >>| reachable grid |> List.count ~f:(fun x -> x)

let remove grid =
  let reachable = List.cartesian_product (fromto 0 (Array.length grid)) (fromto 0 (Array.length grid.(0))) |> List.filter ~f:(reachable grid) in
  List.iter reachable ~f:(fun (x, y) -> Array.set grid.(y) x Floor); List.length reachable

let rec part2_aux grid =
  let cur = remove grid in
  if cur = 0 then
    0
  else
    cur + part2_aux grid

let part2 grid =
  let newgrid = Array.map grid ~f:Array.copy in
  part2_aux newgrid

let cell_of_char c = match c with
  | '.' -> Floor
  | '@' -> Paper
  | _   -> invalid_arg "Couldn't convert to cell"

let parse_input input = let open List.Monad_infix in
  String.strip input |> String.split ~on:'\n' >>| (fun line -> String.to_list line >>| cell_of_char |> List.to_array) |> List.to_array

let solve input = let grid = parse_input input in
  printf "%i %i\n" (part1 grid) (part2 grid)
