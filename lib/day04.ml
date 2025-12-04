open! Core

type cell = Floor | Paper

let reachable grid (x, y) = match List.nth_exn grid y |> (fun row -> List.nth_exn row x) with
  | Floor -> false
  | Paper -> (List.map
    [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]
    ~f:(fun (dx, dy) -> let open Option.Monad_infix in List.nth grid (y + dy) >>= (fun row -> List.nth row (x + dx)) |> Option.value ~default:Floor)
    |> List.sum (module Int) ~f:(fun c -> match c with | Floor -> 0 | Paper -> 1)) < 4

let rec fromto a b = if a >= b then [] else a :: fromto (a + 1) b

let part1 grid = let open List.Monad_infix in
  List.cartesian_product (fromto 0 (List.length grid)) (fromto 0 (List.nth_exn grid 0 |> List.length)) >>| reachable grid |> List.count ~f:(fun x -> x)

let cell_of_char c = match c with
  | '.' -> Floor
  | '@' -> Paper
  | _   -> invalid_arg "Couldn't convert to cell"

let parse_input input = let open List.Monad_infix in
  String.strip input |> String.split ~on:'\n' >>| (fun line -> String.to_list line >>| cell_of_char)

let solve input = let grid = parse_input input in
  printf "%i\n" (part1 grid)
