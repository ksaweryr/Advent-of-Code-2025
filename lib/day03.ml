open! Core

let rec max_joltage n bank = if n = 0 then
    0L
  else
    match List.mapi bank ~f:(fun i x -> (i, x)) |> (fun a -> List.take a (List.length a - n + 1)) |> List.max_elt ~compare:(fun (_, a) (_, b) -> a - b) with
    | Some (idx, x) -> let open Int64 in (pow 10L (Int64.of_int n - 1L)) * (Int64.of_int x) + max_joltage (Int.pred n) (List.drop bank (Int.succ idx))
    | None          -> failwith "Should never happen"

let part1 banks = List.sum (module Int64) banks ~f:(max_joltage 2)
let part2 banks = List.sum (module Int64) banks ~f:(max_joltage 12)

let parse_input input = let open List.Monad_infix in
  String.strip input
  |> String.split ~on:'\n'
  >>| (fun s -> String.to_list s
    >>| (fun x -> Char.to_int x - Char.to_int '0'))

let solve input = let banks = parse_input input in
  printf "%s %s\n" (part1 banks |> Int64.to_string) (part2 banks |> Int64.to_string)
