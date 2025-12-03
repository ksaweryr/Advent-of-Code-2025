open! Core

let rec init a = match a with
  | []       -> []
  | [_]      -> []
  | hd :: tl -> hd :: init tl

let max_joltage bank = match List.mapi bank ~f:(fun i x -> (i, x)) |> init |> List.max_elt ~compare:(fun (_, a) (_, b) -> a - b) with
  | Some (idx, n) -> 10 * n + (List.drop bank (idx + 1) |> List.max_elt ~compare:(Int.(-)) |> Option.value_exn)
  | None          -> 0

let part1 banks = List.sum (module Int) banks ~f:max_joltage

let parse_input input = let open List.Monad_infix in
  String.strip input
  |> String.split ~on:'\n'
  >>| (fun s -> String.to_list s
    >>| (fun x -> Char.to_int x - Char.to_int '0'))

let solve input = let banks = parse_input input in
  printf "%i\n" (part1 banks)
