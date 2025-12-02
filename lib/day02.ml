open! Core

type range = {
  lo : int64;
  hi : int64;
}

let rec num_digits n = if Int64.(=) n 0L then 0 else 1 + num_digits (Int64.(/) n 10L)

let split_id n = let h = (num_digits n) / 2 in let m = Int64.pow 10L (Int64.of_int h) in
  (Int64.(/) n m, Int64.(%) n m)

let make_invalid_id n = let nd = num_digits n in let open Int64 in n + n * (Int64.pow 10L (Int64.of_int nd))

let strip_range_left { lo ; hi } = let ndl = num_digits lo and ndh = num_digits hi in
  if ndl mod 2 = 1 then
    if ndl < ndh then
      Some { lo = Int64.pow 10L (Int64.of_int ndl) ; hi }
    else None
  else
    Some { lo ; hi }

let strip_range_right { lo ; hi } = let ndl = num_digits lo and ndh = num_digits hi in
  if ndh mod 2 = 1 then
    if ndl < ndh then
      Some { lo ; hi = let open Int64 in (Int64.pow 10L (Int64.of_int ndh - 1L)) - 1L }
    else None
  else
    Some { lo ; hi }

let simplify_lo n = let (l, r) = split_id n in
    let open Int64 in
      if l >= r then l else l + 1L

let simplify_hi n = let (l, r) = split_id n in
    let open Int64 in
      if l <= r then l else l - 1L

let simplify_range { lo ; hi } = { lo = simplify_lo lo ; hi = simplify_hi hi }

let strip_range r = let open Option.Monad_infix in strip_range_left r >>= strip_range_right >>| simplify_range

let rec fromto a b = let open Int64 in if a >= b then [] else a :: fromto (a + 1L) b

let invalid_ids r = match strip_range r with
  | Some { lo ; hi } -> fromto lo (Int64.(+) hi 1L)
  | None             -> []

let range_of_string s = match String.split s ~on:'-' with
  | lo :: hi :: [] -> { lo = Int64.of_string lo ; hi = Int64.of_string hi }
  | _              -> invalid_arg "Can't parse range"

let parse_input input = let open List.Monad_infix in String.strip input |> String.split ~on:',' >>| range_of_string

let part1 ranges = let open List.Monad_infix in List.sum (module Int64) (ranges >>= invalid_ids) ~f:make_invalid_id

let solve input = let ranges = parse_input input in
    printf "%s\n" (Int64.to_string (part1 ranges))
