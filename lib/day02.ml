open! Core

type range = {
  lo : int64;
  hi : int64;
}

let rec num_digits n = if Int64.(=) n 0L then 0 else 1 + num_digits (Int64.(/) n 10L)

let rec fromto a b = let open Int64 in if a >= b then [] else a :: fromto (a + 1L) b

let split_id numparts n = let h = (num_digits n) / numparts in let m = Int64.pow 10L (Int64.of_int h) in
  let rec aux n' = let open Int64 in if n' = 0L then [] else (n' % m) :: (aux (n' / m)) in
    aux n |> List.rev

let rec make_invalid_id numparts n = if numparts = 1 then
    n
  else
    let nd = num_digits n in let open Int64 in n + (make_invalid_id (Int.pred numparts) n) * (Int64.pow 10L (Int64.of_int nd))

let strip_range_left numparts { lo ; hi } = let ndl = num_digits lo and ndh = num_digits hi in
  let missing_digits = (numparts - (ndl mod numparts)) mod numparts in
  if missing_digits = 0 then
    Some { lo ; hi }
  else
    if ndl + missing_digits <= ndh then
      Some { lo = Int64.pow 10L (Int64.of_int (ndl + missing_digits - 1)) ; hi }
    else None
    

let strip_range_right numparts { lo ; hi } = let ndl = num_digits lo and ndh = num_digits hi in
  let overflown_digits = ndh mod numparts in
    if overflown_digits = 0 then
      Some { lo ; hi }
    else
      if ndl <= ndh - overflown_digits then
        Some { lo ; hi = let open Int64 in (Int64.pow 10L (Int64.of_int (Int.(-) ndh overflown_digits))) - 1L}
      else
        None

let rec simplify_aux x a f = match a with
  | hd :: tl -> if f x hd then true else if Int64.(=) x hd then simplify_aux x tl f else false
  | []       -> true

let simplify_lo numparts n = match split_id numparts n with
  | hd :: tl -> let open Int64 in if simplify_aux hd tl (Int64.(>)) then hd else hd + 1L
  | []       -> invalid_arg "Should never happen"

let simplify_hi numparts n = match split_id numparts n with
  | hd :: tl -> let open Int64 in if simplify_aux hd tl (Int64.(<)) then hd else hd - 1L
  | []       -> invalid_arg "Should never happen"

let simplify_range numparts { lo ; hi } = { lo = simplify_lo numparts lo ; hi = simplify_hi numparts hi }

let strip_range numparts r = let open Option.Monad_infix in strip_range_left numparts r >>= strip_range_right numparts >>| simplify_range numparts

let invalid_ids numparts r = match strip_range numparts r with
  | Some { lo ; hi } -> fromto lo (Int64.(+) hi 1L)
  | None             -> []

let part2_single r = let open List.Monad_infix in
  fromto 2L 11L
  >>| (fun i' -> let i = Int64.to_int i' |> Option.value ~default:(-1) in invalid_ids i r >>| (make_invalid_id i) |> Set.of_list (module Int64))
  |> List.fold ~init:(Set.empty (module Int64)) ~f:(fun acc s -> Set.union acc s)
  |> Set.sum (module Int64) ~f:(fun x -> x)

let range_of_string s = match String.split s ~on:'-' with
  | lo :: hi :: [] -> { lo = Int64.of_string lo ; hi = Int64.of_string hi }
  | _              -> invalid_arg "Can't parse range"

let parse_input input = let open List.Monad_infix in String.strip input |> String.split ~on:',' >>| range_of_string

let part1 ranges = let open List.Monad_infix in List.sum (module Int64) (ranges >>= (invalid_ids 2)) ~f:(make_invalid_id 2)
let part2 ranges = let open List.Monad_infix in List.sum (module Int64) (ranges >>| part2_single) ~f:(fun x -> x)

let solve input = let ranges = parse_input input in
    printf "%s %s\n" (Int64.to_string (part1 ranges)) (Int64.to_string (part2 ranges))
