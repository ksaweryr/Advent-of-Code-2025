open! Core

type direction = Left | Right

type rotation = {
  direction : direction;
  distance : int;
}

type state = {
  position : int;
  counter : int;
}

let apply_rotation pos { direction ; distance } = let raw_pos = (match direction with
  | Left  -> pos - distance
  | Right -> pos + distance) in
    match raw_pos with
    | n when n < 0 -> (((raw_pos mod 100) + 100) mod 100, (-raw_pos / 100) + (if pos = 0 then 0 else 1))
    | 0            -> (0, 1)
    | _            -> (raw_pos mod 100, raw_pos / 100)


let direction_of_char s = match s with
  | 'L' -> Left
  | 'R' -> Right
  | _   -> invalid_arg "Can't parse direction"

let rotation_of_str s = let dir = direction_of_char (String.get s 0) and dist = int_of_string (String.drop_prefix s 1) in
  { direction = dir ; distance = dist }

let part1 rs = let final_state = List.fold rs ~init:{ position = 50 ; counter = 0 } ~f:(fun st rot ->
  let (new_position, _) = apply_rotation st.position rot in
    { position = new_position ; counter = st.counter + (if new_position = 0 then 1 else 0) }) in
  final_state.counter

let part2 rs = let final_state = List.fold rs ~init:{ position = 50 ; counter = 0 } ~f:(fun st rot ->
  let (new_position, delta) = apply_rotation st.position rot in
    { position = new_position ; counter = st.counter + delta }) in
  final_state.counter

let solve input = let open List.Monad_infix in let rotations = String.strip input |> String.split ~on:'\n' >>| rotation_of_str in
  printf "%i %i\n" (part1 rotations) (part2 rotations)
