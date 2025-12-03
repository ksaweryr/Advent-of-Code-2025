type range = {
  lo : int64;
  hi : int64;
}

val make_invalid_id: int -> int64 -> int64
val split_id: int -> int64 -> int64 list

val part1: range list -> int64
val part2: range list -> int64

val solve: string -> unit
