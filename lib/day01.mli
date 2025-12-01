type direction = Left | Right

type rotation = {
  direction : direction;
  distance : int;
}

type state = {
  position : int;
  counter : int;
}

val solve: string -> unit
val part1: rotation list -> int
val part2: rotation list -> int