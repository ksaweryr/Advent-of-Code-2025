open! Core

module Simulation = struct
  type cell = EMPTY | SPLITTER | START
  type diagram = cell array array
  
  module Position = struct
    type t = int * int [@@deriving compare, sexp]
  end

  module Position_order = struct
    type t = Position.t
    include Comparator.Make(Position)
  end

  type t = {
    diagram : diagram;
    tachyons : (Position.t, int, Position_order.comparator_witness) Map.t;
    splits : int;
  }

  let cell_of_char = function
    | '.' -> EMPTY
    | '^' -> SPLITTER
    | 'S' -> START
    | _   -> invalid_arg "Couldn't parse cell"

  let of_string s =
    let diagram =
      let open List.Monad_infix in
      String.strip s
      |> String.split_lines
      >>| Fun.compose (Array.map ~f:cell_of_char) String.to_array
      |> List.to_array in
    let start = Array.findi diagram.(0) ~f:(fun _ -> function | START -> true | _ -> false) |> Option.value_exn |> fst in
    { diagram ; tachyons = Map.empty (module Position_order) |> (Map.set ~key:(start, 0) ~data:1) ; splits = 0 }

  let add_tachyon m pos cnt = Map.update m pos ~f:(fun o -> cnt + Option.value o ~default:0)

  let rec simulate ?(depth = 0) ({ diagram ; tachyons ; splits } as state) =
    if depth = Array.length diagram - 1 then
      state
    else
      let width = Array.length diagram.(0) in
      let (new_tachyons, new_splits) = Map.fold ~init:(Map.empty (module Position_order), splits) tachyons ~f:(fun ~key:(x, y) ~data:cnt (m, acc_splits) -> match diagram.(y + 1).(x) with
        | SPLITTER -> ((if x > 0 then add_tachyon m (x - 1, y + 1) cnt else m) |> (fun m' -> (if x < width - 1 then add_tachyon m' (x + 1, y + 1) cnt else m')), acc_splits + 1)
        | _        -> (add_tachyon m (x, y + 1) cnt, acc_splits)
      ) in
      simulate ~depth:(depth + 1) { diagram ; tachyons = new_tachyons ; splits = new_splits }
end

let part1 { Simulation.splits ; _ } = splits

let part2 { Simulation.tachyons ; _ } = Map.sum (module Int) tachyons ~f:Fun.id

let solve input =
  let simulation = Simulation.of_string input |> Simulation.simulate in
    printf "%i %i\n" (part1 simulation) (part2 simulation)
