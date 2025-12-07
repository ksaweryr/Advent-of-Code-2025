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
    tachyons : (Position.t, Position_order.comparator_witness) Set.t;
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
    { diagram ; tachyons = Set.empty (module Position_order) |> (Fun.flip Set.add (start, 0)) ; splits = 0 }
  
  let rec simulate ?(depth = 0) ({ diagram ; tachyons ; splits } as state) =
    if depth = Array.length diagram - 1 then
      state
    else
      let width = Array.length diagram.(0) in
      let (new_tachyons, new_splits) = Set.fold ~init:(Set.empty (module Position_order), splits) tachyons ~f:(fun (s, cnt) (x, y) -> match diagram.(y + 1).(x) with
        | SPLITTER -> ((if x > 0 then Set.add s (x - 1, y + 1) else s) |> (fun s' -> (if x < width - 1 then Set.add s' (x + 1, y + 1) else s')), cnt + 1)
        | _        -> (Set.add s (x, y + 1), cnt)
      ) in
      simulate ~depth:(depth + 1) { diagram ; tachyons = new_tachyons ; splits = new_splits }
end

let part1 simulation = (Simulation.simulate simulation).splits

let solve input =
  let simulation = Simulation.of_string input in
    printf "%i\n" (part1 simulation)
