open! Core

module Problem = struct
  type operation = ADD | MUL

  type t = {
    numbers : int list;
    operation : operation;
  }

  let operation_of_string s = match s with
    | "+" -> ADD
    | "*" -> MUL
    | _   -> invalid_arg "Couldn't parse operation"
  
  let monoid_of_operation = function
    | ADD -> (0, Int.(+))
    | MUL -> (1, Int.( * ))

  let rec parse a = match a with
    | []       -> invalid_arg "Cannot parse a problem from an empty list"
    | [op]     -> { numbers = [] ; operation = operation_of_string op }
    | hd :: tl -> let { numbers ; operation } = parse tl in { numbers = int_of_string hd :: numbers ; operation }
  
  let evaluate { numbers ; operation } =
    let (init, f) = monoid_of_operation operation in
    List.fold numbers ~init ~f
end

let part1 problems = List.sum (module Int) problems ~f:Problem.evaluate

let parse_input input =
  let open List.Monad_infix in
  String.strip input
  |> String.split_lines
  >>| (fun s -> String.split s ~on:' ' |> List.filter ~f:(Fun.compose not String.is_empty))
  |> List.transpose
  |> Option.value_exn
  >>| Problem.parse

let solve input =
  let problems = parse_input input in
  printf "%i\n" (part1 problems)
