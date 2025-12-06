open! Core

module Problem = struct
  type operation = ADD | MUL [@@deriving compare, sexp]

  type t = {
    numbers : int list;
    operation : operation;
  } [@@deriving compare, sexp]

  let operation_of_string s = match s with
    | "+" -> ADD
    | "*" -> MUL
    | _   -> invalid_arg "Couldn't parse operation"
  
  let monoid_of_operation = function
    | ADD -> (0, Int.(+))
    | MUL -> (1, Int.( * ))

  let rec parse_1 = function
    | []       -> invalid_arg "Cannot parse a problem from an empty list"
    | [op]     -> { numbers = [] ; operation = operation_of_string op }
    | hd :: tl -> let { numbers ; operation } = parse_1 tl in { numbers = int_of_string hd :: numbers ; operation }

  let parse_2 = function
    | []       -> invalid_arg "Cannot parse a problem from an empty list"
    | (hd :: _) as cols ->
      let numbers =
        let open List.Monad_infix in
        cols >>| List.drop_last_exn
        |> List.map ~f:(fun col -> List.filter col ~f:Char.is_digit)
        >>| (Fun.compose int_of_string String.of_list)
      and operation = (List.last_exn hd |> Char.to_string |> operation_of_string) in
      { numbers ; operation }

  let evaluate { numbers ; operation } =
    let (init, f) = monoid_of_operation operation in
    List.fold numbers ~init ~f
end

let solve problems = List.sum (module Int) problems ~f:Problem.evaluate

let parse_input_1 input =
  let open List.Monad_infix in
  String.strip input
  |> String.split_lines
  >>| (fun s -> String.split s ~on:' ' |> List.filter ~f:(Fun.compose not String.is_empty))
  |> List.transpose_exn
  >>| Problem.parse_1

let rec group_problems_2 = function
  | [] -> []
  | xss ->
    let cur = List.take_while xss ~f:(fun xs -> List.exists xs ~f:(Char.(<>) ' ')) in
    cur :: group_problems_2 (List.drop xss (List.length cur + 1))

let parse_input_2 input =
  let open List.Monad_infix in
  String.strip input ~drop:(fun c -> Char.(=) c '\n')
  |> String.split_lines
  >>| String.to_list
  |> List.transpose_exn
  |> group_problems_2
  >>| Problem.parse_2

let solve input =
  let problems_1 = parse_input_1 input
  and problems_2 = parse_input_2 input in
  printf "%i %i\n" (solve problems_1) (solve problems_2)
