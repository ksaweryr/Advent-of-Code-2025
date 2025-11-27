open! Core

let parse_envvar line = match String.split line ~on:'=' with
  | name :: values -> Some (name, String.concat ~sep:"=" values)
  | []             -> None

let load_dotenv () = In_channel.with_file (Filename.of_parts [Core_unix.getcwd (); ".env"]) ~f:(fun ch -> let open List.Monad_infix in
  List.fold (In_channel.input_lines ch >>| parse_envvar)
    ~init:()
    ~f:(fun _ x -> match x with
      | Some (key, data) -> Core_unix.putenv ~key ~data
      | None               -> ()))

let fetch_input day = let open Lwt in let open Cohttp in let open Cohttp_lwt_unix in
  load_dotenv ();
  let year = Sys.getenv "AOC_YEAR" |> Option.value ~default:(string_of_int ((Core_unix.localtime (Core_unix.time ())).tm_year + 1900))
    and session = Sys.getenv "AOC_SESSION" |> Option.value ~default:""
    in let url = Fmt.str "https://adventofcode.com/%s/day/%i/input" year day in
      Lwt_main.run (Client.get ~headers:(Http.Header.of_list [("Cookie", Fmt.str "session=%s" session)]) (Uri.of_string url) >>= fun (resp, body) ->
          match resp |> Response.status with
            | `OK          -> (Cohttp_lwt.Body.to_string body) >|= (fun x -> Ok x)
            | `Bad_request -> return "SESSION is invalid" >|= (fun x -> Error x)
            | `Not_found   -> return "Puzzle is not available yet" >|= (fun x -> Error x)
            | c                   -> return (Code.code_of_status c |> Fmt.str "Unknown error: received HTTP status code %i") >|= (fun x -> Error x))

let get_input day =
  let path = Filename.of_parts [Core_unix.getcwd () ; "input" ; Fmt.str "day_%02i.txt" day] in
    let open Result.Monad_infix in
      (match Core_unix.access path [`Exists] with
        | Ok _    -> Ok ()
        | Error _ -> Result.map (fetch_input day) ~f:(fun data -> Out_channel.write_all path ~data)) >>=
      (fun () -> try
        Ok (In_channel.read_all path)
      with _ -> Error "Couldn't read file")

let solutions = [|
  Advent_of_code_2025.Day01.solve;
  Advent_of_code_2025.Day02.solve;
  Advent_of_code_2025.Day03.solve;
  Advent_of_code_2025.Day04.solve;
  Advent_of_code_2025.Day05.solve;
  Advent_of_code_2025.Day06.solve;
  Advent_of_code_2025.Day07.solve;
  Advent_of_code_2025.Day08.solve;
  Advent_of_code_2025.Day09.solve;
  Advent_of_code_2025.Day10.solve;
  Advent_of_code_2025.Day11.solve;
  Advent_of_code_2025.Day12.solve
|]

let () =
  let argv = Sys.get_argv () in
    if Array.length argv <> 2 then
      printf "Usage: %s <day number>\n" argv.(0)
    else
      let open Option.Monad_infix in
        match (int_of_string_opt argv.(1) >>= (fun x -> if x >= 1 && x <= 12 then Some x else None)) with
          | Some day -> (match get_input day with
            | Ok input -> solutions.(day - 1) input
            | Error err -> printf "Couldn't run solution for day %i: %s\n" day err)
          | None -> print_endline "Day number must be an integer between 1 and 12"
