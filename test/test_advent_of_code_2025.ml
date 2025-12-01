open! Core

let%test "day01_part2_warning" = (Advent_of_code_2025.Day01.part2 [{ direction = Right ; distance = 1000}]) = 10

let rec prefixes lst = match lst with
  | []       -> [[]]
  | hd :: tl -> let open List.Monad_infix in [] :: (prefixes tl >>| (fun x -> hd :: x))

let%test_unit "prefixes" = [%test_result: int list list] (prefixes [1 ; 2 ; 3]) ~expect:[[] ; [1] ; [1 ; 2] ; [1 ; 2 ; 3]]

let%test_unit "day01_part2_example" = [%test_result: int] (Advent_of_code_2025.Day01.part2 [
  { direction = Left ; distance = 68};
  { direction = Left ; distance = 30};
  { direction = Right ; distance = 48};
  { direction = Left ; distance = 5};
  { direction = Right ; distance = 60};
  { direction = Left ; distance = 55};
  { direction = Left ; distance = 1};
  { direction = Left ; distance = 99};
  { direction = Right ; distance = 14};
  { direction = Left ; distance = 82}]) ~expect:6

let%test_unit "day01_part2_example_fold" = [%test_result: int list] (let input: Advent_of_code_2025.Day01.rotation list = [
  { direction = Left ; distance = 68};
  { direction = Left ; distance = 30};
  { direction = Right ; distance = 48};
  { direction = Left ; distance = 5};
  { direction = Right ; distance = 60};
  { direction = Left ; distance = 55};
  { direction = Left ; distance = 1};
  { direction = Left ; distance = 99};
  { direction = Right ; distance = 14};
  { direction = Left ; distance = 82}] in
  List.fold (prefixes input) ~init:[] ~f:(fun st inp -> st @ [Advent_of_code_2025.Day01.part2 inp])) ~expect:[0; 1; 1; 2; 2; 3; 4; 4; 5; 5; 6]
