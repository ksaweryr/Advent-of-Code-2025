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

let%test_unit "day02_make_invalid_id_part1" = [%test_result: int64] (Advent_of_code_2025.Day02.make_invalid_id 2 1L) ~expect:11L
let%test_unit "day02_make_invalid_id_part2" = [%test_result: int64] (Advent_of_code_2025.Day02.make_invalid_id 5 123L) ~expect:123123123123123L

let%test_unit "day02_split_id" = [%test_result: int64 list] (Advent_of_code_2025.Day02.split_id 4 21371337L) ~expect:[21L ; 37L ; 13L ; 37L]

let%test_unit "day02_part1_example" = [%test_result: int64] (let input: Advent_of_code_2025.Day02.range list = [
  { lo = 11L ; hi = 22L };
  { lo = 95L ; hi = 115L };
  { lo = 998L ; hi = 1012L };
  { lo = 1188511880L ; hi = 1188511890L };
  { lo = 222220L ; hi = 222224L };
  { lo = 1698522L ; hi = 1698528L };
  { lo = 446443L ; hi = 446449L };
  { lo = 38593856L ; hi = 38593862L };
  { lo = 565653L ; hi = 565659L };
  { lo = 824824821L ; hi = 824824827L };
  { lo = 2121212118L ; hi = 2121212124L }] in
  Advent_of_code_2025.Day02.part1 input) ~expect:1227775554L

let%test_unit "day02_part2_example" = [%test_result: int64] (let input: Advent_of_code_2025.Day02.range list = [
  { lo = 11L ; hi = 22L };
  { lo = 95L ; hi = 115L };
  { lo = 998L ; hi = 1012L };
  { lo = 1188511880L ; hi = 1188511890L };
  { lo = 222220L ; hi = 222224L };
  { lo = 1698522L ; hi = 1698528L };
  { lo = 446443L ; hi = 446449L };
  { lo = 38593856L ; hi = 38593862L };
  { lo = 565653L ; hi = 565659L };
  { lo = 824824821L ; hi = 824824827L };
  { lo = 2121212118L ; hi = 2121212124L }] in
  Advent_of_code_2025.Day02.part2 input) ~expect:4174379265L
