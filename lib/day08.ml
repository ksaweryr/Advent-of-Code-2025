open! Core

module Union_find = struct
  type uf_elem = {
    parent : int;
    size   : int;
  } [@@deriving sexp]

  let create n = Array.init n ~f:(fun i -> { parent = i ; size = 1 })

  let rec find uf a =
    let parent = uf.(a).parent in
    if parent = a then
      uf.(a)
    else
      (let result = find uf parent in
      Array.set uf a result;
      result)

  let union uf a b =
    (* printf "union %i %i\n" a b; *)
    let x = find uf a and y = find uf b in
    if x.parent = y.parent then
      ()
    else
      (let (a, b, x, y) = (if x.size >= y.size then (a, b, x, y) else (b, a, y, x)) in
      let nx = { parent = x.parent ; size = x.size + y.size } in
      Array.set uf x.parent nx;
      Array.set uf y.parent nx;
      Array.set uf a nx;
      Array.set uf b nx)
end

type edge = {
  i      : int;
  j      : int;
  length : int
}

let distance (a, b, c) (d, e, f) =
  let dx = a - d and dy = b - e and dz = c - f in
  dx * dx + dy * dy + dz * dz

let all_edges points =
  let arr =
    let indices = List.init (Array.length points) ~f:Fun.id in
    let open List.Monad_infix in
    List.cartesian_product indices indices
    |> List.filter ~f:(fun (i, j) -> i > j)
    >>| (fun (p, q) -> { i = p ; j = q ; length = distance points.(p) points.(q) })
    |> List.to_array in
  Array.sort arr ~compare:(fun e1 e2 -> compare e1.length e2.length);
  arr

let rec merge_n uf edges start n =
  (* printf "merge_n %i %i\n" start n; *)
  if n = 0 then
    ()
  else
    let edge = edges.(start) in
    let a = Union_find.find uf edge.i and b = Union_find.find uf edge.j in
    if a.parent = b.parent then
      merge_n uf edges (start + 1) (n - 1)
    else
      (Union_find.union uf edge.i edge.j;
      merge_n uf edges (start + 1) (n - 1))

let part1 points edges =
  let uf = Union_find.create (Array.length points) in
  merge_n uf edges 0 1000;
  List.init (Array.length points) ~f:Fun.id
  |> List.fold ~init:(Map.empty (module Int)) ~f:(fun m i -> let { Union_find.parent ; Union_find.size } = Union_find.find uf i in Map.set m ~key:parent ~data:size)
  |> Map.to_alist
  |> List.sort ~compare:(fun a b -> compare (snd b) (snd a))
  |> Fun.flip List.take 3
  |> List.fold ~init:1 ~f:(fun acc (_, s) -> acc * s)

let point_of_string s =
  match let open List.Monad_infix in String.split s ~on:',' >>| int_of_string with
    | [a ; b ; c] -> (a, b, c)
    | _           -> invalid_arg "Couldn't parse point"

let parse_input input =
  let open List.Monad_infix in
  String.strip input
  |> String.split_lines
  >>| point_of_string
  |> List.to_array

let solve input =
  let points = parse_input input in
  let edges = all_edges points in
  printf "%i\n" (part1 points edges)
