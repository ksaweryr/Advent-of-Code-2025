val solve: string -> unit

module Problem : sig
  type operation = ADD | MUL [@@deriving compare, sexp]

  type t = {
    numbers : int list;
    operation : operation;
  } [@@deriving compare, sexp]

  val parse_1 : string list -> t
  val parse_2 : char list list -> t
  val evaluate : t -> int
end

val parse_input_2 : string -> Problem.t list
