open Core

type t [@@deriving sexp_of]

val equal : t -> t -> bool

include Comparable.Map_and_set_binable with type t := t

val to_string : t -> string
val score : t -> against:t -> [ `Better | `Worse | `Equal ]
val score_two_dice : t -> against:t -> [ `Better | `Worse | `Equal ]
val all : low:int -> high:int -> num_sides:int -> t list

(* Must provide sides in non-decreasing order. *)
val create_unsafe : int list -> t
