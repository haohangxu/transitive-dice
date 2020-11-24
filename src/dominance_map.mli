type t

val create : Dice.t list -> t
val find : t -> Dice.t -> Dice.Set.t
val find_set : t -> Dice.t -> length:int -> Dice.t list option
