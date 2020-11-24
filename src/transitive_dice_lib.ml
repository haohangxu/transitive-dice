open Core

let run () =
  let all = Dice.all ~low:1 ~high:9 ~num_sides:6 in
  let dominance_map = Dominance_map.create all in
  let result = List.find_map all ~f:(Dominance_map.find_set dominance_map ~length:6) in
  Core.printf !"%{sexp: Dice.t list option}\n%!" result
;;
