open Core

type t = Dice.Set.t Dice.Map.t

let create dice =
  List.fold dice ~init:Dice.Map.empty ~f:(fun map t ->
      let data =
        List.filter dice ~f:(fun against ->
            match Dice.score t ~against, Dice.score_two_dice t ~against with
            | `Better, `Worse -> true
            | `Worse, _ | `Equal, _ | `Better, (`Better | `Equal) -> false)
      in
      Dice.Map.set map ~key:t ~data:(Dice.Set.of_list data))
;;

let find t dice = Dice.Map.find t dice |> Option.value ~default:Dice.Set.empty

let find_set t start ~length =
  let rec helper curr prev ~length =
    let curr_dominates = find t curr in
    if length = 0
    then if Dice.Set.mem curr_dominates start then Some (curr :: prev) else None
    else
      Dice.Set.find_map curr_dominates ~f:(fun elt ->
          if List.mem prev elt ~equal:Dice.equal
          then None
          else helper elt (curr :: prev) ~length:(length - 1))
  in
  helper start [] ~length:(length - 1)
;;

(* For debugging *)
let _print t =
  Dice.Map.iteri t ~f:(fun ~key ~data ->
      Core.printf
        !"%{Dice} dominates (%s)\n"
        key
        (Dice.Set.to_list data |> List.map ~f:Dice.to_string |> String.concat ~sep:"; "))
;;

let%expect_test "find_set" =
  let all = Dice.all ~low:1 ~high:6 ~num_sides:3 in
  let t = create all in
  Core.printf
    !"%{sexp: Dice.t list option}\n"
    (find_set t (Dice.create_unsafe [ 3; 3; 3 ]) ~length:3);
  [%expect {| (((1 4 4) (2 2 5) (3 3 3))) |}]
;;
