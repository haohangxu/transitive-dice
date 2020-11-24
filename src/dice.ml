open Core

module T = struct
  type t = int list [@@deriving sexp, compare, bin_io]

  (* This isn't totally correct, but we expect dice to be generated with sorted
     sides, so it's fine. *)
  let equal = [%compare.equal: int list]
end

include T
include Comparable.Map_and_set_binable (T)

let create_unsafe sides = sides
let to_string t = List.map t ~f:Int.to_string |> String.concat ~sep:", "

let score' t ~against =
  List.fold ~init:0 t ~f:(fun accum side ->
      accum + List.count against ~f:(fun other_side -> side > other_side))
;;

let%expect_test "score'" =
  let test a b = Core.printf "%d\n" (score' a ~against:b) in
  test [ 5; 5; 5; 5; 5; 5 ] [ 1; 1; 1; 1; 1; 1 ];
  [%expect {| 36 |}];
  test [ 1; 2; 3; 4; 5; 6 ] [ 3; 3; 3; 3; 3; 3 ];
  [%expect {| 18 |}];
  test [ 3; 3; 3; 3; 3; 3 ] [ 1; 2; 3; 4; 5; 6 ];
  [%expect {| 12 |}]
;;

let score t ~against =
  let score_for = score' t ~against in
  let score_against = score' against ~against:t in
  if score_for > score_against
  then `Better
  else if score_for < score_against
  then `Worse
  else `Equal
;;

let%expect_test "score" =
  let test a b =
    Core.printf !"%{sexp:[ `Better | `Worse | `Equal ]}\n" (score a ~against:b)
  in
  test [ 5; 5; 5; 5; 5; 5 ] [ 1; 1; 1; 1; 1; 1 ];
  [%expect {| Better |}];
  test [ 1; 2; 3; 4; 5; 6 ] [ 3; 3; 3; 3; 3; 3 ];
  [%expect {| Better |}];
  test [ 3; 3; 3; 3; 3; 3 ] [ 1; 2; 3; 4; 5; 6 ];
  [%expect {| Worse |}];
  test [ 1; 1; 1; 9; 9; 9 ] [ 2; 2; 2; 8; 8; 8 ];
  [%expect {| Equal |}]
;;

let score_two_dice t ~against =
  let all_possible t =
    List.concat_map t ~f:(fun first -> List.map t ~f:(fun second -> first + second))
  in
  let all_possible_for = all_possible t in
  let all_possible_against = all_possible against in
  score all_possible_for ~against:all_possible_against
;;

let all ~low ~high ~num_sides =
  let rec helper ~low ~num_sides =
    if num_sides = 0
    then [ [] ]
    else
      List.init
        (high - low + 1)
        ~f:(fun i ->
          let low = low + i in
          List.map (helper ~low ~num_sides:(num_sides - 1)) ~f:(fun rest -> low :: rest))
      |> List.concat
  in
  helper ~low ~num_sides
;;

let%expect_test "all" =
  List.iter (all ~low:1 ~high:3 ~num_sides:3) ~f:(fun t ->
      Core.print_endline (to_string t));
  [%expect
    {|
    1, 1, 1
    1, 1, 2
    1, 1, 3
    1, 2, 2
    1, 2, 3
    1, 3, 3
    2, 2, 2
    2, 2, 3
    2, 3, 3
    3, 3, 3 |}]
;;
