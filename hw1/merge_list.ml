let rec merge (list1, list2) =
  match list1 with
  | [] ->  list2
  | hd1 :: tl1 ->
    (match list2 with
    | [] -> list1
    | hd2 :: tl2 ->
	if hd1 >= hd2 then hd1 :: merge(tl1, list2)
	else hd2 :: merge(list1, tl2)
    )

(*
open Printf
let l1 = [9;5;4;3;2]
let l2 = [8;7;1]
let result = merge(l1, l2)
let () = List.iter(printf "%d ") result
*)

let _ =
    let rec string_of_list = function
        [] -> ""
        | e::l -> string_of_int e ^ " " ^ string_of_list l
    in
    let assert_equal (expected: int list) (actual: int list) =
        if expected = actual then print_endline "true"
        else
            let expected_string = string_of_list expected in
            let actual_string = string_of_list actual in
            Printf.printf "Expected %s but actual %s\n" expected_string actual_string
    in
    let test_merge (xs: int list) (ys: int list) (expected: int list) =
        merge (xs, ys) |> assert_equal expected
    in
    test_merge [3;2;1] [4;3;2] [4;3;3;2;2;1];
    test_merge [2;1] [10;9] [10;9;2;1];
    test_merge [10;9] [4;3;2] [10;9;4;3;2];
    test_merge [5;3;1] [6;4;2] [6;5;4;3;2;1]