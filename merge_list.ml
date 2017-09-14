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
