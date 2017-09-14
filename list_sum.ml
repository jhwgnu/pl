let nums = [5;2;6;3]

let rec sum list =
  match list with
  | [] -> 0
  | hd::tl -> hd + sum tl

let _ = print_endline(string_of_int(sum nums))
