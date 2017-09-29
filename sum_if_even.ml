let is_even x = x mod 2 =0

let sum_if_even x y =
  (if is_even x then x else 0) +
  (if is_even y then y else 0)

let a = 8
let b = 9

let _ = print_endline(string_of_int (sum_if_even a b))
