let test_even a = 
  a mod 2 = 0

let add_even_num test a b =
  (if test a then a else 0) + (if test b then b else 0)
let result = add_even_num test_even 7 8

let _ = print_endline(string_of_int result)


