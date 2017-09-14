let add_even_num a b =
  (if a mod 2==0 then a else 0) + (if b mod 2==0 then b else 0)
let result = add_even_num 7 8

let _ = print_endline(string_of_int result)
