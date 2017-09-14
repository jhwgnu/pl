let echo = fun () ->
  let i = read_int() in
  let str = string_of_int i in
  print_endline("Your input: " ^str)

let y = echo print_endline("hihi")


