let rec sigma (a, b, f) =
  if a>b then 0
  else f(a) + sigma(a+1,b,f)

(*
let foo a = a*2
let result = sigma(3,5,foo)
let _ = print_endline(string_of_int result)
*)
