let rec iter (n,f) x =
  if n=0 then x
  else f (iter (n-1,f) x)

(*
let foo x = 2+x
let result = iter(10, foo) 8
let _ = print_endline(string_of_int result)
*)
