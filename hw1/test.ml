let rec clear nums =
match nums with
|[] -> []
|[hd] -> [hd]
|hd1::hd2::tl ->
  if hd1=hd2 then clear (hd2::tl)
  else hd1::clear (hd2::tl)

let list1 = [2;2;3;4;5;5]

let rec print nums =
match nums with
|[] -> print_string " "
|hd::tl -> print_int hd; print_string " "

print(clear list1)
