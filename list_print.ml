let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let _ =
print_list([1;2;]);
print_endline " ";
print_list([2;3;])