let l = [1;2;3]

let rec increment l =
    match l with
        | [] -> []
        | hd::tl -> hd+1::increment(tl)

let rec repeat func n =
    match n with
        | 1 -> func
        | _ -> func(repeat(func(n-1)))

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let _ =
print_list (increment l);
