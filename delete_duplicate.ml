let rec process l =
    match l with
    | [] -> []
    | [hd] -> [hd]
    | hd1::hd2::tl ->
        if hd1=hd2 then process (hd2::tl)
        else hd1::process (hd2::tl)

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l


let l1 = [4;4;5;6;7;7]
let _ =
print_list(process(l1))