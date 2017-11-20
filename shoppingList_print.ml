type require = id * (cond list)
and cond
  = Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list

and gift = int
and id = A | B | C | D | E

let emptyL = [(A, []); (B, []); (C, []); (D, []); (E, [])]

let sl1 = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]
let sl2 = [(A, [2; 4; 8]); (B, [2; 3; 4; 6; 8; 9]); (C, [2; 3; 4; 5; 6; 8; 9; 10]); (D, [5; 10]); (E, [])]


let rec print_intlist = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_intlist l

let rec print_shoplist = function
[] -> ()
| (person, gift_list)::tl ->
    print_string "gifts: " ;
    print_intlist gift_list;
    print_endline " ";
    print_shoplist tl

let _ =
(* print_intlist([1;2]); *)
print_shoplist(sl1)