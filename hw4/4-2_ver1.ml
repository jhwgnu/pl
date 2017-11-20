type require = id * (cond list)
and cond
  = Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list

and gift = int
and id = A | B | C | D | E

let emptyL = [(A, []); (B, []); (C, []); (D, []); (E, [])]

let rec delete_duplicate l =
    match l with
    | [] -> []
    | [hd] -> [hd]
    | hd1::hd2::tl ->
        if hd1=hd2 then delete_duplicate (hd2::tl)
        else hd1::delete_duplicate (hd2::tl)

let rec process g_list =
    match g_list with
    | [] -> []
    | (person, l)::tl -> (person, delete_duplicate(List.sort compare l))::process(tl)

let rec shop r_list =
    match r_list with
    | [] -> []
    | (person, cond_list)::r_tl ->
        (match cond_list with
            | [] -> (person, [])::shop(r_tl)
            | cond_hd::cond_tl ->
                match cond_hd with
                    | Items(l) -> (person, l)::shop(r_tl)
                    | _ -> (person, [])::shop(r_tl)
        )

let shoppingList r_list = process(shop r_list)



(* let rec print_intlist = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_intlist l

let rec print_giftlist = function
[] -> ()
| (person, gift_list)::tl ->
    print_string ">";
    print_intlist gift_list;
    print_endline " ";
    print_giftlist tl





let rl1 = [
(A, []);
(B, []);
(C, []);
(D, []);
(E, []);
]

let rl2 = [
(A, [Same B]);
(B, [Same C]);
(C, [Same D]);
(D, [Same E]);
(E, [Same A]);
]

let rl3 = [
(A, [Items [1;2;3]]);
(B, [Items [2;3;4]]);
(C, [Items [3;4;1]]);
(D, [Items [4;1;2]]);
(E, [Items [1;2;3]]);
]
let sl3 = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]

let rl4 = [
(A, [Items [1;2;3;3]]);
(B, [Items [2;3;4]]);
(C, [Items [3;4;1]]);
(D, [Items [4;1;2]]);
(E, [Items [1;2;3]]);
]
let sl4 = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]

let rl5 = [
(A, [Items [1;2]; Items[3]]);
(B, [Items [2;3;4]]);
(C, [Items [3;4;1]]);
(D, [Items [4;1;2]]);
(E, [Items [1;2;3]]);
]
let sl5 = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]



let _ = print_giftlist(shoppingList rl5)

let _ =
(* assert ((shoppingList []) = emptyL); print_endline "0"; *)
assert ((shoppingList rl1) = emptyL); print_endline "1 pass";
assert ((shoppingList rl2) = emptyL); print_endline "2 pass";
assert ((shoppingList rl3) = sl3); print_endline "3 pass";
assert ((shoppingList rl4) = sl4); print_endline "4 pass";
assert ((shoppingList rl5) = sl4); print_endline "5 pass";

print_endline "pass all tests"
 *)
