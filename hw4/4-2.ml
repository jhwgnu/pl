type require = id * (cond list)
and cond
  = Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list
and gift = int
and id = A | B | C | D | E

let rec shoppingList r_list = function
  | [] -> []
  | hd::tl -> tl
