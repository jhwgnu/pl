let friends = ["Joe"; "Michael"; "Thomas"]

let my_favorite list = 
  match list with
  | first :: rest -> first (* at least one element + [] *)
  | [] -> "No Friend TT"

let _ = print_endline(my_favorite friends)
