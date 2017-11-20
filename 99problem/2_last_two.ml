let rec last_two l =
    match l with
        | [] -> None
        | [x] -> None
        | [x; y] -> Some (x, y)
        | _::tl ->  last_two tl

let option_to_string data =
    match data with
        | None -> ""
        | Some x -> x

let l1 = ["gg"; "ff"; "rq"; "gf"; "jj"]
