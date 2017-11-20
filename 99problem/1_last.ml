let rec last  l =
    match l with
        | [] -> None
        | [x] -> Some x
        | hd::tl -> last tl

let get_string data =
    match data with
        | None -> ""
        | Some str -> str

let l1 = ["ad"; "dd"; "qg"; "vv"]
let _ =
print_string(get_string(last l1))