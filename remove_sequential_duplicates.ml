let rec remove_same list =
  match list with
  | [] -> []
  | [hd1] -> [hd1]
  | [hd1 :: hd2 :: tl] ->
    if hd1=hd2 then remove_same [hd2::tl]
    else [hd1 :: remove_same [hd2::tl]




