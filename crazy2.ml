type crazy2 = NIL
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2add =
  let sum = 0 in
	function
	| (ZERO(c1), ONE(c2)) -> 1
	| (ZERO(c1), MONE(c2)) -> 2
	| (ONE(c1), MONE(c2)) -> 3
	| _ -> 0
