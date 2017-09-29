type crazy2  = NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2

let rec powerof2 = function
    | 0 -> 1
    | 1 -> 2
    | n ->(powerof2(n-1)) * 2

let rec foo k = function
    | NIL -> 0
    | ZERO next -> foo(k+1) next
    | ONE next -> foo(k+1) next + powerof2(k)
    | MONE next -> foo(k+1) next - powerof2(k)

let rec crazy2val = foo 0

let crazy2add c1 c2 = crazy2val(c1) + crazy2val(c2)
