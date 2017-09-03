
let rec sum ?(multiple=[1]) = function
  | 0 -> 0
  | n -> (sum ~multiple (n-1)) + if List.exists (fun y -> ( n mod y ) = 0 ) multiple then n else 0
  ;;

let rec prob1 = sum ~multiple:[3; 5] 999;;


print_endline ( "problem 1: " ^ ( string_of_int prob1 ) );;