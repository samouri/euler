open Core.Std;;

(* 
  sum between 0 and provided number. 
  @param multiple: only sum numbers that are multiples of a number in this list
*)
let rec sum ?(multiple=[1]) = function
  | 0 -> 0
  | n -> (sum ~multiple (n-1)) + if List.exists ~f: (fun y -> n mod y = 0 ) multiple then n else 0
  ;;

(* 
  a worse version of sum because of mem usage and speed but at least i got to 
  play with some cool functions like reverse applicative operator 
*)
let sum2 ?(multiple=[1]) n = List.range 0 (n+1)
  |> List.filter ~f: (fun z -> (List.exists ~f: (fun y -> z mod y = 0) multiple ) )
  |> List.fold ~f: (+) ~init: 0;;

let rec fib = function
  | 0 -> 1
  | 1 -> 2
  | n -> fib (n - 1) + fib (n - 2)
  ;;

(* 
  Keeping this for old times sake so i see just how bad my first solution
  was 
*)
let rec sum_until_4mil n =
  let fibVal = fib n in
  let isEven = fibVal mod 2 = 0 in
  if fibVal > 4_000_000 then 0 else (
    ( if isEven then fibVal else 0 ) + sum_until_4mil (n+1)
  )

let prob1 = sum ~multiple:[3; 5] 999;;
let prob1v2 = sum2 ~multiple:[3; 5] 999;;

let prob2 = sum_until_4mil 0;;

print_endline ( "problem 1: " ^ ( string_of_int prob1 ) );;
print_endline ( "problem 1v2: " ^ ( string_of_int prob1v2 ) );;
print_endline ( "problem 2: " ^ ( string_of_int prob2 ) );;