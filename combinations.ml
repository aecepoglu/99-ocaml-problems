(* a stick can be cut from 3 positions <1|2|3|4>
   Find all possible outcomes
   *)

let rec solve (left:int list) (xs:int list) :(int list list list) = match xs with
  | [] -> []
  | [h] -> [[List.rev (h::left)]]
  | h :: t -> (solve (h :: left) t)
              @ ((solve [] t) |> List.map (List.cons (List.rev (h::left))))


let run = solve []

let input = [1;2;3;4];;
