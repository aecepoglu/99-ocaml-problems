(*
   This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

   Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4;2;7;3;6;8;5;1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.
*)

let rec is_ok i unwanteds unwanteds2 = function
  | [] -> true
  | h :: t -> not(List.mem h t)
              && not(List.mem (h - i) unwanteds)
              && not(List.mem (h + i) unwanteds2)
              && is_ok
                   (i + 1)
                   ((h - i) :: unwanteds)
                   ((h + i) :: unwanteds2)
                   t

let check x =
  is_ok 1 [] [] x;;


let rec incr = function
  | 8 :: t -> 1 :: incr t
  | h :: t -> h + 1 :: t
  | [] -> []

let rec collect_8_queens found_solutions = function
  | [8;8;8;8;8;8;8;8] -> found_solutions
  | x when check x -> collect_8_queens (x :: found_solutions) (incr x)
  | x -> collect_8_queens found_solutions (incr x)

let find_8_queens () =
  collect_8_queens [] [1;1;1;1;1;1;1;1]
