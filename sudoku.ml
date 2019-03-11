type cell =
  | Hard of int
  | Soft of int

let int_of_cell = function
  | Hard x | Soft x -> x

let seri = [0; 1; 2; 3; 4; 5; 6; 7; 8]
let seri2 = [0; 1; 2; 9; 10; 11; 18; 19; 20]

let cell_equals x = function
  | Soft y | Hard y -> y = x

let is_ok map i v =
  let col = i mod 9 in
  let row_start = i - col in
  let box_row = i / 27 in
  let box_col = col / 3 in
  let box_start = box_row * 27 + box_col *3 in
    (*
    Printf.printf "is_ok %d in" v;
    Printf.printf " col %d, " col;
    Printf.printf " rows between [%d, %d)" row_start (row_start + 9);
    Printf.printf " box %d, %d starting with %d \n" box_row box_col box_start;
       *)
    List.for_all (fun k -> not (cell_equals v map.(row_start + k))) seri
    && List.for_all (fun k -> not (cell_equals v map.(9*k + col))) seri
    && List.for_all (fun k -> not (cell_equals v map.(box_start + k))) seri2


let print_map xs =
  for row = 0 to 8 do
    for col = 0 to 8 do
      Printf.printf "%d " (int_of_cell xs.(row * 9 + col));
    done;
    print_endline ""
  done

let rec solve map i v =
  (*
  Printf.printf "i: %d, v: %d\n" i v;
  print_map map;
     *)
  if i >= 81
  then Some map
  else match map.(i) with
    | Hard x                    -> solve map (i + 1) 1
    | Soft _ when v >= 10       -> None
    | Soft _ when is_ok map i v -> map.(i) <- Soft v;
                                   (match solve map (i + 1) 1 with
                                    | None ->
                                       map.(i) <- Soft 0;
                                       solve map i (v + 1)
                                    | Some solution -> Some solution
                                   )
    | Soft _                    -> solve map i (v + 1)

let transform xs =
  let map = Array.make 81 (Soft 0) in
    List.iteri (fun i x ->
        map.(i) <- (
          if x = 0
          then (Soft 0)
          else (Hard x)
        )
      ) xs
    ;
    map

let sudoku values =
  solve (transform values) 0 1
  |> (fun x -> match x with
      | Some solution -> print_map solution
      | None -> print_endline "no solution"
    )

let solution = sudoku [
    3; 8; 7;  0; 9; 0;  4; 2; 1;
    6; 0; 0;  0; 8; 1;  0; 0; 7;
    1; 0; 0;  4; 0; 2;  0; 0; 8;

    0; 2; 4;  0; 0; 0;  8; 0; 0;
    7; 5; 0;  0; 0; 0;  0; 6; 9;
    0; 0; 3;  0; 0; 0;  5; 7; 0;

    5; 0; 0;  7; 0; 8;  0; 0; 6;
    2; 0; 0;  9; 4; 0;  0; 0; 5;
    4; 7; 8;  0; 6; 0;  3; 9; 2;
  ]
