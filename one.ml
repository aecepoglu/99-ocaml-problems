let pack list =
  let rec aux cur acc = function
    | a :: (b :: _ as t) ->
       if a = b
       then aux (a :: cur) acc t
       else aux [] ((a :: cur) :: acc) t
    | [x] -> (x :: cur) :: acc
    | [] -> []
  in
    aux [] [] list |> List.rev

let encode' list =
  list |> pack |> List.map (fun x -> (List.length x, List.hd x))

let encode list =
  let rec aux count acc = function
    | a :: (b :: _ as t) ->
       if a = b
       then aux (count + 1) acc t
       else aux 0 ((count + 1, a) :: acc) t
    | [x] -> (count + 1, x) :: acc
    | [] -> []
  in
    aux 0 [] list

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode'' list =
  let rec create_tuple l x =
    if l = 1
    then One(x)
    else Many(l, x)
  and aux count acc = function
    | a :: (b :: _ as t) ->
       if a = b
       then aux (count + 1) acc t
       else aux 0 ((create_tuple (count + 1) a) :: acc) t
    | [a] -> aux 0 (create_tuple (count + 1) a :: acc) []
    | [] -> List.rev acc
  in
    aux 0 [] list

let decode list =
  let rec repeat i x =
    if i <= 0
    then []
    else x :: repeat (i - 1) x
  and aux acc = function
    | One x :: t      -> aux (x :: acc) t
    | Many(i, x) :: t -> aux (repeat i x @ acc) t
    | [] -> List.rev acc
  in
    aux [] list

let rec duplicate = function
  | h :: t -> h :: h :: duplicate t
  | [] -> []

let rec replicate n list =
  let rec aux i acc = function
    | h :: t as l ->
       if i > 0
       then aux (i - 1) (h :: acc) l
       else aux n acc t
    | [] -> List.rev acc
  in
    aux n [] list

let drop n list =
  let rec aux i = function
    | h :: t ->
       if i = n
       then aux 1 t
       else h :: aux (i + 1) t
    | [] -> []
  in
    aux 1 list

let split i_split list =
  let rec aux i fst_half = function
    | h :: t when i < i_split -> aux (i + 1) (h :: fst_half) t
    | x -> (List.rev fst_half, x)
  in
    aux 0 [] list

let rec range i0 i1 =
  if i0 <= i1
  then i0 :: range (i0 + 1) i1
  else []

let slice i0 i1 list =
  let rec aux i0 i1 acc = function
    | _ :: t when i0 > 0 ->
       aux (i0 - 1) (i1 -1) acc t
    | h :: t when i1 > 0 ->
       aux 0 (i1 - 1) (h :: acc) t
    | _ -> List.rev acc
  in
    aux i0 i1 [] list

let rotate_left n list =
  let n' = n mod List.length list in
  let left, right = split n' list in
    right @ left

let repeating_list = [
  "a";"a";"a";"a";
  "b";
  "c";"c";
  "a";"a";
  "d";"d";
  "e";"e";"e";"e";
]
let numbers = range 1 8
