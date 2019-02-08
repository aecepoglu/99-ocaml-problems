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

let rand_select n list =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t ->
       if n = 0
       then (h, (List.rev acc) @ t)
       else extract (h :: acc) (n - 1) t
  and extract_rand len list =
    extract [] (Random.int len) list
  and aux n acc len list =
    if n = 0
    then
      acc
    else
      let picked, rest = extract_rand len list in
        aux (n - 1) (picked :: acc) (len - 1) rest
  in
    aux n [] (List.length list) list

let lotto n list =
  rand_select n (range 1 n)

let permutation list =
  rand_select (List.length list) list

let rec permutations = function
  | [x] :: t        -> permutations t |> List.map (List.cons x)
  | (h0 :: t0) :: t ->  (permutations ([h0] :: t)) @ (permutations (t0 :: t))
  | []              -> [[]]
  | [] :: _         -> [] (* this case is never matched *)

let rec combinations n list =
  if n > 0
  then
    match list with
    | [] -> []
    | h :: t ->
       let combos_with_h = List.map (List.cons h) (combinations (n - 1) t) in
       let combos_without_h = combinations n t in
         combos_with_h @ combos_without_h
  else 
    [[]]

let is_prime x =
  let rec aux y =
    if y <= 1 then true
    else if x mod y = 0 then false
    else aux (y - 1)
  in
    aux (x - 1)

let gcd a b =
  let rec aux i =
    if (a mod i) = 0 && (b mod i) = 0
    then i
    else aux (i - 1)
  in
    aux (min a b)

let are_coprimes a b =
  gcd a b = 1

let phi n =
  range 1 (n - 1)
  |> List.filter (are_coprimes n)
  |> List.length

let rec factors_of n =
  let rec aux i =
    if n mod i = 0
    then i :: factors_of (n / i)
    else aux (i + 1)
  in
    if n > 1
    then aux 2
    else []

let rec factors_of' n =
  factors_of n |> encode';;

let prime_numbers_in_range i0 i1 =
  range i0 i1
  |> List.filter is_prime

let goldbach_pairs n =
  range 1 (n/2)
  |> List.map (fun x -> x, n - x)
  |> List.filter (fun (a, b) -> is_prime a && is_prime b)

let goldbach_pair n =
  match goldbach_pairs n with
  | h :: _ -> Some h
  | [] -> None

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let truth_table vars (expr:bool_expr) =
  let rec eval (lookup:(string*bool) list) = function
    | Var x        -> List.assoc x lookup
    | Not e        -> not (eval lookup e)
    | And (e1, e2) -> (&&) (eval lookup e1) (eval lookup e2)
    | Or (e1, e2)  -> (||) (eval lookup e1) (eval lookup e2)
  in
  let var_combo = List.map
                    (fun x -> [(x, false); (x, true)])
                    vars
  in
    var_combo
    |> permutations
    |> List.map (fun x -> x, eval x expr)

let rec greycode n =
  range 1 n
  |> List.map (fun _ -> [0; 1]) 
  |> permutations
  |> List.map (fun l ->
      l
      |> List.map string_of_int
      |> String.concat ""
    )

module BT = struct
  type 'a t =
    | Empty
    | Node of 'a * 'a t * 'a t

  let rec count_tree = function
    | Empty -> 0
    | Node(_, left, right) -> 1 + count_tree left + count_tree right

  let rec count_leaves = function
    | Empty -> 0
    | Node(_, Empty, Empty) -> 1
    | Node(_, left, right) -> count_leaves left + count_leaves right

  let rec collect_leaves = function
    | Empty -> []
    | Node(x, Empty, Empty) -> [x]
    | Node(_, left, right) -> collect_leaves left @ collect_leaves right

  let rec collect_internals = function
    | Empty | Node(_, Empty, Empty) -> []
    | Node(x, left, right) -> x :: (collect_internals left @ collect_internals right)

  let rec nodes_at_depth n = function
    | x when n = 0 -> [x]
    | Empty -> []
    | Node(_, left, right) -> (nodes_at_depth (n - 1) left)
                              @ (nodes_at_depth (n - 1) right)
end

module MT = struct
  type 'a mult_tree = T of 'a * 'a mult_tree list

  let rec count_nodes = function
    | T(_, trees) -> List.fold_left
                       (fun acc x -> acc + (count_nodes x))
                       1
                       trees
end

(* variables to use in interactive shell (eg. utop) *)

let repeating_list = [
  "a";"a";"a";"a";
  "b";
  "c";"c";
  "a";"a";
  "d";"d";
  "e";"e";"e";"e";
]
let numbers = range 1 8
