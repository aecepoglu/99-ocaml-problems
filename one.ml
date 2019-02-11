let rec pow a b =
  if b <= 0 then 1
  else a * (pow a (b - 1))

let sqr = pow 2

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
  type 'a tree =
    | Empty
    | Node of 'a * 'a tree * 'a tree

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

  let rec is_completely_balanced = function
    | Empty -> true
    | Node(_, left, right) ->
       abs (count_tree left - count_tree right) <= 1

  let sum_trees lefts rights acc0 =
    List.fold_left
      (fun acc left ->
         List.fold_left
           (fun acc' right ->
              (Node('x', left, right)) :: acc'
           )
           acc
           rights
      )
      acc0
      lefts

  let rec create_balanced n =
    if n = 0 then
      [Empty]
    else if n mod 2 = 0 then
      let trees_1 = create_balanced (n / 2) in
      let trees_2 = create_balanced ((n / 2) - 1) in
        sum_trees trees_1 trees_2
          (sum_trees trees_2 trees_1 [])
    else
      let trees = create_balanced (n/2) in
        sum_trees trees trees []

  let rec create_height_balanced h =
    if h = 0 then
      [Empty]
    else if h = 1 then
      [Node('x', Empty, Empty)]
    else
      let t1 = create_height_balanced (h - 1) in
      let t2 = create_height_balanced (h - 2) in
        sum_trees t1 t2 (sum_trees t2 t1 (sum_trees t1 t1 []))

  let rec are_symmetrical left right =
    match (left, right) with
    | (Empty, Empty) -> true
    | (Node(a, ll, lr), Node(b, rl, rr)) -> a = b
                                            && (are_symmetrical ll rr)
                                            && (are_symmetrical lr rl)
    | _ -> false

  let rec is_symmetrical = function
    | Empty -> true
    | Node(_, l, r) -> are_symmetrical l r

  let rec add_to_search_tree x = function
    | Empty -> Node(x, Empty, Empty)
    | Node(a, l, r) ->
       if x < a
       then
         Node(a, add_to_search_tree x l, r)
       else
         Node(a, l, add_to_search_tree x r)

  let create_search_tree xs =
    List.fold_left
      (fun t x -> add_to_search_tree x t)
      Empty
      xs

  let iter_bfs f t =
    let rec aux = function
      | [] -> ()
      | Empty :: t -> aux t
      | Node(x, ltree, rtree) :: t ->
         let _ = f x in
           aux (ltree :: rtree :: t)
    in
      aux [t]

  let is_complete_tree n tree =
    let rec aux i = function
      | [] -> i = n + 1
      | Empty :: t -> List.for_all ((=) Empty) t && aux i []
      | Node(x, ltree, rtree) :: t ->
         if x = i
         then aux (x + 1) (ltree :: rtree :: t)
         else false
    in
      aux 1 [tree]

  let create_complete_tree n =
    let rec aux i =
      if i > n
      then Empty
      else Node(i,
                aux (2*i),
                aux (2*i + 1)
               )
    in
      aux 1

  let layout_1 (tree:'a tree) :(('a*int*int) tree)=
    let rec iter i0 d = function
      | Empty -> (i0, Empty)
      | Node(x, ltree, rtree) ->
         let (il, ltree') = (match ltree with
             | Empty -> i0, Empty
             | y -> iter i0 (d + 1) y
           ) in
         let (ir, rtree') = (match rtree with
             | Empty -> il + 1, Empty
             | y -> iter (il + 1) (d + 1) y
           ) in
           (
             ir,
             Node((x, il + 1, d), ltree', rtree')
           )
    in
    let (_, result) = iter 0 1 tree in
      result

  let rec depth = function
    | Empty -> 0
    | Node(_, l, r) -> 1 + max (depth l) (depth r)

  let layout_2 tree :(('a*int*int) tree) =
    let d_max = depth(tree) in
    let rec left_only_depth = function
      | Empty -> 0
      | Node(_, l, _) -> 1 + left_only_depth l
    in
    let rec aux x0 d = function
      | Empty -> Empty
      | Node(x, ltree, rtree) ->
         let dx = pow 2 (d_max - d - 1) in
         let ltree' = aux (x0 - dx) (d + 1) ltree in
         let rtree' = aux (x0 + dx) (d + 1) rtree in
           Node((x, x0, d), ltree', rtree')
    in
    let missing_left_depth = d_max - left_only_depth tree in
    let left_offset = (pow 2 missing_left_depth) - 1 in
    let x_root = (pow 2 (d_max - 1)) - left_offset in
      aux x_root 1 tree

  let sym_and_balanced n =
    create_balanced n
    |> List.filter is_symmetrical

  let print_by f x =
    let rec aux f prefix = function
      | Empty -> print_string "·"
      | Node(a, left, right) ->
         let mystr = f a in
         let spaces = String.make ((String.length mystr) - 1) ' ' in
         let () = print_string mystr in
         let () = print_string " ─┬─ " in
         let () = aux f (prefix ^ spaces ^ "   │  ") right in
         let () = print_string ("\n" ^ prefix ^ spaces ^ "   └─ ") in
           aux f (prefix ^ spaces ^ "      ") left
    in
    let () = aux f "" x in
      print_endline ""

  let rec to_string f = function
    | Empty -> ""
    | Node(x, Empty, Empty) -> f x
    | Node(x, l, r) -> (f x)
                       ^ "("
                       ^ (to_string f l)
                       ^ ","
                       ^ (to_string f r)
                       ^ ")"

  let from_string s =
    let rec aux i :(int*'a tree) =
      if s.[i] = ',' || s.[i] = ')' then
        (i + 0, Empty)
      else
        let c = s.[i] in
          if s.[i+1] = '(' then
            let l_i, ltree = aux (i + 2) in (*skip 'x(' *)
            let r_i, rtree = aux (l_i + 1) in (*skip ',' *)
              (r_i + 1 (*skip ')'*), Node(c, ltree, rtree))
          else
            (i + 1, Node(c, Empty, Empty))
            (*
              a(b(d,e),c(,f(g,)))
            *)
    in
      aux 0 |> snd

  let preorder_seq tree =
    let rec aux acc = function
      | Empty -> acc
      | Node(x, ltree, rtree) -> x :: aux (aux acc rtree) ltree 
    in
      aux [] tree

  let print_int = print_by string_of_int
  let print_char = print_by Char.escaped
end

module MT = struct
  type 'a mult_tree = T of 'a * 'a mult_tree list

  let rec count_nodes = function
    | T(_, trees) -> List.fold_left
                       (fun acc x -> acc + (count_nodes x))
                       1
                       trees

  (*   afg^^c^bd^e^^^


       a^b^c
  *)
  let from_hatstring str :char mult_tree =
    let maxlen = String.length str in
    let rec aux i =
      if i >= maxlen
      then i, []
      else
        let c = str.[i] in
          if c = '^' then
            i, [] (*TODO*)
          else
            let i', children = aux (i + 1) in
            let i'', siblings = aux (i' + 1) in
              i'', T(c, children) :: siblings
    in
      aux 0
      |> snd
      |> List.hd

  let to_hatstring tree =
    let rec aux = function
      | [] -> ""
      | T(x, subtrees) :: t -> (Char.escaped x)
                               ^ aux subtrees
                               ^ "^" 
                               ^ aux t
    in
      aux [tree]

  let ipl tree =
    let rec aux dist_to_root = function
      | [] -> 0
      | T(_, subtrees) :: tl -> dist_to_root
                                + aux (dist_to_root + 1) subtrees
                                + aux (dist_to_root) tl
    in
      aux 0 [tree]

  let rec bottom_up = function
    | T(x, subtrees) -> (subtrees
                        |> List.map bottom_up
                        |> List.flatten
                       ) @ [x]

  let rec lispy (tree:char mult_tree) :string =
    match tree with
    | T(x, []) -> Char.escaped x
    | T(x, subtrees) -> "("
                        ^ (Char.escaped x)
                        ^ " "
                        ^ (subtrees
                           |> List.map lispy
                           |> String.concat " "
                          )
                        ^ ")"

  let example1 = (T('a', [T('f',[]) ]))
  let example2 =
    T('a', [
      T('f', [
          T('g', [])
        ]);
      T('c', []);
      T('b', [
          T('d',[]);
          T('e',[]);
        ])
    ])
end

module Queue = struct
  type 'a node = N of 'a * 'a node * 'a node
               | E

  type 'a queue = Q of ('a node * 'a node)

  let create () = Q(E, E)
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
