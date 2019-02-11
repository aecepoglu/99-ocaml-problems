type gameinput = I of char

type action = Action of char list

let char_of_input = function I c -> c

let action_of_inputs inputs =
   Action (List.rev (List.map (char_of_input) inputs))

let rec find_combos_for
          (acc_input:gameinput list)
          (inp:gameinput list) :action list list =
  match inp with
  | [] -> []
  | [h] -> [[ (action_of_inputs (h :: acc_input)) ]]
  | h :: rest -> (find_combos_for ((h :: acc_input)) rest)
                    @ (
                      (find_combos_for [] rest)
                      |> List.map (
                          List.cons (action_of_inputs
                                       (h :: acc_input)
                                    )
                        )
                    )

let example = [I('v'); I('>'); I('a'); I('b');]
