type variable = string

type expression = 
  | Variable of variable
  | Literal of int
  | Addition of expression * expression
  | Multiplication of expression * expression
  | StrictLess of expression * expression

type command =
  | Skip
  | Assignment variable * expression
  | Sequence command * command
  | SelectionNonZero expression * command * command
  | IterationNonZero expression * command

type state = variable -> int

let rec interpret_expression (s : state) (e : expression) : int =
  match e with
  | Var x -> s x
  | Addition e0 e1 -> (interpret_expression s e0) + interpret_expression(s, e1)
  | Multiplication e0 e1 -> (interpret_expression s e0) * (interpret_expression s e1)
  | StrictLess e0 e1 -> if (interpret_expression s e0) < (interpret_expression s e1) then 1 else 0
  | Lit n -> n

let update (s : state) (x : variable) (v : int) =
  fun y -> if x = y then v else s y

let rec interpret_command (s : state) (c : command) : state =
  match c with
  | Skip -> s
  | Assignment x e ->
      let v = (interpret_expression s e) in
      update s x v
  | SelectionNonZero e c0 c1 ->
      if (interpret_expression s e) = 0 then (interpret_command s c0) else (interpret_command s c1)
  | IterationNonZero e c ->
      if (interpret_expression s e) = 0 then s else interpret_command s (Sequence c (IterationNonZero e c))
  | Sequence c0 c1 -> interpret_command (interpret_command s c0) c1

let initial_state : state = fun _ -> 0

let factorial : cmd =
  let x = "X" in
  let ans = "ANS" in
  Sequence(
    Assignment x Literal 6,
    Sequence(
      Assignment(ans, Literal 1),
      IterationNonZero(Variable x,
        Sequence(
          Assignment(ans, Multiplication(Var ans, Var x)),
          Assignment(x, Addition(Var x, Literal (-1)))
        )
      )
    )
  )

let main () =
  Printf.printf "ANS = %d/n" ((interpret_command initial_state factorial) "ANS")