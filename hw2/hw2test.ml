type math_nonterminals =
  | Expr | Operator | Operand

let math_rules =
  [Expr, [N Operand];
  Expr, [N Expr; N Operator; N Expr];
  Operand, [T "x"];
  Operand, [T "y"];
  Operand, [T "0"];
  Operand, [T "1"];
  Operand, [T "2"];
  Operand, [T "3"];
  Operand, [T "4"];
  Operand, [T "5"];
  Operand, [T "6"];
  Operand, [T "7"];
  Operand, [T "8"];
  Operand, [T "9"];
  Operator, [T"+"];
  Operator, [T"-"];
  Operator, [T"*"];
  Operator, [T"/"]]

let math_grammar = convert_grammar (Expr, math_rules)

(* 5. *)
let accept_complete suf =
  match suf with
  | [] -> Some []
  | _ -> None

let matcher_test = ((make_matcher math_grammar accept_complete ["1"; "+"; "3"; "*"; "3"; "/"; "3"]) = Some [])

(* 6. *)
let parser_test =
  ((make_parser math_grammar ["1"; "+"; "3"; "*"; "3"; "/"; "3"]) =
    Some
      (Node (Expr,
        [Node (Expr, [Node (Operand, [Leaf "1"])]); Node (Operator, [Leaf "+"]);
          Node (Expr,
          [Node (Expr, [Node (Operand, [Leaf "3"])]); Node (Operator, [Leaf "*"]);
            Node (Expr,
            [Node (Expr, [Node (Operand, [Leaf "3"])]); Node (Operator, [Leaf "/"]);
              Node (Expr, [Node (Operand, [Leaf "3"])])])])])))
