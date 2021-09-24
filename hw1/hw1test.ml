let subset_test0 = subset [] []
let subset_test1 = subset [1;1;1] [1]
let subset_test2 = not (subset [1;2;3;4;5;6] [1;2;3;4;5;7;8;9])

let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1;2;3] [1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;3]
let equal_sets_test2 = not (equal_sets [1] [2])
let equal_sets_test3 = not (equal_sets [1;2] [2])

let set_union_test0 = equal_sets (set_union [] []) []
let set_union_test1 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [1;2;3] []) [1;2;3]
let set_union_test3 = equal_sets (set_union [1;1;1] [1;2;3;1;1;1]) [1;2;3]
let set_union_test4 = equal_sets (set_union [1;2;3;4] [5;6;7;8;9]) [1;2;3;4;5;6;7;8;9]

let set_intersection_test0 =
  equal_sets (set_intersection [] []) []
let set_intersection_test1 =
  equal_sets (set_intersection [] [1;2;3]) []
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;3] []) []
let set_intersection_test3 =
  equal_sets (set_intersection [1;2;3;4;5;4] [1;2;3;4;5]) [1;2;3;4;5]
let set_intersection_test4 =
  equal_sets (set_intersection [1;1;1;1;1;1;1;1;] [3;1;2;4]) [1]

let set_diff_test0 = equal_sets (set_diff [] []) []
let set_diff_test1 = equal_sets (set_diff [32] [1;2;3213;215;3]) [32]
let set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []
let set_diff_test4 = equal_sets (set_diff [1;2;3;4;5;6;7;8;9] [1;2;3;4;5;6;7;8;9]) []

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x) 10 = 10
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. x) 3. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) (fun x -> x / 2 + 1) 10000 = 2
let computed_fixed_point_test3 =
  computed_fixed_point (fun x y -> x + y * y > 1000) (fun x -> x + 1) 0 = 31

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

let math_test0 =
  filter_reachable (Expr, math_rules) = (Expr, math_rules)

let math_test1 =
  filter_reachable (Expr, List.tl math_rules) =
    (Expr,
    [Expr, [N Expr; N Operator; N Expr];
      Operator, [T"+"];
      Operator, [T"-"];
      Operator, [T"*"];
      Operator, [T"/"]])

let math_test2 =
  filter_reachable (Expr, List.hd math_rules::List.tl (List.tl math_rules)) =
    (Expr,
    [Expr, [N Operand];
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
      Operand, [T "9"]])

let math_test3 =
  filter_reachable (Expr, List.tl (List.tl math_rules)) =
    (Expr,
    [])
