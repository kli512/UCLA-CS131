let rec element_of a b =
  match b with
  | [] -> false
  | car::cdr ->
    if a = car
    then true
    else element_of a cdr

let rec subset a b =
  match a with
  | [] -> true
  | car::cdr ->
    if element_of car b
    then subset cdr b
    else false

let equal_sets a b =
  subset a b && subset b a

let rec set_union a b =
  match a with
  | [] -> b
  | car::cdr ->
    if element_of car b
    then set_union cdr b
    else set_union cdr (car::b)

let rec set_intersection a b =
  match a with
  | [] -> []
  | car::cdr ->
    if element_of car b
    then car::set_intersection cdr b
    else set_intersection cdr b

let rec set_diff a b =
  match a with
  | [] -> []
  | car::cdr ->
    if element_of car b
    then set_diff cdr b
    else car::set_diff cdr b

let rec computed_fixed_point eq f x =
  if eq x (f x) then x
  else computed_fixed_point eq f (f x)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec valid_end_symbols end_symbols =
  match end_symbols with
  | [] -> []
  | car::cdr ->
    match car with
    | N car_nt -> car_nt::valid_end_symbols cdr
    | T car_t -> valid_end_symbols cdr

let rec neighbors nt rules =
  match rules with
  | [] -> []
  | car::cdr ->
    match car with
    | start_symbol, end_symbols ->
      if start_symbol = nt
      then set_union (valid_end_symbols end_symbols) (neighbors nt cdr)
      else neighbors nt cdr

let rec expand fringe rules =
  match fringe with
    | [] -> []
    | car::cdr -> set_union (neighbors car rules) (expand cdr rules)

let filter_reachable g =
  match g with
  | start, rules ->
    let reachable_nt = computed_fixed_point equal_sets (fun fringe -> set_union fringe (expand fringe rules)) [start] in
      start, List.filter (fun (nt, t) -> element_of nt reachable_nt) rules
