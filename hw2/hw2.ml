type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* 1. *)
let convert_grammar gram1 =
  match gram1 with
  | start, rules ->
    start, fun x -> List.map Pervasives.snd (List.filter (fun rhs -> Pervasives.fst rhs = x) rules)

(* 2. *)
let rec parse_tree_leaves t =
  match t with
  | Leaf l -> [l]
  | Node (_, subtrees) -> List.concat_map parse_tree_leaves subtrees

(* 3. *)
let rec test_nt prod_func rhs acceptor frag =
  match rhs with
  | [] -> None
  | hd_rhs::tl ->
    match test_frag prod_func hd_rhs acceptor frag with
    | None -> test_nt prod_func tl acceptor frag
    | node -> node
and test_frag prod_func node acceptor frag =
  match node with
  | [] -> acceptor frag
  | _ ->
    match frag with
    | [] -> None
    | hd_frag::tl_frag ->
      match node with
      | [] -> None
      | T t::tl_node -> if hd_frag = t
                        then test_frag prod_func tl_node acceptor tl_frag
                        else None
      | N nt::tl_node -> test_nt prod_func (prod_func nt) (test_frag prod_func tl_node acceptor) frag

let make_matcher gram =
  match gram with
  | start_sym, prod_func ->
    fun acceptor frag -> test_nt prod_func (prod_func start_sym) acceptor frag

(* 4. *)
(* Simply modify the previous one to store where the suffix begins *)
let rec test_nt2 prod_func rhs acceptor frag =
  match rhs with
  | [] -> None
  | hd_rhs::tl ->
    match test_frag2 prod_func hd_rhs acceptor frag with
    | None -> test_nt2 prod_func tl acceptor frag
    | Some node -> Some (hd_rhs::node)
and test_frag2 prod_func node acceptor frag =
  match node with
  | [] -> acceptor frag
  | _ ->
    match frag with
    | [] -> None
    | hd_frag::tl_frag ->
      match node with
      | [] -> None
      | T t::tl_node -> if hd_frag = t
                        then test_frag2 prod_func tl_node acceptor tl_frag
                        else None
      | N nt::tl_node -> test_nt2 prod_func (prod_func nt) (test_frag2 prod_func tl_node acceptor) frag

let rec create_tree start rules_todo =
  match start with
  | [] -> (rules_todo, [])
  | hd_rules::tl_rules ->
    match expand_down hd_rules rules_todo with
    | n1, n2 ->
      match create_tree tl_rules n1 with
      | n3, n4 -> n3, n2::n4
and expand_down start rules_todo =
  match start, rules_todo with
  | T t, [] -> ([], Leaf t)
  | T t, rules -> (rules, Leaf t)
  | N n, [] -> ([], Node (n, []))
  | N n, hd_rules::tl_rules ->
    match create_tree hd_rules tl_rules with
    | n1, n2 -> (n1, Node (n, n2))

let complete_frag suf =
  match suf with
  | [] -> Some []
  | _ -> None

let make_parser gram =
  match gram with
  | start_sym, prod_func ->
    fun frag ->
      match test_nt2 prod_func (prod_func start_sym) complete_frag frag with
      | None -> None
      | Some [] -> None
      | Some t ->
        match create_tree [N start_sym] t with
        | _, tree ->
          match tree with
          | [] -> None
          | hd_tree::tl_tree -> Some hd_tree
