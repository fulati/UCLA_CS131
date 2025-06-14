type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* 1. convert_grammar gram1 function *)
let rec group_rules_list symbol rule_list = match rule_list with 
| [] -> []
| (lhs, rhs) :: t -> if lhs = symbol 
                     then rhs :: group_rules_list symbol t
                     else group_rules_list symbol t

let convert_grammar gram1 = let (start_sym, rule_list) = gram1 in 
(start_sym, fun symbol -> group_rules_list symbol rule_list)


(* 2. parse_tree_leaves tree function *)
let rec parse_tree_leaves tree = match tree with
| Leaf terminal -> [terminal]
| Node (_, subtree) -> List.flatten (List.map parse_tree_leaves subtree)


(* 3. make_matcher gram function *)
let match_terminal t accept frag = match frag with
| [] -> None
| head :: tail -> if head = t 
                  then accept tail
                  else None

let rec match_rule prod_func rule accept frag = match rule with
| [] -> accept frag
| sym :: tail_sym -> let new_accept = match_rule prod_func tail_sym accept in 
      match sym with
      | T t -> match_terminal t new_accept frag
      | N nt -> match_rules prod_func (prod_func nt) new_accept frag

and match_rules prod_func rule_list accept frag = match rule_list with
| [] -> None
| rule :: rest -> match match_rule prod_func rule accept frag with
        | Some x -> Some x
        | None -> match_rules prod_func rest accept frag

let make_matcher gram = let (start_symbol, prod_func) = gram in
    fun accept frag -> let start_prod_rules = prod_func start_symbol in
        match_rules prod_func start_prod_rules accept frag


(* 4. make_parser gram function *)
let parser_accept frag tree = match frag with
| [] -> Some tree
| _ -> None

let rec parse_terminal prod_func current_symbol t tail_sym accept frag subtrees = match frag with
| [] -> None
| head :: tail -> if head = t 
                  then parse_rule prod_func current_symbol tail_sym accept tail (subtrees @ [Leaf t])
                  else None

and parse_rule prod_func current_symbol rule accept frag subtrees = match rule with
| [] -> accept frag (Node(current_symbol, subtrees))
| sym :: tail_sym -> match sym with
      | T t -> parse_terminal prod_func current_symbol t tail_sym accept frag subtrees
      | N nt -> let new_accept tail_frag new_tree = parse_rule prod_func current_symbol tail_sym accept tail_frag (subtrees @ [new_tree]) in
                parse_rules prod_func nt (prod_func nt) new_accept frag []

and parse_rules prod_func current_symbol rules accept frag subtrees = match rules with
| [] -> None
| rule :: rest -> match parse_rule prod_func current_symbol rule accept frag subtrees with
      | Some x -> Some x
      | None -> parse_rules prod_func current_symbol rest accept frag subtrees

let make_parser gram = let (start_symbol, prod_func) = gram in
    fun frag -> let start_prod_rules = prod_func start_symbol in
        parse_rules prod_func start_symbol start_prod_rules parser_accept frag []
