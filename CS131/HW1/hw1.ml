type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* 1. subset a b function *)
let rec subset a b = match a with
| [] -> true
| h :: t -> if List.mem h b
  then subset t b 
  else false

(* 2. equal_sets a b function *)
let equal_sets a b = subset a b && subset b a

(* 3. set_union a b function *)
let set_union a b = List.merge compare a b

(* 4. set_all_union a function *)
let rec set_all_union a = match a with
| [] -> []
| h :: t -> set_union h (set_all_union t)

(* 5. Russell's Paradox: the self_member s function cannot be written in OCaml because 
lists are homogenous in OCaml, meaning that all elements must have the same type. 
So it is not possible for a list to contain both elements of type 'a as well as elements 
of type 'a list, which is itself. The paradox is a contradiction. *)

(* 6. computed_fixed_point eq f x function *)
let rec computed_fixed_point eq f x = 
if eq (f x) x 
then x
else computed_fixed_point eq f (f x)

(* 7. computed_periodic_point eq f p x function *)
let rec periodic_helper f p x = match p with
| 1 -> (f x)
| _ -> (periodic_helper f (p - 1) (f x))

let rec computed_periodic_point eq f p x = match p with
| 0 -> x
| 1 -> computed_fixed_point eq f x
| _ -> if eq (periodic_helper f p x) x
then x
else computed_periodic_point eq f p (f x)

(* 8. whileseq s p x function *)
let rec whileseq s p x = 
if not (p x) then []
else x :: (whileseq s p (s x))


(*------------------------------------------*)
(*Blind Alley Function*)

(* Check if RHS of a rule are terminable *)
let rhs_terminable rhs terminable_list = 
List.for_all (function
| T _ -> true
| N nt -> List.mem nt terminable_list) rhs

(* Iterate through rules to get the terminable nonterminals *)
let rec build_terminable_list rules terminable_list = match rules with
| [] -> terminable_list
| (lhs, rhs) :: t -> if rhs_terminable rhs terminable_list && not (List.mem lhs terminable_list)
then build_terminable_list t (lhs :: terminable_list)
else build_terminable_list t terminable_list

(* Combine the rules with the terminable list as pairs *)
let build_terminable_rule (rules, terminable_list) = rules, build_terminable_list rules terminable_list

(* Filter out rules whose RHS is not terminable *)
let rec filter_terminable rules terminable_list new_rules = match rules with
| [] -> new_rules
| (lhs, rhs) :: t -> if rhs_terminable rhs terminable_list
then filter_terminable t terminable_list (new_rules@[(lhs, rhs)])
else filter_terminable t terminable_list new_rules

(* 9. filter_blind_alleys g function *)
let filter_blind_alleys g = match g with
| start_sym, all_rules -> 
(start_sym, filter_terminable all_rules 
  (snd (computed_fixed_point 
          (fun (_, b1) (_, b2) -> equal_sets b1 b2) 
          build_terminable_rule 
          (all_rules, [])))
  [])

