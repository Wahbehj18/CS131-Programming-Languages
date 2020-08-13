(* CS 131 Homework 1 - Code *)

(* Write a function subset a b that returns true iff a⊆b, i.e., if the set represented by the list a is a subset of the set represented by t\
he list b. Every set is a subset of itself.*)
let rec isin element list =
	match list with
	| [] -> false
	| head :: tail -> if head = element then true else isin element tail;;

let rec subset a b =
	match a with
	| [] -> true
	| head :: tail -> if (isin head b) then subset tail b else false;;

(* Write a function equal_sets a b that returns true iff the represented sets are equal.*)
let equal_sets a b = 
	subset a b && subset b a;;

(* Write a function set_union a b that returns a list representing a∪b.*)

let rec set_union a b = 
	match a with
	| [] -> b
	| head :: tail -> set_union tail (head::b);;

(* Write a function set_intersection a b that returns a list representing a∩b.*)
let rec set_intersection a b = 
	if subset a b then a
	else if subset b a then b
	else match a with
	| [] -> []
	| head :: tail -> 
		if isin head b then set_intersection (List.append tail [head]) b 
		else set_intersection tail b;; 

(* Write a function set_diff a b that returns a list representing a−b, that is, the set of all members of a that are not also members of b.*)
let set_diff a b = List.filter (fun x -> not (List.mem x b)) a;;

(* Write a function computed_fixed_point eq f x that returns the computed fixed point for f with respect to x, assuming that eq is the equality predicate for f's domain.*)
let rec computed_fixed_point eq f x = 
	if (eq (f x) x) then x
	else computed_fixed_point eq f (f x);; 

(* Write a function filter_reachable g that returns a copy of the grammar g with all unreachable rules removed. This function should preserve the order of rules: that is, all rules that are returned should be in the same order as the rules in g. *)
type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

let rec get_nonterminals rhs = 
	match rhs with
	| [] -> [] 
	| N head :: tail -> [head] @ (get_nonterminals tail)
	| T head :: tail -> get_nonterminals tail;;

let rec get_reachable_symbols g reachable =
	match g with
	| [] -> []
	| rule :: other_rules -> 
		let lhs = fst rule in
		let rhs = snd rule in
		if (List.mem lhs reachable) 
			then lhs :: (get_nonterminals rhs) @ (get_reachable_symbols other_rules (reachable @ (get_nonterminals rhs)))
		else get_reachable_symbols other_rules reachable;;

(* let rec get_unreachable_symbols g reachable = 
	match g with 
	| [] -> []
	| rule :: other_rules ->
		let lhs = fst rule in 
		let rhs = snd rule in
		if isin lhs reachable 
			then get_unreachable_symbols other_rules (reachable @ (get_nonterminals rhs))
		else get_unreachable_symbols (other_rules @ [rule]) reachable;; *)

let filter_reachable g = 
	let startsym = fst g in 
	let rules = snd g in
	let reachable_symbols = computed_fixed_point equal_sets (get_reachable_symbols rules) [startsym] in
	let filtered_rules = List.filter (fun x -> List.mem (fst x) reachable_symbols) rules in
	(startsym, filtered_rules);;

	(* let unreachable_symbols = computed_fixed_point equal_sets (get_unreachable_symbols rules) startsym in
	(startsym, (set_diff rules unreachable_symbols));; *)





