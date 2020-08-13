(* CS 131 - Homework 2 *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Write a function convert_grammar gram1 that returns a Homework 2-style grammar, 
which is converted from the Homework 1-style grammar gram1. *)

let rec merge_rules rules nonterminal = match rules with 
	| [] -> []
	| first_rule::other_rules -> match first_rule with 
		| (symbol, expression) -> 
			(* skip if terminal, otherwise append and recurse *)
			if symbol = nonterminal then expression::(merge_rules other_rules nonterminal)
			else merge_rules other_rules nonterminal;;

let convert_grammar gram1 = match gram1 with
	| (startsym, rules) -> 
		(* returns a function that takes in the nonterminal of interest *)
		(startsym, fun nonterminal -> merge_rules rules nonterminal);;

(* write a function parse_tree_leaves tree that traverses the parse tree tree 
left to right and yields a list of the leaves encountered *)

let rec traverse_tree = function
	| [] -> []
	| head::tail -> match head with 
		(* if node, traverse the node's subtree then continue w remaining tree *)
		| Node (nonterminal, subtree) -> (traverse_tree subtree) @ (traverse_tree tail)
		(* if leaf, append and continue *)
		| Leaf leaf -> leaf::(traverse_tree tail);;

let parse_tree_leaves tree = traverse_tree [tree];;

(* Write a function make_matcher gram that returns a matcher for the grammar gram. 
When applied to an acceptor accept and a fragment frag, the matcher must try 
the grammar rules in order and return the result of calling accept on the suffix 
corresponding to the first acceptable matching prefix of frag. *)

let rec call_accept_on_frag rules rules_func accept frag = 

	(* helper function! use me to find fragment/rule matches *)
	let rec check_frag_match rule rules_func accept frag = (match rule with
	| [] -> accept frag
	| _ -> (match frag with
		| [] -> None
		| frag_head::frag_tail -> 
			match rule with
			| [] -> None
			| (N n)::rule_tail -> 
				(* if nonterminal, recurse to apply this new rule layer *)
				call_accept_on_frag (rules_func n) rules_func 
				(check_frag_match rule_tail rules_func accept) frag
			| (T t)::rule_tail -> 
				(* if terminal, check if it matches the fragment prefix. 
				   if matches, continue w rest of fragment, otherwise drop *)
				if t = frag_head then check_frag_match rule_tail rules_func accept frag_tail
				else None)) in 

	match rules with 
	| [] -> None
	| first_rule::other_rules -> 
		match check_frag_match first_rule rules_func accept frag with
			(* if check_frag_match finds a match, return it! otherwise try again w other rules *)
			| None -> call_accept_on_frag other_rules rules_func accept frag
			| res -> res;;

let make_matcher gram = match gram with
	| (startsym, rules_func) -> 
		fun accept frag -> call_accept_on_frag (rules_func startsym) rules_func accept frag;;

(* Write a function make_parser gram that returns a parser for the grammar gram. 
When applied to a fragment frag, the parser returns an optional parse tree. 
If frag cannot be parsed entirely (that is, from beginning to end), the parser returns None. 
Otherwise, it returns Some tree where tree is the parse tree corresponding to the input fragment. *)
(* let rec make_opt_parse_tree 

let parse_frag rules rules_func frag = 
	let parsable = make_matcher gram accept_empty_suffix frag in 
	match parsable with 
		| None -> None
		| _ -> 


let make_parser gram = match gram with 
	| (startsym, rules_func) ->
		fun frag -> parse_frag (rules_func startsym) rules_func;;

 *)