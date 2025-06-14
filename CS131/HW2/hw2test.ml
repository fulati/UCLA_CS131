let my_accept = function 
| [] -> Some []
| _ -> None

type my_nonterminals = 
  | S | VP | NP | Verb | Noun | Det

let my_grammar =
  (S, function
    | S -> [[N NP; N VP]]
    | VP -> [[N Verb]]
    | NP -> [[N Det; N Noun]]
    | Det -> [[T "my"]; [T "the"]; [T "his"]; [T "that"]]
    | Noun -> [[T "coffee"]; [T "show"]; [T "phone"]; [T "laptop"]]
    | Verb -> [[T "spilled"]; [T "rang"]; [T "broke"]; [T "started"]])

let my_frag_1 = ["his"; "laptop"; "broke"]

let make_matcher_test =
  (make_matcher my_grammar my_accept my_frag_1 = Some [])

let my_frag_2 = ["the"; "phone"; "rang"]

let make_parser_test =
  (make_parser my_grammar my_frag_2 = Some (
    Node (S, [
      Node (NP, [
        Node (Det, [Leaf "the"]);
        Node (Noun, [Leaf "phone"])
      ]);
      Node (VP, [
        Node (Verb, [Leaf "rang"])
      ])
    ])
  ))
