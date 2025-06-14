let my_subset_test0 = subset [1;2] [4;3;2;1]
let my_subset_test1 = not (subset [2;4;6] [7;5;3;1])

let my_equal_sets_test0 = equal_sets [1;3;5] [5;3;1]
let my_equal_sets_test1 = not (equal_sets [1;3;4] [5;3;1])

let my_set_union_test0 = equal_sets (set_union [] [1]) [1]
let my_set_union_test1 = not (equal_sets (set_union [3;1;3] [4;4;4]) [1;2;3;4])

let my_set_all_union_test0 =
  equal_sets (set_all_union [[3;1;3]; [4;4;4]; [1;2;5;6]]) [1;2;3;4;5;6]
let my_set_all_union_test1 =
  equal_sets (set_all_union [[2]; []; [4;2]; [0;6]]) [0;2;4;6]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2 + 1) 8 = 2

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> -x) 2 2 = 2

let my_whileseq_test0 = (whileseq ((+) 1) ((>) 4) 0) = [0;1;2;3]

type animal_nonterminals =
  | Conversation | Sound | Cat | Dog | Cow | Fox

let animal_grammar =
  Conversation,
  [Cat, [T "Meow"];
   Fox, [];
   Cow, [T "Moo"];
   Dog, [T "Woof"];
   Sound, [N Cat];
   Sound, [N Cow];
   Sound, [N Dog];
   Conversation, [N Sound];
   Conversation, [N Sound; T ","; N Conversation]]

let my_filter_blind_alleys_test0 =
  filter_blind_alleys (Conversation, List.tl (List.tl (List.tl (snd animal_grammar)))) =
    (Conversation,
     [
      Dog, [T "Woof"];
      Sound, [N Dog];
      Conversation, [N Sound];
      Conversation, [N Sound; T ","; N Conversation]
     ])

