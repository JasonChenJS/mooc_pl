(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val test111 = all_except_option ("", []) = NONE
val test112 = all_except_option ("a", []) = NONE
val test113 = all_except_option ("string", []) = NONE
val test121 = all_except_option ("", [""]) = SOME []
val test122 = all_except_option ("a", [""]) = NONE
val test123 = all_except_option ("string", [""]) = NONE
val test131 = all_except_option ("", ["string"]) = NONE
val test132 = all_except_option ("a", ["string"]) = NONE
val test133 = all_except_option ("string", ["string"]) = SOME []
val test141 = all_except_option ("", ["string", "", "a", "a"]) = SOME ["string", "a", "a"]
val test142 = all_except_option ("a", ["string", "", "a", "a"]) = SOME ["string", ""]
val test143 = all_except_option ("string", ["string", "", "a", "a"]) = SOME ["", "a", "a"]
val test144 = all_except_option ("string2", ["string", "", "a", "a"]) = NONE



val test211 = get_substitutions1 ([], "") = []
val test212 = get_substitutions1 ([], "foo") = []
val test221 = get_substitutions1 ([[]], "") = []
val test222 = get_substitutions1 ([[]], "foo") = []
val test223 = get_substitutions1 ([[""]], "") = []
val test224 = get_substitutions1 ([[""]], "foo") = []
val test225 = get_substitutions1 ([["foo"]], "") = []
val test226 = get_substitutions1 ([["foo"]], "foo") = []
val test227 = get_substitutions1 ([["foo2"]], "") = []
val test228 = get_substitutions1 ([["foo2"]], "foo") = []
val test231 = get_substitutions1 ([["", "foo"]], "") = ["foo"]
val test232 = get_substitutions1 ([["", "foo"]], "foo") = [""]
val test2322 = get_substitutions1 ([["", "foo"]], "foo2") = [ ]
val test233 = get_substitutions1 ([["", ""]], "") = []
val test234 = get_substitutions1 ([["foo", "foo"]], "foo") = []
val test235 = get_substitutions1 ([["", "foo"], ["", "foo"]], "") = ["foo", "foo"]
val test236 = get_substitutions1 ([["", "foo"], ["", "foo"]], "foo") = ["", ""]
val test237 = get_substitutions1 ([["", "foo"], ["", "foo"]], "foo2") = []
val test238 = get_substitutions1 ([["", "foo", ""], ["", "foo"]], "") = ["foo", "foo"]


val test311 = get_substitutions2 ([], "") = []
val test312 = get_substitutions2 ([], "foo") = []
val test321 = get_substitutions2 ([[]], "") = []
val test322 = get_substitutions2 ([[]], "foo") = []
val test323 = get_substitutions2 ([[""]], "") = []
val test324 = get_substitutions2 ([[""]], "foo") = []
val test325 = get_substitutions2 ([["foo"]], "") = []
val test326 = get_substitutions2 ([["foo"]], "foo") = []
val test327 = get_substitutions2 ([["foo2"]], "") = []
val test328 = get_substitutions2 ([["foo2"]], "foo") = []
val test331 = get_substitutions2 ([["", "foo"]], "") = ["foo"]
val test332 = get_substitutions2 ([["", "foo"]], "foo") = [""]
val test3322 = get_substitutions2 ([["", "foo"]], "foo2") = [ ]
val test333 = get_substitutions2 ([["", ""]], "") = []
val test334 = get_substitutions2 ([["foo", "foo"]], "foo") = []
val test335 = get_substitutions2 ([["", "foo"], ["", "foo"]], "") = ["foo", "foo"]
val test336 = get_substitutions2 ([["", "foo"], ["", "foo"]], "foo") = ["", ""]
val test337 = get_substitutions2 ([["", "foo"], ["", "foo"]], "foo2") = []
val test338 = get_substitutions2 ([["", "foo", ""], ["", "foo"]], "") = ["foo", "foo"]





val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test52 = card_color (Diamonds, Ace) = Red
val test53 = card_color (Hearts, Queen) = Red
val test54 = card_color (Spades, Jack) = Black


val test6 = card_value (Clubs, Num 2) = 2
val test62 = card_value (Diamonds, Ace) = 11
val test63 = card_value (Hearts, Queen) = 10
val test64 = card_value (Spades, Jack) = 10

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test72 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]



val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test82 = all_same_color [] = true
val test83 = all_same_color [(Hearts, Ace)] = true
val test84 = all_same_color [(Diamonds, Ace), (Hearts, Ace)] = true
val test85 = all_same_color [(Spades, Ace), (Hearts, Ace)] = false
val test86 = all_same_color [(Clubs, Ace), (Hearts, Ace)] = false
val test87 = all_same_color [(Spades, Ace), (Clubs, Ace)] = true
val test88 = all_same_color [(Spades, Ace), (Diamonds, Ace)] = false
val test89 = all_same_color [(Diamonds, Ace), (Clubs, Ace)] = false



val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test92 = sum_cards [] = 0
val test93 = sum_cards [(Hearts, Ace)] = 11
val test94 = sum_cards [(Diamonds, King),(Spades, Num 10)] = 20



val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test102 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test103 = score ([(Hearts, Num 9),(Clubs, Num 4)],10) = 9
val test104 = score ([(Clubs, Num 9),(Clubs, Num 4)],10) = 4


val test11a = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12b = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13c = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             

val test14 = score_challenge ([(Hearts, Ace),(Clubs, Ace)],10) = 6
val test142 = score_challenge ([(Hearts, Ace),(Hearts, Ace)],10) = 3

val test15a = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test15b = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3
val test15c = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        21)
             = 3
val test15d = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        22)
             = 3
val test15e = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        23)
             = 1
val test15f = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        24)
             = 0
val test15g = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        25)
             = 0