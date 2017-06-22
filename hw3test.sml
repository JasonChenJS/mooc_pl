(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)



val test1 = only_capitals["A","B","C"] = ["A","B","C"]
val test12 = only_capitals[] = []
val test13 = only_capitals["A","Bc","cA", "a"] = ["A","Bc"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 [ ] = ""
val test2c = longest_string1 [""] = ""
val test2d = longest_string1 ["A"] = "A"
val test2e = longest_string1 ["Ab","bc","C"] = "Ab"




val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 [ ] = ""
val test3c = longest_string2 [""] = ""
val test3d = longest_string2 ["A"] = "A"
val test3e = longest_string2 ["Ab","bc","C"] = "bc"


val test5 = longest_string3 ["A","bc","C"] = "bc"
val test5b = longest_string3 [ ] = ""
val test5c = longest_string3 [""] = ""
val test5d = longest_string3 ["A"] = "A"
val test5e = longest_string3 ["Ab","bc","C"] = "Ab"




val test6 = longest_string4 ["A","bc","C"] = "bc"
val test6b = longest_string4 [ ] = ""
val test6c = longest_string4 [""] = ""
val test6d = longest_string4 ["A"] = "A"
val test6e = longest_string4 ["Ab","bc","C"] = "bc"


val test7 = longest_capitalized ["A","bc","C"] = "A"
val test7b = longest_capitalized [ ] = ""
val test7d = longest_capitalized ["a"] = ""
val test7e = longest_capitalized ["A"] = "A"
val test7f = longest_capitalized ["Ab","Ac","C"] = "Ab"



val test8a = rev_string "abc" = "cba"
val test8b = rev_string "" = ""
val test8c = rev_string "a" = "a"
val test8d = rev_string "ab" = "ba"




val test9a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4



val test10a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test10b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test10c = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,3,4,5,6,7] = SOME [1]
val test10d = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1, 2,3,4,5,6,7,1] = SOME [1,1]



val test11a = count_wildcards Wildcard = 1
val test11a2 = count_wildcards (TupleP [Wildcard]) = 1

val test11b = count_wild_and_variable_lengths (Variable("a")) = 1

val test11c = count_some_var ("x", Variable("x")) = 1

val test12a = check_pat (Variable("x")) = true
val test12b = check_pat (TupleP [Wildcard, 
                                Variable "x", 
                                TupleP [], 
				ConstructorP ("y", Variable "z"),				
                                TupleP [ Variable "x"],  
			        Variable "x2"
				]
			) = false

(*
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)

val test11 = match (Const(1), UnitP) = NONE
val test1101 = match (Const(1), ConstP 1) = SOME []
val test1102 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val test1103 = match (Const(1), TupleP [Wildcard]) = NONE
val test1104 = match (Const(1), TupleP [ConstP 1]) = NONE
val test1105 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val test1106 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []
val test1107 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val test1108 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []
val test1109 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val test1110 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]])
val test1111 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]])





val test12 = first_match Unit [UnitP] = SOME []
val test1201 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]
val test1202 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]
val test1203 = first_match 
          (Tuple [Const(1), 
	          Tuple [Unit, Const(2)]
		 ]
	    ) 

	  [   (TupleP [ConstP 1, 
	               TupleP[UnitP, ConstP 3]
		      ]
	       )
	  ]
