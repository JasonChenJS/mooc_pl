(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


use "hw1.sml";

val test111 = is_older ((1,2,3),(2,3,4)) = true
val test112 = is_older ((1,2,3),(2,3,3)) = true
val test113 = is_older ((1,2,3),(2,3,2)) = true

val test114 = is_older ((1,2,3),(2,2,4)) = true
val test115 = is_older ((1,2,3),(2,2,3)) = true
val test116 = is_older ((1,2,3),(2,2,2)) = true

val test117 = is_older ((1,2,3),(2,1,4)) = true
val test118 = is_older ((1,2,3),(2,1,3)) = true
val test119 = is_older ((1,2,3),(2,1,2)) = true


val test121 = is_older ((1,2,3),(0,3,4)) = false
val test122 = is_older ((1,2,3),(0,3,3)) = false
val test123 = is_older ((1,2,3),(0,3,2)) = false

val test124 = is_older ((1,2,3),(0,2,4)) = false
val test125 = is_older ((1,2,3),(0,2,3)) = false
val test126 = is_older ((1,2,3),(0,2,2)) = false

val test127 = is_older ((1,2,3),(0,1,4)) = false
val test128 = is_older ((1,2,3),(0,1,3)) = false
val test129 = is_older ((1,2,3),(0,1,2)) = false

val test131 = is_older ((1,2,3),(1,3,4)) = true
val test132 = is_older ((1,2,3),(1,3,3)) = true
val test133 = is_older ((1,2,3),(1,3,2)) = true

val test134 = is_older ((1,2,3),(1,2,4)) = true
val test135 = is_older ((1,2,3),(1,2,3)) = false
val test136 = is_older ((1,2,3),(1,2,2)) = false

val test137 = is_older ((1,2,3),(1,1,4)) = false
val test138 = is_older ((1,2,3),(1,1,3)) = false
val test139 = is_older ((1,2,3),(1,1,2)) = false


val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test22 = number_in_month ([(2012,2,28),(2013,12,1), (2011,2,20), (2011, 0, 50)],2) = 2


val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test32 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test33 = number_in_months ([(2012,2,28),(2013,12,1),(2011,2,31),(2011,4,28)],[2]) = 2
val test34 = number_in_months ([(2012,2,28),(2013,4,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 4



val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test42 = dates_in_month ([(2012,2,28),(2013,12,1), (2000, 2, 29)],2) = [(2012,2,28), (2000, 2, 29)]



val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test52 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test53 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4]) = [(2011,4,28)]
val test54 = dates_in_months ([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)]




val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test62 = get_nth ([], 2) = ""
val test63 = get_nth (["hi", "there", "how", "are", "you"], 0) = ""
val test64 = get_nth (["hi", "there", "how", "are", "you"], ~1) = ""
val test65 = get_nth (["hi", "there", "how", "are", "you"], 6) = ""
val test66 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
val test67 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"



val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test82 = number_before_reaching_sum (10, []) = 0
val test83 = number_before_reaching_sum (10, [1,1,1,1,1]) = 5
val test84 = number_before_reaching_sum (1, [1,2,3,4,5]) = 0

val test9 = what_month 70 = 3
val test911 = what_month 1 = 1
val test912 = what_month 31 = 1
val test921 = what_month 32 = 2
val test922 = what_month 59 = 2
val test931 = what_month 60 = 3
val test932 = what_month 90 = 3
val test941 = what_month 91 = 4
val test942 = what_month 120 = 5
val test951 = what_month 121 = 5
val test952 = what_month 151 = 5
val test961 = what_month 152 = 6
val test962 = what_month 181 = 6
val test971 = what_month 182 = 7
val test972 = what_month 212 = 7
val test981 = what_month 213 = 8
val test982 = what_month 243 = 8
val test991 = what_month 244 = 9
val test992 = what_month 273 = 9
val test9101 = what_month 274 = 10
val test9102 = what_month 304 = 10
val test9111 = what_month 305 = 11
val test9112 = what_month 334 = 11
val test9121 = what_month 335 = 12
val test9122 = what_month 365 = 12



val test10 = month_range (31, 34) = [1,2,2,2]
val test102 = month_range (31, 30) = []
val test103 = month_range (31, 31) = [1]


val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test112 = oldest([(2012,2,28),(2012,2,28),(2012,3,28)]) = SOME (2012,2,28)
val test113 = oldest([]) = NONE
val test114 = oldest([(2012,2,28)]) = SOME (2012,2,28)

