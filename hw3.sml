(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun isHeadCap s =
  let
     val c = String.sub(s, 0)
  in
     Char.isUpper(c)
  end

val only_capitals = List.filter isHeadCap

fun longer (news, accu) = 
  let
    val len1 = String.size accu
    val len2 = String.size news
  in
     if len1 < len2 then news else accu
  end

fun longer2 (news, accu) = 
  let
    val len1 = String.size accu
    val len2 = String.size news
  in
     if len1 <= len2 then news else accu
  end

val longest_string1 = List.foldl longer ""
val longest_string2 = List.foldl longer2 ""

fun longerT (news, accu, cmp) = 
  let
    val len_old = String.size accu
    val len_new = String.size news
  in
     if cmp(len_new, len_old) then news else accu
  end


fun  makeStdFoldFunc (longer2, helper) = 
    fn (news, accu) => longer2(news, accu, helper)


(* difficult as this curry function looks 'special' partial applicaion *)
fun longest_string_helper internalHelper xs = 
  (*List.foldl (makeStdFoldFunc(longerT internalHelper)) "" xs*)
  (*List.foldl (fn (news, accu) => longer2(news, accu, internalHelper) ) "" xs*)
  List.foldl (fn (news, accu) => (if internalHelper(String.size news, String.size accu) then news else accu) ) "" xs



val longest_string3 = longest_string_helper (fn (a,b) => a >  b)
val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f xs =
 case xs of 
   [] => raise NoAnswer
   | x::xs' => case (f x) of 
                NONE => first_answer f xs'
		| SOME v => v

fun all_answers f xs =
  case xs of
    [] => SOME []
    | x::xs' => case (f x) of 
                 NONE => NONE
		 | SOME xs => case (all_answers f xs') of
		              NONE => NONE
			      | SOME ys => SOME (xs@ys)

val count_wildcards  = g (fn () => 1) (fn x => 0)
(*val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x) *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (varname, p) =
    g (fn () => 0) (fn x => if x = varname then 1 else 0) p

fun check_pat p2 = 
   let 

     fun vars p = 
	case p of
	  Variable x          => [x]
	  | TupleP ps         => List.foldl (fn (p,xs) => (vars p) @ xs) [] ps
	  | ConstructorP(_,p) => vars p
	  | _                 => []
	  
      fun aux varsss =
        case varsss of
           [] => true
           | var::vars' => not (List.exists (fn x => x = var)  vars') andalso (aux vars') 
  
   in
      aux(vars p2)
   end


fun match (v, p) = 

let 
  fun hp (t1, t2) = 
      case (t1, t2) of 
        ([], []) => SOME []
	| (x::xs, y::ys) =>  (case (match(x,y), hp(xs,ys)) of
	          (NONE, NONE) => NONE
		  | (NONE, (SOME v)) => SOME v
		  | (SOME v, NONE) => SOME v
		  | (SOME v, (SOME w)) => SOME (v@w))
        | _ => NONE
	   

in

  case (v, p) of
    (x, Wildcard) => SOME []
    | (x, Variable s) => SOME [(s, x)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP i2) => if i = i2 then SOME [] else NONE
(* difficult to thinking of using ListPair.zip 
    | (Tuple t1, TupleP t2) => if List.length t1 = List.length t2 then hp(t1, t2) else NONE*)
    | (Tuple t1, TupleP t2) => if List.length t1 <> List.length t2 then NONE else (all_answers match (ListPair.zip(t1, t2)))
    | (Constructor (s, v), ConstructorP (s2, p)) => if s <> s2 then NONE else match(v,p)
    | _ => NONE
end  

(* difficult adapter *)
fun first_match v ps = 
       SOME (first_answer (fn p => match (v, p)) ps) 
        handle NoAnswer => NONE



