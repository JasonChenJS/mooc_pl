(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun is_in_it (s: string, ss:string list) = 
    case ss of
         [] => false
	 | x::ss' => if same_string(s, x) then true else is_in_it(s, ss')

fun remove_in_it (s: string, ss:string list) = 
    case ss of
         [] => []
	 | x::ss' => if same_string(s, x) then remove_in_it(s, ss') else x::remove_in_it(s, ss')

fun all_except_option (s:string, ss:string list) = 
    if not (is_in_it(s, ss)) then NONE else SOME (remove_in_it(s, ss))

fun appe (xl, yl) = 
    case xl of
        [] => yl
	| x::xl' => x::appe(xl', yl)

fun get_substitutions1 (ssl: string list list, s:string) = 
    case ssl of
        [] => []
	| x::ssl' =>
	   let
	       val t = get_substitutions1(ssl', s)
	   in
	       if not(is_in_it(s, x)) then t else appe(remove_in_it(s, x), t)
	   end

fun get_substitutions2 (ssl: string list list, s:string) = 
  let
    fun aux (ssl: string list list, s: string, accu: string list) =
      case ssl of
        [] => accu
	| x::ssl' =>
	   let
	       val t = if not(is_in_it(s, x)) then [] else remove_in_it(s, x)
	   in
	       aux(ssl', s, appe(accu, t))
	   end
  in
     aux(ssl, s, [])
  end

fun similar_names (ssl: string list list, {first=f, middle=m, last=z}) = 
  let
   fun fn_it (sl:string list) = 
     case sl of
         [] => []
	 | x::sl' => {first=x, last=z, middle=m}::fn_it(sl')
   in
    {first=f, last=z, middle=m}::fn_it(get_substitutions2(ssl, f))
   end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (s:suit, r:rank) = 
  case s of 
    Clubs => Black 
    | Diamonds => Red 
    | Hearts => Red 
    | Spades => Black

fun card_value (s:suit, r:rank) = 
  case r of 
    Ace => 11
    | Num i => i
    | _ => 10

fun card_value2 (s:suit, r:rank) = 
  case r of 
    Ace => 1
    | Num i => i
    | _ => 10

fun remove_card (cs: card list, c: card, e: exn) = 
    case cs of 
      [] => raise e
      | x::cs' => if x = c then cs' else x::remove_card(cs', c, e)

fun all_same_color (cs: card list) = 
    case cs of 
      [] => true
      | c::[] => true
      | c1::(c2::cs') => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')

fun sum_cards (cs: card list) = 
  let
   fun aux (cs: card list, accu) = 
    case cs of 
      [] => accu
      | c::cs' => aux(cs', card_value(c)+accu)
  in
    aux(cs, 0)
  end


fun sum_cards2 (cs: card list) = 
  let
   fun aux (cs: card list, accu) = 
    case cs of 
      [] => accu
      | c::cs' => aux(cs', card_value2(c)+accu)
  in
    aux(cs, 0)
  end


fun score (cs: card list, goal) = 
   let
      val sum = sum_cards(cs)
      val pre = if sum > goal then 3 * (sum-goal) else goal-sum
   in
      if all_same_color(cs) then pre div 2 else pre
   end

fun officiate (cs: card list, ds: move list, goal) = 
    let
      fun aux (cs: card list, ds: move list, accu_cs: card list) =
        case (cs, ds) of
	    (_, []) => score(accu_cs, goal)
	 |  (_, (Discard c)::ds') => aux(cs, ds', remove_card(accu_cs, c, IllegalMove))
         |  ([], Draw::ds') => score(accu_cs, goal)
         |  (c::cs', Draw::ds') =>
	     if sum_cards(c::accu_cs) > goal then score(c::accu_cs, goal) else aux(cs', ds', c::accu_cs)		   
    in
       aux(cs, ds, [])
    end

fun score_challenge (cs: card list, goal) = 
   let
      fun ap_less (x) = 
        let
           val d = x - goal
           val n = d div 10
        in
           x - 10*n
        end
      fun ap_more (x) =
        let
            val d = goal-x
            val n = d div 10 
        in
            x + 10*n
        end
      fun preC (x) = 
        if x > goal then 3*(x-goal) else (goal-x)
      
      val sum_more = sum_cards(cs)
      val sum_less = sum_cards2(cs)
      val pre = if sum_more < goal then preC(sum_more)
		else if sum_less > goal then preC(sum_less)
		else Int.min(preC(ap_more(sum_less)), preC(ap_less(sum_more)))

   in
      if all_same_color(cs) then pre div 2 else pre
   end

fun officiate_challenge (cs: card list, ds: move list, goal) = 
    let
      fun aux (cs: card list, ds: move list, accu_cs: card list) =
        case (cs, ds) of
	    (_, []) => score_challenge(accu_cs, goal)
	 |  (_, (Discard c)::ds') => aux(cs, ds', remove_card(accu_cs, c, IllegalMove))
         |  ([], Draw::ds') => score_challenge(accu_cs, goal)
         |  (c::cs', Draw::ds') =>
	     if sum_cards2(c::accu_cs) > goal then score_challenge(c::accu_cs, goal) else aux(cs', ds', c::accu_cs)		   
    in
       aux(cs, ds, [])
    end


(* generate moves *)
(* you can halt or draw or discard *)
fun careful_player (cs: card list, goal) =
  let
    fun tryDisAndDraw (cs: card list, (s, r)) = 
        let
	    val sum = sum_cards((s,r)::cs)
	in
	    (*bestCardToDiscard(sum-goal)*)
	    NONE
	end
    fun aux (cs, accu_hs, accu_ds) =
      case cs of 
        [] => accu_ds
      | c::cs' =>
          let 
	     val sum = sum_cards(accu_hs)
	  in
	     if sum = goal then accu_ds
	     else if goal-sum > 10 then aux(cs', c::accu_hs, Draw::accu_ds)
	     else (* peek  *)
	         if goal-sum >= card_value(c) then aux(cs', c::accu_hs, Draw::accu_ds)
		 else 
		   case tryDisAndDraw(accu_hs, c) of
		      NONE => accu_ds
		      | SOME c2 => aux(cs', c::remove_card(accu_hs, c2, IllegalMove), Draw::((Discard c2)::accu_ds))

		   
	  end
  in
     aux(cs, [], [])
  end

