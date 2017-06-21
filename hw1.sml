fun is_older ( dt1:(int * int * int), dt2:(int * int * int) ) =
    if #1 dt1 < #1 dt2 
    then true 
    else if #1 dt1 > #1 dt2 
    then false
    else

    if #2 dt1 < #2 dt2 
    then true 
    else if #2 dt1 > #2 dt2 
    then false
    else

    if #3 dt1 < #3 dt2 
    then true 
    else if #3 dt1 > #3 dt2 
    then false
    else

    false

fun number_in_month (dts: (int * int * int) list, m:int) = 
    if null dts then 0 else
    let 
        val t = if #2 (hd dts) = m then 1 else 0
    in 
        t + number_in_month(tl dts, m)
    end

fun number_in_months (dts: (int * int * int) list, ms: int list) = 
    if null ms then 0 else
    number_in_month(dts, hd ms) + number_in_months(dts, tl ms)

fun dates_in_month (dts: (int * int * int) list, m:int) = 
    if null dts then [] else
    let
        val h = hd dts
        val hm = #2 (hd dts)
        val t = dates_in_month(tl dts, m)
    in 
        if hm = m then h::t else t 
    end

fun combine(ls1:(int * int * int) list, ls2:(int * int * int) list) = 
    if null ls1 then ls2 else
    (hd ls1)::combine(tl ls1, ls2)

fun dates_in_months (dts: (int * int * int) list, ms: int list) = 
    if null ms then [] else
    combine(dates_in_month(dts, hd ms), dates_in_months(dts, tl ms))

fun get_nth (ls: string list, n:int) = 
    if null ls orelse n < 1 then "" else
(*    if n < 1 then "" else *)
    if n = 1 then hd ls else get_nth(tl ls, n-1)

fun date_to_string (dti:(int * int * int)) = 
    let
        val months = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 dti) ^ " " ^ Int.toString(#3 dti) ^ ", " ^ Int.toString(#1 dti)
    end

fun number_before_reaching_sum (s:int, ls:int list) = 
    if null ls then 0 else
    if s <= hd ls then 0 else 1 + number_before_reaching_sum(s - hd ls, tl ls)

fun what_month (d: int) = 
    let
        val days = [31,28,31,30,31,30, 31,31,30,31,30,31]
    in
        number_before_reaching_sum(d, days) + 1
    end

fun month_range (d1:int, d2:int) = 
    if d1 > d2 then [] else
    what_month(d1)::month_range(d1+1, d2)

fun oldest (dls: (int * int * int) list) = 
    if null dls then NONE else
    let
        val dt1 = hd dls	
    in
        if null (tl dls) then SOME dt1 else 
	let
	    val SOME_dt2 = oldest(tl dls)
	in 
	    (*SOME dt1*)
	    if is_older(dt1, valOf SOME_dt2) then SOME dt1 else SOME_dt2
	end
    end