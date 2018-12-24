fun is_older(a: int * int * int, b: int * int * int) =
    if #1 a < #1 b
    then true
    else
	if #1 a > #1 b
	then false
	else
	    if #2 a < #2 b
	    then true
	    else
		if #2 a > #2 b
		then false
		else
		    #3 a < #3 b

fun number_in_month(date : (int * int * int) list, month : int) =
    if null date
    then 0
    else
	 let val cur = hd date
	 in
	     if #2 cur = month
	     then 1 + number_in_month(tl date, month)
	     else number_in_month(tl date, month)
	 end
	    	
fun number_in_months(date : (int * int * int) list, months : int list) =
    if null months
    then 0
    else
	number_in_month(date, hd months) + number_in_months(date, tl months)
							   
fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	let val cur = hd dates
	in
	    if #2 cur = month
	    then cur :: dates_in_month(tl dates, month)
	    else dates_in_month(tl dates, month)
	end

fun dates_in_months(dates : (int * int * int) list, months: int list) =
    if null months
    then []
    else
	let val cur = hd months
	in
	    dates_in_month(dates, cur) @ dates_in_months(dates, tl months)
	end
	   
fun get_nth(a : string list, n : int) =
    if n = 1
    then hd a
    else get_nth(tl a, n - 1)

fun date_to_string(date : int * int * int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	let val month =get_nth(months, #2 date)
	in
	    let val day = Int.toString(#3 date)
	    in
		let val year = Int.toString(#1 date)
		in month ^ " " ^ day ^ ", " ^ year
		end
	    end
	end
    end

fun number_before_reaching_sum(sum : int, a : int list) =
    let val cur = hd a
    in
	if cur >= sum
	then 0
	else 1 + number_before_reaching_sum(sum - cur, tl a)
    end
	
fun what_month(day : int) =
    if day > 334
    then 12
    else
	let val months = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
	in
	    number_before_reaching_sum(day, months)
	end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	what_month(day1) :: month_range(day1 + 1, day2)

	
fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let fun old_nempty(dates : (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else
		    let val later = old_nempty(tl dates)
		    in
			if is_older(hd dates, later)
			then hd dates
			else later
		    end
	in
	    SOME(old_nempty(dates))
	end

fun exist(a : int list, num : int) =
    if a = []
    then false
    else
	if hd a = num
	then true
	else exist(tl a, num)

fun remove_multiple(a : int list, b : int list) =
    if a = []
    then []
    else
	if exist(b, hd a)
	then remove_multiple(tl a, b)
	else
	    (hd a) :: remove_multiple(tl a, (hd a) :: b)

fun number_in_months_challenge(date : (int * int * int) list, months : int list) =
    let val new_months = remove_multiple(months, [])
    in
	number_in_months(date, new_months)
    end

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_multiple(months, []));

fun reasonable_date(date : (int * int * int)) =
    let val year = #1 date
    in
	let val month = #2 date
	in
	    let val day = #3 date
	    in
		if year < 1 orelse month < 1 orelse month > 12 orelse day < 1
		then false
		else
		    if exist([1, 3, 5, 7, 8, 10, 12], month)
		    then
			day <= 31
		    else
			if day > 30
			then false
			else
			    if month = 2
			    then
				if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
				then
				    day <= 29
				else
				    day <= 28
			    else
				true
	    end
	end
    end
