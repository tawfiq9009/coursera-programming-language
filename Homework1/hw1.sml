(* Homework 1 - Programming Language Course Part A *)
(* Written by Tawfiq Hamed *)

(* check if first_date before than second_date *)
fun is_older(first_date : int*int*int, second_date: int*int*int)=
  if (#1 first_date) < (#1 second_date) then
      true
  else if (#1 first_date) = (#1 second_date) then
      if (#2 first_date) < (#2 second_date)
      orelse (#2 first_date) = (#2 second_date)
	 andalso (#3 first_date) < (#3 second_date) then
	  true
      else
	  false
  else
      false

(* count number of dates has a passed month *)
fun number_in_month(dates : (int*int*int) list, month : int)=
  if null dates then 0
  else
      let
	  val count = if (#2 (hd dates)) = month
	  then 1
	  else 0					   
      in
	  count + number_in_month(tl dates, month)
      end

(* count number of dates has any month in list *)
fun number_in_months(dates : (int*int*int) list, months : int list)=
  if null dates orelse null months then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* return list of dates at a specific month *)
fun dates_in_month(dates : (int*int*int) list, month : int)=
  if null dates then []
  else
      if (#2 (hd dates)) = month
      then (hd dates)::dates_in_month((tl dates), month)
      else dates_in_month((tl dates), month)

(* return list of dates in any month in list *)
fun dates_in_months(dates : (int*int*int) list, months: int list)=
  if null dates orelse null months then []
  else
      dates_in_month(dates, hd months)@dates_in_months(dates, tl months)


(* get nth element in list *)
fun get_nth(list_of_strings : string list, element : int)=
  if element = 1 then (hd list_of_strings)
  else get_nth(tl list_of_strings, element - 1)

(* convert date to string *)
fun date_to_string(date:int*int*int)=
  let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
      get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* get number before reaching sum *)
fun number_before_reaching_sum(sum : int, numbers: int list)=
  let fun sum_numbers(current_sum:int, last_number:int,  numbers: int list) =
	if sum > current_sum + (hd numbers)
	then sum_numbers(current_sum + (hd numbers), last_number+1, tl numbers)
	else last_number
  in
      sum_numbers(0, 0, numbers)
  end

(* get month by day *)
fun what_month(day : int)=
  number_before_reaching_sum(day, [0,31,29,31,30,31,30,31,31,30,31,30,31])
     
(* get list of month range *)
fun month_range(day1:int, day2:int)=
  if day1 > day2 then []
  else
      what_month(day1)::month_range(day1+1, day2)

(* get oldest date *)
fun oldest(dates:(int*int*int) list)=
  if null dates then NONE
  else
      let fun get_oldest(old_date, dates:(int*int*int) list)=
            if null dates then SOME(old_date)
	    else if is_older(old_date, (hd dates))
            then get_oldest(old_date, tl dates)
	    else get_oldest(hd dates, tl dates)
      in
	  get_oldest(hd dates, tl dates)
      end

(* Challenges *)
fun clean_list(months_list: int list)=
  if null months_list then []
  else
      let val current_list = List.filter(fn y => y <> (hd months_list)) months_list
  in
   (hd months_list)::clean_list(current_list)
  end
	  
fun number_in_months_challenge(dates : (int*int*int) list, months : int list)=
  number_in_months(dates, clean_list(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list)=
  dates_in_months(dates, clean_list(months))


fun reasonable_date(date:int*int*int)=
  if (#1 date) > 0 andalso (#2 date) > 0 andalso (#2 date) <= 12
  then
      let
	  fun is_leap(year:int)=
	    if (year mod 4) <> 0 then false
	    else if (year mod 100) <> 0 then true
	    else if (year mod 400) <> 0 then false
	    else
		true

	  fun get_max_day(list_of_days : int list, element : int)=
	    if element = 1 then (hd list_of_days)
	    else get_max_day(tl list_of_days, element - 1)

      in
	  if (#2 date) = 2 andalso is_leap(#1 date)
	  then
	      if (#3 date) < 29 andalso (#3 date) > 0
	      then true
	      else false
	  
          else
	      let val max_days = get_max_day([31,29,31,30,31,30,31,31,30,31,30,31], (#2 date))
	      in
		  if (#3 date) <= max_days andalso (#3 date) > 0
		  then true
		  else false
	      end
      end
  else
      false
