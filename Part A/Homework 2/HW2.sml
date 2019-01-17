(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


fun all_except_option(s, lst) =
    case lst of
	[] => NONE
      | cur :: rlst => if cur = s
		       then SOME(rlst)
		       else
			   let val r = all_except_option(s, rlst)
			   in
			       case r of
				   NONE => NONE
				 | SOME rem => SOME (cur :: rem)
			   end
      

fun get_substitutions1(lst, s) =
    case lst of
	[] => []
      | front :: tail => let val cur = all_except_option(s, front)
			 in
			     case cur of
				 NONE => get_substitutions1(tail, s)
			       | SOME ret => ret @ get_substitutions1(tail, s)
			 end

fun get_substitutions2(lst, s) =
    let fun my_get(lst, s, ans) =
	    case lst of
		[] => ans
	      | front :: tail => let val cur = all_except_option(s, front)
				 in
				     case cur of
					 NONE => my_get(tail, s, ans)
				       | SOME ret => my_get(tail, s, ans @ ret)
				 end
    in
	my_get(lst, s, [])
    end


fun similar_names(lst, {first = x, middle = y, last = z}) =
    let fun my_append(first, middle, last) =
	    case first of
		[] => []
	      | front :: tail => {first = front, middle = middle, last = last} :: my_append(tail, middle, last)
    in
	let val cur = x :: get_substitutions2(lst, x)
	in
	    my_append(cur, y, z)
	end
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

fun card_color(s, r) =
    case s of
	Clubs => Black
      | Spades => Black
      | _ => Red

fun card_value(s, r) =
    case r of
	Num x => x
      | Ace => 11
      | _ => 10
		 
fun remove_card(lst, c,  ex) =
    case lst of
	[] => raise ex
      | front :: tail => if c = front
			 then tail
			 else
			     front :: remove_card(tail, c, ex)
						 
fun all_same_color(lst) =
    case lst of
	[] => true
      | _ :: [] => true
      | a :: (b :: rest) => card_color(a) = card_color(b) andalso all_same_color(b :: rest)
										
fun sum_cards(lst) =
    let fun my_sum(lst, ans) =
	    case lst of
		[] => ans
	      | front :: tail => my_sum(tail, ans + card_value(front))
    in
	my_sum(lst, 0)
    end
	
fun score(lst, goal) =
    let val p = sum_cards(lst)
    in
	if p > goal
	then
	    if all_same_color(lst)
	    then ((p - goal) * 3) div 2
	    else p * 3
	else
	    if all_same_color(lst)
	    then (goal - p) div 2
	    else goal - p
			    
    end
	
fun officiate(deck, mlst, goal) =
    let fun simu(deck, mlst, mycard, goal, cur) =
	    case mlst of
		[] => score(mycard, goal)
	      | front :: tail => case front of
				     Discard x => simu(deck, tail, remove_card(mycard, x, IllegalMove), goal, cur - card_value(x))
				   | Draw => case deck of
						 [] => score(mycard, goal)
					       | l :: r => if card_value(l) + cur > goal
							   then score(l :: mycard, goal)
							   else
							       simu(r, tail, l :: mycard, goal, cur + card_value(l))
    in
	simu(deck, mlst, [], goal, 0)
    end
	

fun score_challenge(lst, goal) =
    let fun new_score(lst, goal, sum) =
		case lst of
		    [] => if sum > goal
			  then (sum - goal) * 3
			  else goal - sum
		  | front :: tail => let val (_, cur_rank) = front
				     in
					 case cur_rank of
					     Ace => Int.min(new_score(tail, goal, sum + 1), new_score(tail, goal, sum + 11))
					   | Num x => new_score(tail, goal, sum + x)
					   | _ => new_score(tail, goal, sum + 10)
				     end
    in
	if all_same_color(lst)
	then new_score(lst, goal, 0) div 2
	else new_score(lst, goal, 0)
    end
	

fun officiate_challenge(deck, mlst, goal) =
    let
	fun new_card_value(s, r) =
	    case r of
		Ace => 1
	      | Num x => x
	      | _ => 10
	fun simu(deck, mlst, mycard, goal, cur) =
	    case mlst of
		[] => score_challenge(mycard, goal)
	      | front :: tail => case front of
				     Discard x => simu(deck, tail, remove_card(mycard, x, IllegalMove), goal, cur - new_card_value(x))
				   | Draw => case deck of
						 [] => score_challenge(mycard, goal)
					       | l :: r => if new_card_value(l) + cur > goal
							   then score_challenge(l :: mycard, goal)
							   else
							       simu(r, tail, l :: mycard, goal, cur + new_card_value(l))
    in
	simu(deck, mlst, [], goal, 0)
    end

fun careful_player(deck, goal) =
    let fun exist(lst, value) =
	    case lst of
		[] => false
	      | front :: tail => if card_value(front) = value
				 then true
				 else exist(tail, value)

	fun find(lst, value) =
	    case lst of
		front :: [] => front
	      | front :: tail => if card_value(front) = value
				 then front
				 else find(tail, value)

	fun construct(deck, goal, sum, mycard) =
	    if sum + 10 < goal
	    then
		case deck of
		    [] => [Draw]
		  | front :: tail => Draw :: construct(tail, goal, sum + card_value(front), front :: mycard)
	    else
		if sum = goal
		then []
		else
		    case deck of
			[] => []
		      | front :: tail =>  if exist(mycard, card_value(front) + sum - goal)
					     then
						 let val abd = find(mycard, card_value(front) + sum - goal)
						 in
						     [Discard abd, Draw]
						 end
					     else
						 []
    in
	construct(deck, goal, 0, [])
    end
