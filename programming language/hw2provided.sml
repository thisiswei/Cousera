(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option x =
    case x of 
         (s, head::tail) => let
                                val z = all_except_option(s, tail)
                            in    
                                if same_string(s, head)           
                                then SOME(tail)
                                else if z <> NONE
                                then SOME(head::valOf z)
                                else NONE
                            end
         | _ => NONE





fun get_substitutions1 x = 
    case x of
         (head::tail, s) => let val z = all_except_option (s, head)
                            in 
                                if z = NONE
                                then get_substitutions1(tail, s) 
                                else valOf(z) @ get_substitutions1(tail, s)  
                            end
       | _ => []
                            

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
