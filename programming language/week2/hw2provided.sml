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

fun get_substitutions2 x = 
    let fun get x = 
        case x of
             (head::tail, s) => let val z = all_except_option (s, head)
                                in 
                                    if z = NONE
                                    then get(tail, s) 
                                    else valOf(z) @ get(tail, s)  
                                end
           | _ => []
    in 
        get(x)
    end

fun similar_names (x, record) = 
    let val {first=huh, middle=wtf,last=ftw} = record
        fun r (names) =
            case (names) of
                 [] => []
               | xs::xs' => {first=xs,middle=wtf,last=ftw} :: r xs'
    in
        record :: r (get_substitutions2 (x, huh))
    end 

(* you may assume that Num is always used with values 2, 3, ..., 10 though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (card) =
    case card of 
         (Spades, _) => Black
       | (Clubs, _) => Black
       | _ => Red

fun card_value (_, rank) =
    case rank of
         Num i => i
       | Ace => 11
       | _ => 10

fun remove_card (cs, c, e) = 
    let fun init (lst) =
        case lst of
             [] => []
           | x::xs' => let val n = init(xs')  
                       in if x = c then xs' else if n <> [] then x :: init xs' else [] end
    in 
        let val n = init(cs) in if init(cs) = [] then raise e else n end
    end

fun all_same_color cardlist = 
    case cardlist of 
         x::(y::xs') => (card_color x = card_color y) andalso all_same_color (y::xs')
       | _ => true

fun sum_cards cardlist = 
    let fun cal (lst, acc) =
            case lst of 
                 [] => acc
               | x::xs' => cal (xs', acc + card_value x)
    in 
        cal (cardlist, 0)
    end

fun score (cardlst, goal) = 
    let fun prescore cardlst =
            let val s = sum_cards cardlst
            in if s > goal then (s-goal) * 3 else goal - s end
    in 
        let val pre = prescore cardlst 
        in if all_same_color cardlst then pre else pre div 2 end
    end

    (*
fun officiate (cardlst, heldlst, moves, goal) = 
    let fun current ( cardlst, heldlst, moves, acc) = 
            case moves of 
                 [] => heldlst
               | Draw::xs' => case cardlst of 
                                   [] => heldlst
                                 | y::ys' => let val s = acc + card_value y  
                                                 val xs'' = if s > goal then [] else xs'
                                             in 
                                                current (ys', y::heldlst, xs'', s)
                                             end
               | (Discard c)::xs' =>  
                 current(cardlst, remove_card(heldlst, c, IllegalMove), xs', (acc - card_value c))
    in
        score (current (cardlst, [], moves, 0), goal)
    end

    *)
