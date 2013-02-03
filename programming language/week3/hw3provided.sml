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
val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0)))

val longest_string1 = foldl (fn (x, y) => if String.size x > String.size y then x else y) "" 

val longest_string2 = foldl (fn (y, x) => if String.size x > String.size y then x else y) "" 


fun longest_string_helper cmp = List.foldl (fn (x,y) => if cmp(String.size x,
                                                               String.size y) then x else y) ""

val longest_string_3 = longest_string_helper (op >)
val longest_string_4 = longest_string_helper (op >=)

val longest_capitalized = longest_string_3 o only_capitals 
val rev_string = String.implode o List.rev o String.explode

fun first_answer f = 
    let fun first y =                                              
            case y of                                              
                 ys::ys' => case f ys of                           
                                 SOME v => v                   
                               | NONE => first ys'                 
                 | _ => raise NoAnswer                             
    in first end                                  
                                                  
