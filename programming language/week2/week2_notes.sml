
val my_niece = {name = "amelia", id = 4122};
#id my_niece

val brain_part = {id = true, ego=false, superego=false};

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f(x : mytype) = 
    case x of
         Pizza => 3
       | Str s => 8
       | TwoInts(i1, i2) => i1 + i2

fun fu x = 
    case x of 
         Pizza => 3
       | TwoInts(i1, i2) => i1 + i2
       | Str s => String.size s


fun haha x = 
    case x of
         Pizza => 4
        |TwoInts(i1, i2) => i1~i2
        |Str s => String.size s

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int
datatype id = StudentNum of int
            | Name of string
                      * (string option)
                      * string

datatype exp = Constant of int
             | Negate of exp
             | Add    of exp * exp
             | Multiply of exp * exp

Add (Constant (10 + 9), Negate (Constant 4))


fun eval e = 
    case e of 
         Constant i => i
       | Negate e2  => ~ (eval e2)
       | Add(e1, e2) => (eval e1) + (eval e2)
       | Multiply(e1, e2) => (eval e1) * (eval e2)

fun number_of_adds e = 
    case e of 
         Constant i => 0
       | Negate e2 => number_of_adds e2
       | Add(e1, e2) => 1 + number_of_adds e1 + number_of_adds e2
       | Multiply(e1, e2) => number_of_adds e1 + number_of_adds e2

val example_exp : exp = Add (Constant 19, Negate (Constant 4))

val example_ans : int = eval example_exp

val example_addcount = number_of_adds (Multiply(example_exp, example_exp))



datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiple of exp * exp

fun max_constant e =
    case e of 
         Constant i => i
        |Negate e2 => max_constant e2
        |Add(e1, e2) =>  Int.max(max_constant e1, max_constant e2)
        |Multiple(e1, e2) => Int.max(max_constant e1, max_constant e2)



val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp


(* Type Synonyms *)

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank
type name_record = { student_num : int option,
                     first       : string,
                     middle      : string option,
                     last        : string }

fun is_Queen_of_Spades (c : card) = 
    #1 c = Spade andalso #2 c = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 : suit * rank = (Spade, Ace)

fun is_Queen_of_Spades2 c = 
    case c of 
         (Spade, Queen) => true
        | _ => false

(* list and options are datatypes *)

datatype my_int_list = Empty
                      | Cons of int * my_int_list

val x = Cons(4, Cons(23, Cons(2008, Empty)))

fun append_my_list (xs, ys) = 
    case xs of 
         Empty => ys
        |Cons(x, xs') => Cons(x, append_my_list(xs', ys)

fun inc_or_zero intoption = 
    case intoption of
         None => 0
        |SOME i => i+1

fun sum_list xs = 
    case xs of
         [] => 0
        | x::xs' => x + sum_list xs'

fun append (xs, ys) = 
    case xs of
         [] => ys
        | x::xs' => x :: append(xs', ys)

(* polymorphic datatype *)

datatype 'a option = NONE | SOME of 'a
datatype 'a mylist = Empty | Cons of 'a * 'a mylist
datatype ('a, 'b) tree =
         Node of 'a * ('a, 'b) tree * ('a, 'b) tree
        |Leaf of 'b

fun sum_tree tr = 
    case tr of
         Leaf i => i
        |Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

fun sum_leaves tr = 
    case tr of 
         Leaf i => i
        |Node(i, lft, rgt) => sum_leaves lft + sum_leaves rgt

fun num_leaves tr =
    case tr of
         Leaf i => i
        |Node(i,lft,rgt) => num_leaves lft + num_leaves rgt


(* jan 26 2013 *)

(* 'a list * 'a list -> 'a list *)
fun append (xs,ys) =
    case xs of
         [] => ys
       | x::xs' => x :: append(xs',ys)

fun old_zip3 (l1, l2, l3) = 
    if null l1 andalso null l2 andalso null l3
    then []
    else if null l1 orelse null l2 orelse null l3
    then raise ListLengthMismatch
    else (hd l1, hd l2, hd l3) :: old_zip3(tl l1, tl l2, tl l3)

fun zip3 list_triple =
    case list_triple of
         ([],[],[]) => []
       | (hd1::tl1,hd2::tl2,h3::tl3) => (hd1,hd2,h3) :: zip3(tl1,tl2,tl3)
       | _ => raise ListLengthMismatch

fun unzip3 lst = 
    case lst of =
        [] => ([],[],[])
      | (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
                       in
                           (a::l1,b::l2,c::l3)
                       end

fun nondecreasing xs =
    case xs of
         [] => true
       | _::[] => true
       | head::(neck::rest) => head <= neck
                               andalso nondecreasing (neck::rest)

datatype sgn = P | N | Z

fun multsign (x1,x2) = 
    let fun sign x = if x=0 then Z else if x>0 then P else N
    in
        case (sign x1, sign x2) of
             (Z,_) => Z
           | (_,Z) => Z
           | (N,N) => P
           | (P,P) => P
           | _ => N
    end

fun len xs =
    case xs of
         [] => 0
       | _::xs' => 1 + len xs'


datatype exp = Constant of int
            | Negate of exp
            | Add of exp * exp
            | Multiply of exp *exp

fun old_eval e = 
    case e of
         Constant i => i
       | Negate e2 => ~ (old_evl e2)
       | Add(e3,e4) => (old_evl e3) + (old_evl e4)
       | Multiply(e3,e4) => (old_evl e3) * (eval e4)

fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add(e1,e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1,e2)) = (eval e1) * (eval e2)

fun append([],ys) = ys
  | append(x::xs',ys) = x::append(xs',ys)

fun hd xs =
    case xs of
         [] => raise List.Empty
       | x::_ => x

exception AnyNameIwant
exception AnyException of int * int
raise AnyException(3*3)

fun mydiv (x,y) =
    if y=0
    then raise AnyNameIwant
    else x div y

fun maxlist (xs,ex) =
    case xs of 
         [] => raise ex
       | x::[] => x
       | x::xs' => Int.max(x,maxlist(xs',ex))

val w = maxlist([3,4,5], AnyNameIwant)

e1 handle ex => e2  (* if e1 raise exception ex will eval e2 *)

val x = maxlist([3,4,5], AnyNameIwant)  (* 5 *)
        handle AnyNameIwant => 42

val z = maxlist([], AnyNameIwant) (* 42 *)
        handle AnyNameIwant => 42


fun fact n = 
    let fun aux(n,acc) = 
           if n=0
           then acc
           else aux(n-1,acc*n)
    in 
        aux(n,1)
    end

fun sum xs = 
    case xs of 
         [] => 0
       | x::xs' => x + sum xs'

fun sum2 xs = 
    let fun aux (xs,acc) =
        case xs of
             [] => acc
           | x::xs' => aux(xs', x+acc)
    in
        aux(xs, 0)
    end

fun rev xs =
    case xs of
         [] => []
       | x::xs' => (rev xs') @ [x]

fun rev2 xs =
    let fun aux(xs,acc) =
           case xs of
                [] => acc
              | x::xs' => aux(xs',x::acc)
    in
        aux(xs,[])
    end

