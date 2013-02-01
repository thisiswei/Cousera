val myniece = {name = 'Amelia', id = 41113}

datatype mytype = TwoInts of int * int | Str of string | Pizza 

fun f x = 
    case x of
         Pizza => 3
       | Str s => 8
       | TwoInts(i,i2) => i + i2

fun fact n = if n=0 then 1 else n*fact(n-1)

fun fact n =
    let fun aux(n,acc) = 
        if n=0
        then acc
        else aux(n-1, acc*n)
    in 
        aux(n,1)
    end

datatype wtf = TwoInts of int * int | Str of string | Pizza

datatype suit = Club | Spade | Heart | Diamond
datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
            | Name of string 
                     * (string option)
                     * string

datatype exp = Constant   of int
             | Negate     of exp
             | Add        of exp * exp
             | Multiply   of exp * exp

fun eval e = 
    case e of 
         Constant => i
       | Negate(e2)     => ~(eval e2)
       | Add(e1, e2)     => eval e1 + eval e2
       | Multiply(e1, e2) => eval e1 * eval e2


fun max_constant x = 
    case x of
         Constant i => i
       | Negate   e2 => max_constant e2
       | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
       | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2) 

val test_exp  =  Add (Constant 19, Negate (Constant 4))
val nineteen  = max_constant test_exp

type aname = t

type card = suit * rank

fun is_queen_of_spade (x : card) = 
    #1 x = Spade andalso #2 x = Queen

fun is_queen_of_spade2 x =
    case x of
         (Spade, Queen) => true
       | _ => false

datatype my_int_list = Empty
                     | Con of int * my_int_list
val x = Con(4, Con(23, Con(2008, Empty)))

fun append_my_list (xs, ys) = 
    case xs of
         Empty => ys
       | Con(x,xs') => Con(x, append_my_list(xs', ys))

fun inc_or_zero intoption =
    case intoption of
         NONE => 0
       | SOME i => i + 1

fun append (xs,ys) = 
    case xs of
         [] => ys
       | x::xs' => x :: append(xs', ys)

fun sum_list xs = 
    case xs of
         [] => 0
       | x::xs' => x + sum_list xs'

(* bad *)

fun sum_triple triple = 
    case triple of
         (x, y, z) => x + y + z
fun full_name r = 
    case r of
         {first=x, middle=y, last=z} => x ^ " " ^ y " " ^ z
(* *)

fun sum_triple2 triple = 
    let val (x, y, z) = triple
    in 
        x + y + z
    end

(* val pattern = expression *)


(* function is also pattern matching *)

fun full_name2 r = 
    let val (first=x, middle=y, last=z) = r
    in 
        x ^ " " + y ^ " " ^ z
    end

fun sum_triple3 (x, y, z) =
    x + y + z

fun full_name3 {first=x, middle=y, last=z} = 
    x ^ " " ^ y ^ " " ^ z

fun zip list_triple = 
    case list_triple of 
         (x::xs', y::ys', z::zs') => x::y::z, zip(xs', ys', zs')
       | ([], [], []) => []
       | _ => raise ListLengthMismatch 

fun unzip lst = 
    case lst of
         [] => ([], [], [])
       | (a,b,c)::tl => let val (l1, l2, l3) = unzip tl
                        in
                            (a::l1, b:l2, c:l3)
                        end

fun nondecreasing xs =
    case xs of 
         [] => true
       | _::[] => true
       | head::(neck:rest) => head <= neck andalso nondecreasing(neck:rest)

datatype sgn = P | N | Z

fun mulsign(x1, x2) = 
    let fun sign x = if x=0 then Z else if x>0 then P else N
    in
        case (sign x1, sign x2) of 
             (Z, _) => Z
           | (_, Z) => Z
           | (P, P) => P
           | (N, N) => P
           | _ => N
    end

fun len xs =
    case xs of
         [] => 0
       | _:xs' => 1 + len xs'

fun hd x = 
    case x of
         [] => raise List.Empty
       | x::_ => x

exception Myexception
exception Myotherexception of int * int

fun mydiv (x, y) = 
    if y = 0
    then raise Myexception
    else x div y

fun maxlist (xs, ex) =
    case xs of
         [] => raise ex
       | x::[] => x
       | x::xs' => Int.max(x, maxlist(xs', ex))

e1 handle ex => e2

val x = maxlist ([3,4,5], Myexception) handle Myexception => 42
val x = maxlist ([], Myexception) handle Myexception => 42 (* 42 *)

fun sum xs = 
    case xs of
         [] => 0
       | x::xs' => x + sum xs'

fun sum2 xs =
    let fun aux (xs, acc) = 
        case xs of 
             [] => acc
           | x::xs' => aux(xs', acc+x)
    in
        aux(xs, 0)
    end

fun rev xs =
    case xs of
         [] => []
       | x::xs' => (rev xs') @ [x]
                   (* we can't use rev xs :: x since we ML dont allow :: to be
                   * use after of lists*)
fun rev2 xs = 
    let revv (xs, acc) =
        case xs of
             [] => acc
           | x::xs' => revv (xs', x::acc)
    in
        revv xs
    end



