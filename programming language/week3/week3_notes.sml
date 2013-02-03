
fun double x = 2 * x
fun incr x = x + 1
val a_tuple = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9


fun increment_n_time (n, x) =
    if n = 0
    then x
    else 1 + increment_n_time(n-1, x)

fun double_n_time (n, x) =
    if n=0
    then x
    else 2 * double_n_time (n-1, x)

fun nth_tail_lame (n, xs) = 
    if n=0
    then xs
    else tl (nth_tail_lame (n-1, xs))

fun n_times (f, n, x) =
    if n=0
    then x
    else f (n_times(f, n-1, x))

fun tims_until_zero (f, x) = 
    if x=0 then 0 else 1 + tims_until_zero(f, f x)

fun len xs = 
    case xs of 
         [] => 0
       | _::xs' => 1 + len xs'



fun triple_n_times (n, x) = 
    let 
        fun triple x = x * 3 
    in 
        n_times (triple, n, x)
    end

fun triple_n_times2 (n, x) = 
    n_times (let fun triple x = x * 3 in triple end, n, x)

fun triple_n_times3 (n, x) = 
    n_times (fn x => x * 3, n, x)

fun triple x = x * 3

val triple = fn y => y * 3

fun nth_tail (n, xs) = n_times (tl, n, xs)

(* if x then true else false  should be fn x => f x *)

val rev = List.rev
(* not val rev = fn xs => List.rev xs *)

List.map
fun map (f, n) = 
    case n of 
         [] => []
       | x::xs' => (f x) :: map(f, xs')

fun filter (f, xs) = 
    case xs of
         [] => []
       | x::xs' => let val n = filter (f, xs') 
                   in 
                       if f x then x :: n else n
                   end

fun is_even v = 
    v mod 2 = 0

fun all_even xs = filter (is_even, xs)

fun all_even_snd xs = filter ((fn (_,v) => is_even v), xs)

fun double_or_triple f = 
    if f 7
    then fn x => 2*x
    else fn x => 3*x

val nine = (double_or_triple (fn x=> x - 3 = 4)) 3

val x = 1

fun f y = 
    let val x = y + 1
    in 
        fn z => x + y + z
    end

val x = 3 (* irrelevant *)
val g = f 4 (* add 9 *)
val y = 5  
val z = g 6 (* add 9 *)

fun f g = 
    let val x = 3 in g 2 end

val x = 4

fun h y = x + y

val z = f h (* 6 *)

fun filter (f, xs) = 
    case xs of 
         [] => []
       | x::xs' => if f xs then xs::filter(f, xs') else filter(f, xs')

fun greaterThanX x = fn y => y > x

fun noNegatives xs = filter(greaterThanX ~1, xs)

fun allGreater (xs, n) = filter(fn x => x > n, xs)

fun allshorterThan1 (xs, s) = 
    filter (fn x => String.size x < String.size s, xs)

fun allshorterThan2 (xs, s) = 
    let val i = String.size s
    in
        filter (fn x => String.size x < i, xs)
    end

fun fold (f, acc, xs) =
    case xs of 
         [] => acc
       | x::xs' => fold(f, f(acc, x), xs')

fun f1 xs = fold ((fn (x, y) => x + y), 0, xs)

fun f2 xs = fold ((fn (x, y) => x andalso y >= 0), true, xs)

fun f3 (xs, lo, hi) = 
    fold ((fn, (x, y) => x + (if y>=lo andalso y<=hi then 1 else 0), 0, xs))

fun f4 (xs, s) = 
    let val i = String.size s
    in 
        fold((fn (x,y) => x andalso String.size y < 1), true, xs)
    end

fun f5 (g, xs) = fold ((fn(x, y) => x andalso g y), true, xs)

fun f4agina (xs, s) = 
    let val i = String.size s 
    in 
        f5(fn y => String.size y < i, xs)
    end

fun compose(f, g) => fn x => f(g, x)

fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))

fun sqrt_of_abs2 i = (Math.sqrt o Real.fromInt o abs) i 

fun sqrt_of_abs3 = Math.sqrt o Real.fromInt o abs

(* |> *)
fun backup (f, g) = fn x => case f x of
                                 NONE => g x
                               | SOME y => y

fun backup2 (f, g) = fn x => f x handle _ => g x

fun sorted_tuple (x, y, z) = z >= y andalso y>= x

val t1 = sorted_tuple (7, 9, 13)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >=x

fun sorted3_nicer x y z = z >= y andalso y >= x

val t2 = ((sorted3 7) 9) 11
 (* same as sorted3 7 9 11 *)

fun fold2 f acc xs = 
    case xs of 
         [] => acc
       | x::xs' => fold2 f (f(acc, x)) xs'

fun sum xs = fold2 (fn(x, y) => x+y) 0 xs
    
val is_nonnegative = sorted3 0 0 (* will take a funtion tell if it > 0 *)

val sum = fold (fn(x, y) => x + y) 0

fun range i j = if i > j then [] else i :: range (i+1) j

(* if cal range 3 6 will get back [3 4 5 6] *)
val countup = range 1
(* [1 2 3 4 5 ...i] *)

fun exists predicate xs =
    case xs of 
         [] => []
       | x::xs' => predicate x orelse exists predicate xs'

val no = exists (fn x =>  x=7) [4, 11, 23]
val hasZero = exists (fn x => x=0)

val incrementAll = List.map (fn x => x + 1)

val removeZeros = List.filter (fn x => x <> 0)

fun pair_with_one = List.map (fn x => (x, 1))

fun range (i, j) = if i > j then [] else i :: range(i+1, j)

fun curry f x y => f(x,y)

fun countup = curry range 1

fun uncurry f(x, y) => f x y

fun other_curry f x y = f y x 

val x = ref 42
val y = ref 42
val z = x
val _ = x := 43
val w = (!y) + (!z)  (* 85 *)

val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f::(!cbs)

fun onEvent i = 
    let fun loop fs = 
            case fs of
                 [] => ()
               | f::fs' => (f i; loop fs')
    in loop (!cbs) end

val timesPressed = ref 0

val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1 

fun printIfPressed i =
    onKeyEvent (fn j => 
        if i=j
        then print ("you pressed " ^ Int.toString i)
        else ())

datatype set = S of { insert : int -> set,
                      member : int -> bool,
                      size   : unit -> int }

val empty_set : set

val empty_set =
    let 
        fun make_set xs = 
        let 
            fun contains i = List.exists (fn j => i=j) xs
        in 
            S {insert = fn i => if contains i
                                then make_set xs
                                else make_set (i::xs);
               member = contains,
               size = fn () => length xs
               }
        end
    in
        make_set []
    end

fun use_sets () = 
    let val S s1 = empty_set
        val S s2 = (#insert s1) 34
        val S s3 = (#insert s2) 34
        val S s4 = #insert s3 19
    in
        if (#member s4) 42
        then 99
        else if (#member s4) 19
        then 17 + (#size s3) ()
        else 0
    end 


fun counts (xs, n : int) = length (filter (fn x => x=n), xs
