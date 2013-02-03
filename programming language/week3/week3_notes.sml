
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


