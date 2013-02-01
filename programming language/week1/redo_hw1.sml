fun is_older (x : int * int * int, y : int * int * int) = 
    if #1 x < #1 y orelse (#1 x = #1 y andalso (#2 x < #2 y orelse #3 x < #3 y))
    then true
    else false

fun number_in_month (x : (int * int * int) list, y : int) = 
    if null x
    then 0
    else if #2 (hd x) <> y
    then 0 + number_in_month((tl x), y)
    else 1 + number_in_month((tl x), y)

fun number_in_months (x : (int * int * int) list, y : int list) = 
    if y = []
    then 0
    else number_in_month(x, (hd y)) + number_in_months(x, (tl y))

fun dates_in_month (x : (int * int * int) list, y : int) =
    if null x
    then []
    else if #2 (hd x) = y
    then (hd x) :: dates_in_month((tl x), y)
    else dates_in_month((tl x), y)

fun dates_in_months (x : (int * int * int) list, y : int list) =
    if null y
    then []
    else dates_in_month(x, (hd y)) @ dates_in_months(x, (tl y))

fun get_nth (x : string list, y : int) = 
    if y = 1
    then hd x
    else get_nth((tl x), y-1)

val months = ["January", "February", "March", "April","May", "June", "July",
"August", "September", "October", "November", "December"];    

fun date_to_string (x : int * int * int) = 
    (* year month date -> month date year *)
    get_nth(months, (#2 x)) ^ " "
           ^ Int.toString(#3 x) ^ ", "
           ^ Int.toString(#1 x)

fun number_before_reaching_sum (x : int, y : int list) = 
    if hd y >= x orelse tl y = []
    then 0
    else 1 + number_before_reaching_sum(x-(hd y), (tl y))

val daysinmonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]  
fun what_month (x : int) = 1 + number_before_reaching_sum(x, daysinmonths)

fun month_range (x : int, y : int) =
    if x > y
    then []
    else what_month(x) :: month_range(x+1, y)

(*fun oldest (x : (int * int * int) list) = 
    if null x
    then NONE
    else
        let
            fun old(x : (int * int * int) list) = 
                if tl x = []
                then hd x
                else
                    let z = hd (tl x)
                    in
                        if is_older(hd x, z)
                        then old(hd x, hd z) 
                        else old(z, hd z)
                    end
        in
            SOME(old(x))
        end *)


