let rec gcd (x:int) (y:int) :int = 
    if y=0 then x
    else gcd y (x%y)

printfn "%A" (gcd 100000 327613)