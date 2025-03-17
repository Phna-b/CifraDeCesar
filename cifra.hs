fact1(0) = 1
fact1(n) = n * fact1(n-1)

fact2(n) = if n == 0 then 1 else n * fact2(n-1)

fact3(n)
| (n) == 0 = 1
| otherwise = n * fact3(n-1)


fib(n)
| n == 0 = 0
| n == 1 = 1
| otherwise = fib(n-1) + fib(n-2)

par(n) = (mod n 2) == 0