x = 5
sum(a, b) = a + b
m = new int[10]
m[0..9] <- 0
for i in 0..9
 m[i] <- i + 1
end
iter(f, seq) =  for i in seq
  f(i)
 end
fold(f, v0, seq) = {
 t = v0
 for x in seq
  t <- f(t, x)
 end
 t
}
iter((x) =>  print(x), m)
print(fold(sum, 0, m))
add = (x) => (y) => x + y
print(add(2, 3))
five = add(2, 3)
print(add(five, five))
map(f, m) = {
 r = new int[|m|]
 for i in m
  r[i] <- f(m[i])
 end
 r
}
adder = (y) => 5 + y
a = map(adder, m)
fib(n) = if n < 2 then 1 else fib(n - 1) + fib(n - 2)
print(fib(five))

x = 5
m = new int[10]
m[0..9] <- 0
for i in 0..9
 m[i] <- i + 1
end
   for i in m
            print(i)
  end
print( {
  t_3 = 0
  for x in m
   t_3 <-     t_3 + x
  end
  t_3
 })
print( 2 + 3)
five =  2 + 3
print( five + five)
a =  {
  r_8 = new int[|m|]
  for i in m
   r_8[i] <- {
    y_9 = m[i]
    5 + y_9
   }
  end
  r_8
 }
fib(n) = if n < 2 then 1 else fib(n - 1) + fib(n - 2)
print(fib(five))
