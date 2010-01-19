p(x) = print(x)
iter(f, xs) = for x in xs f(x) end
m = new byte[10]
for i in 0 .. |m|-1 m[i] <- i+1 end

map(f, xs) = do
 a = new int [|xs|]
 for e in a, x in xs
  e <- f(x)
 end
 a
end

iter(p, m)
iter(\x->print(x), 100..110)
b = map(\x -> x + 10, m)

show(xs) = iter(p, xs)
show(b)

nat = map(\x->x, 1..10)
show(nat)

range = 3..7
b[range] <- m[range]

show(b)

sumall(x) = {
 sum_loop(xs, sum) = 
  if |xs| > 0 then sum_loop(xs.tail, sum + xs.head) else sum
 sum_loop(x, 0)            
}

print(sumall(m))
