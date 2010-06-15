$notrash
x = 5   # comment
sum(a, b) = a + b
sum(x, 1)

#more comment

print(x, 3)
m = new int[10]
rng = 0..9
for i in rng
 m[i] <- i + 1 # some comment
end

bs = new byte[10]
bs <- m
iter(f, seq) = {  for i in seq
  f(i)
 end }

fold(f, v0, seq) = {
 t = v0
 for x in seq
  t <- f(t, x)
 end
 t
}
printer(x) =  print(x)
iter(printer, m)
iter(printer, bs)
iter(printer, 5..15)
print(fold(sum, 0, m))

sumall(x) = {
 sum_loop(xs, sum) = 
  if |xs| > 0 then sum_loop(xs.tail, sum + xs.head) else sum
 sum_loop(x, 0)            
}

for mi in m, bi in bs
 mi <- bi * 2
end
print(sumall(m))
