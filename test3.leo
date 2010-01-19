a = new int[10]
for i in a.range a[i] <- i+1 end

sumall(xs) = {
 s = 0
 for x in xs s <- s + x end
 s
}

show(xs) = for x in xs print(x) end

print(sumall(a))

seed = 123

random(z) = {
 seed <- (seed * 6597 + 3241) % 65536
 seed
}

encode(xs) = do
 seed <- 13
 for i in xs i <- i ^ random(0) end
end

encode(a)
show(a)
encode(a)
show(a)
