flags = new byte[10000000]
maxnum = |flags| - 1
for x in flags x <- 1 end

n = 0
for i in 2..maxnum
  if flags[i] > 0 then {
    loop(m, j, delta) = if j < |m| then { m[j] <- 0; loop(m, j+delta, delta) }
    loop(flags, i, i)
    n <- n + 1
    if n = 10001 then print(i)
  }
end
print(n)


sumall(xs) = {
 s = 0
 for x in xs s <- s + x end
 s
}

#print( sumall( flags ))

