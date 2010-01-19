sum_loop(sum, xs) = if |xs| > 0 then sum_loop(sum + xs.head, xs.tail) else sum
sum_loop(0, 1..10)
arr = new int[5]
sum_loop(0, arr)

